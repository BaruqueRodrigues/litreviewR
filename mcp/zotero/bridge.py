"""Strict JSON bridge from the MCP process to the litreviewR R entrypoint."""
from __future__ import annotations

import asyncio
import json
import logging
import shutil
import sys
from pathlib import Path
from typing import Any, Dict, List, Optional

import jsonschema

logger = logging.getLogger("litreview.mcp.bridge")
if not logger.handlers:
    handler = logging.StreamHandler(sys.stderr)
    handler.setFormatter(logging.Formatter("[%(levelname)s] [mcp.bridge] %(message)s"))
    logger.addHandler(handler)
    logger.setLevel(logging.INFO)

ERROR_CODES = {
    "INVALID_CONFIG", "INVALID_ARGUMENT", "CONNECTION_UNAVAILABLE", "ACCESS_DENIED",
    "NOT_FOUND", "RATE_LIMITED", "INVALID_RESPONSE", "ATTACHMENT_UNAVAILABLE",
    "VERSION_CONFLICT", "WRITE_FAILED", "CORPUS_LOCKED", "LIMIT_EXCEEDED",
    "IDENTITY_UNRESOLVED", "BACKEND_UNAVAILABLE",
}


class ZoteroBridgeError(Exception):
    """Safe, contract-shaped error returned by the bridge."""

    def __init__(self, message: str, code: str = "BACKEND_UNAVAILABLE",
                 details: Optional[Dict[str, Any]] = None,
                 warnings: Optional[List[str]] = None) -> None:
        super().__init__(message)
        self.message = message
        self.code = code if code in ERROR_CODES else "INVALID_RESPONSE"
        self.details = details
        self.warnings = warnings or []

    def to_dict(self) -> Dict[str, Any]:
        return {"code": self.code, "message": self.message,
                "retryable": self.code in {"CONNECTION_UNAVAILABLE", "RATE_LIMITED"},
                "details": self.details}


class ZoteroTimeoutError(ZoteroBridgeError):
    def __init__(self, timeout: float) -> None:
        super().__init__(f"A execução do Rscript excedeu {timeout:.1f} segundos.",
                         code="CONNECTION_UNAVAILABLE", details={"timeout_seconds": timeout})


class ZoteroInvalidResponseError(ZoteroBridgeError):
    def __init__(self, reason: str) -> None:
        super().__init__(f"Resposta inválida do subprocesso R: {reason}",
                         code="INVALID_RESPONSE")


class RscriptBridge:
    FORMAT_VERSION = "1.0.0"

    def __init__(self, rscript_bin: str = "Rscript",
                 entrypoint_path: Optional[Path | str] = None,
                 schema_path: Optional[Path | str] = None,
                 envelope_schema_path: Optional[Path | str] = None,
                 default_timeout: float = 30.0) -> None:
        self.rscript_bin = rscript_bin
        self.entrypoint_path = Path(entrypoint_path) if entrypoint_path else Path(__file__).parent / "r_entrypoint.R"
        package_root = Path(__file__).resolve().parents[2]
        if schema_path:
            self.schema_path = Path(schema_path)
        else:
            source_schema = package_root / "inst/schemas/zotero/request.schema.json"
            installed_schema = package_root / "schemas/zotero/request.schema.json"
            self.schema_path = source_schema if source_schema.is_file() else installed_schema
        if envelope_schema_path:
            self.envelope_schema_path = Path(envelope_schema_path)
        else:
            source_schema = package_root / "inst/schemas/zotero/envelope.schema.json"
            installed_schema = package_root / "schemas/zotero/envelope.schema.json"
            self.envelope_schema_path = source_schema if source_schema.is_file() else installed_schema
        self.default_timeout = default_timeout

    def _resolve_rscript(self) -> str:
        found = shutil.which(self.rscript_bin)
        if found:
            return found
        raise ZoteroBridgeError("Rscript não encontrado no PATH.", code="BACKEND_UNAVAILABLE")

    def _validate_request(self, operation: str, arguments: Dict[str, Any]) -> Dict[str, Any]:
        if not self.schema_path.is_file():
            raise ZoteroBridgeError("Schema do contrato Zotero não encontrado.", code="BACKEND_UNAVAILABLE")
        try:
            schema = json.loads(self.schema_path.read_text(encoding="utf-8"))
            request = {"format_version": self.FORMAT_VERSION,
                       "operation": operation, "arguments": arguments}
            jsonschema.validate(request, schema)
            return request
        except jsonschema.ValidationError as error:
            raise ZoteroBridgeError("Pedido fora do contrato Zotero 1.0.0.",
                                    code="INVALID_ARGUMENT") from error
        except (OSError, json.JSONDecodeError, jsonschema.SchemaError) as error:
            raise ZoteroBridgeError("Não foi possível validar o contrato Zotero.",
                                    code="BACKEND_UNAVAILABLE") from error

    def _validate_envelope(self, envelope: Dict[str, Any]) -> None:
        if not self.envelope_schema_path.is_file():
            raise ZoteroInvalidResponseError("schema de resposta do contrato não encontrado.")
        try:
            schema = json.loads(self.envelope_schema_path.read_text(encoding="utf-8"))
            jsonschema.validate(envelope, schema)
        except (OSError, json.JSONDecodeError, jsonschema.SchemaError,
                jsonschema.ValidationError) as error:
            raise ZoteroInvalidResponseError("envelope não corresponde ao schema do contrato.") from error

    @staticmethod
    def _parse_envelope(text: str) -> Dict[str, Any]:
        try:
            envelope = json.loads(text)
        except json.JSONDecodeError as error:
            raise ZoteroInvalidResponseError("stdout não contém um único JSON válido.") from error
        required = {"format_version", "ok", "data", "error", "warnings"}
        if not isinstance(envelope, dict) or set(envelope) != required:
            raise ZoteroInvalidResponseError("campos do envelope não correspondem ao contrato.")
        if envelope["format_version"] != "1.0.0" or type(envelope["ok"]) is not bool:
            raise ZoteroInvalidResponseError("versão ou indicador do envelope inválido.")
        if not isinstance(envelope["warnings"], list) or not all(
                isinstance(item, str) for item in envelope["warnings"]):
            raise ZoteroInvalidResponseError("warnings deve ser uma lista de strings.")
        if envelope["ok"]:
            if not isinstance(envelope["data"], dict) or envelope["error"] is not None:
                raise ZoteroInvalidResponseError("envelope de sucesso inválido.")
        else:
            error = envelope["error"]
            if envelope["data"] is not None or not isinstance(error, dict) or set(error) != {
                    "code", "message", "retryable", "details"}:
                raise ZoteroInvalidResponseError("envelope de erro inválido.")
            if (error["code"] not in ERROR_CODES or not isinstance(error["message"], str)
                    or not error["message"] or type(error["retryable"]) is not bool
                    or (error["details"] is not None and not isinstance(error["details"], dict))):
                raise ZoteroInvalidResponseError("campos do erro não correspondem ao contrato.")
        return envelope

    async def execute(self, operation: str, arguments: Dict[str, Any],
                      timeout: Optional[float] = None) -> Dict[str, Any]:
        if not isinstance(arguments, dict):
            raise ZoteroBridgeError("arguments deve ser um objeto JSON.", code="INVALID_ARGUMENT")
        request = self._validate_request(operation, arguments)
        rscript = self._resolve_rscript()
        if not self.entrypoint_path.is_file():
            raise ZoteroBridgeError("Entry point R não encontrado.", code="BACKEND_UNAVAILABLE")
        timeout_seconds = self.default_timeout if timeout is None else timeout
        if not isinstance(timeout_seconds, (int, float)) or timeout_seconds <= 0:
            raise ZoteroBridgeError("Timeout inválido.", code="INVALID_ARGUMENT")

        process = await asyncio.create_subprocess_exec(
            rscript, "--vanilla", str(self.entrypoint_path),
            stdin=asyncio.subprocess.PIPE, stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )
        try:
            stdout, stderr = await asyncio.wait_for(
                process.communicate(json.dumps(request, ensure_ascii=False).encode("utf-8")),
                timeout=timeout_seconds,
            )
        except asyncio.TimeoutError as error:
            process.kill()
            await process.wait()
            raise ZoteroTimeoutError(float(timeout_seconds)) from error
        except asyncio.CancelledError:
            process.kill()
            await process.wait()
            raise

        if process.returncode != 0:
            raise ZoteroBridgeError("O subprocesso R encerrou com falha.",
                                    code="BACKEND_UNAVAILABLE")
        try:
            stdout_text = stdout.decode("utf-8", errors="strict").strip()
        except UnicodeDecodeError as error:
            raise ZoteroInvalidResponseError("stdout não está em UTF-8.") from error
        if not stdout_text:
            raise ZoteroInvalidResponseError("stdout está vazio.")
        envelope = self._parse_envelope(stdout_text)
        self._validate_envelope(envelope)
        if stderr:
            logger.debug("O subprocesso R emitiu diagnóstico em stderr.")
        if not envelope["ok"]:
            error = envelope["error"]
            raise ZoteroBridgeError(error["message"], error["code"],
                                    error["details"], envelope["warnings"])
        return envelope["data"]
