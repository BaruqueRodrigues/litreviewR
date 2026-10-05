"""MCP server for the read-only Zotero/litreviewR integration."""
from __future__ import annotations

import json
import logging
import os
import re
import sys
from pathlib import Path
from typing import Annotated, Any, Dict, Optional

from mcp.server.mcpserver import MCPServer
from pydantic import Field

try:
    from bridge import RscriptBridge, ZoteroBridgeError
except ImportError:
    from .bridge import RscriptBridge, ZoteroBridgeError

logger = logging.getLogger("litreview.mcp.server")
if not logger.handlers:
    handler = logging.StreamHandler(sys.stderr)
    handler.setFormatter(logging.Formatter("[%(levelname)s] [mcp.server] %(message)s"))
    logger.addHandler(handler)
    logger.setLevel(logging.INFO)

server = MCPServer(name="litreview-zotero", version="1.0.0",
                   description="Consulta Zotero e importa referências em corpus local autorizado.")
_bridge = RscriptBridge(entrypoint_path=Path(__file__).parent / "r_entrypoint.R")
_ID_PATTERN = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$")
_PROFILE_FIELDS = {"backend", "library_type", "library_id", "base_url", "api_key_env",
                   "instance_id", "timeout", "max_response_bytes", "max_file_bytes", "max_pages"}


def _json_map(name: str) -> Dict[str, Any]:
    raw = os.environ.get(name, "")
    try:
        value = json.loads(raw) if raw else {}
    except json.JSONDecodeError as error:
        raise ZoteroBridgeError(f"Configuração local inválida em {name}.",
                                code="INVALID_CONFIG") from error
    if not isinstance(value, dict):
        raise ZoteroBridgeError(f"Configuração local inválida em {name}.",
                                code="INVALID_CONFIG")
    return value


def _profile(profile_id: str) -> Dict[str, Any]:
    if not isinstance(profile_id, str) or not _ID_PATTERN.fullmatch(profile_id):
        raise ZoteroBridgeError("Identificador de perfil inválido.", code="INVALID_ARGUMENT")
    profile = _json_map("LITREVIEW_ZOTERO_PROFILES_JSON").get(profile_id)
    if not isinstance(profile, dict) or set(profile) - _PROFILE_FIELDS:
        raise ZoteroBridgeError("Perfil Zotero não está configurado no servidor.",
                                code="INVALID_CONFIG")
    if any(key.lower() in {"api_key", "credential", "token", "secret"} for key in profile):
        raise ZoteroBridgeError("Perfis guardam nomes de variáveis, nunca credenciais.",
                                code="INVALID_CONFIG")
    backend = profile.get("backend")
    library_type = profile.get("library_type")
    library_id = profile.get("library_id")
    if backend not in {"local", "web"} or library_type not in {"user", "group"}:
        raise ZoteroBridgeError("Perfil Zotero contém backend ou biblioteca inválidos.",
                                code="INVALID_CONFIG")
    if not isinstance(library_id, str) or not re.fullmatch(r"[0-9]+", library_id):
        raise ZoteroBridgeError("Perfil Zotero contém ID de biblioteca inválido.",
                                code="INVALID_CONFIG")
    if library_id == "0" and (backend != "local" or library_type != "user"):
        raise ZoteroBridgeError("Alias de biblioteca 0 só vale para usuário local.",
                                code="INVALID_CONFIG")
    api_key_env = profile.get("api_key_env", "ZOTERO_API_KEY")
    if not isinstance(api_key_env, str) or not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", api_key_env):
        raise ZoteroBridgeError("Perfil Zotero contém nome de variável inválido.",
                                code="INVALID_CONFIG")
    for name in ("timeout", "max_response_bytes", "max_file_bytes", "max_pages"):
        value = profile.get(name)
        if value is not None and (type(value) not in {int, float} or value <= 0):
            raise ZoteroBridgeError("Perfil Zotero contém limite inválido.",
                                    code="INVALID_CONFIG")
    for name in ("base_url", "instance_id"):
        value = profile.get(name)
        if value is not None and (not isinstance(value, str) or not value):
            raise ZoteroBridgeError("Perfil Zotero contém opção inválida.",
                                    code="INVALID_CONFIG")
    return profile


def _corpus(corpus_id: str) -> str:
    if not isinstance(corpus_id, str) or not _ID_PATTERN.fullmatch(corpus_id):
        raise ZoteroBridgeError("Identificador de corpus inválido.", code="INVALID_ARGUMENT")
    root = _json_map("LITREVIEW_ZOTERO_CORPORA_JSON").get(corpus_id)
    if not isinstance(root, str) or not os.path.isabs(root):
        raise ZoteroBridgeError("Corpus não está configurado como caminho absoluto autorizado.",
                                code="INVALID_CONFIG")
    return root


async def _invoke(operation: str, arguments: Dict[str, Any]) -> Dict[str, Any]:
    _profile(arguments.get("library_profile"))
    if operation == "litreview_import_collection":
        _corpus(arguments.get("corpus_id"))
    return await _bridge.execute(operation, arguments)


def _handle_error(error: ZoteroBridgeError) -> Dict[str, Any]:
    logger.warning("Operação Zotero recusada ou falhou: %s", error.code)
    return {"error": error.to_dict()}


@server.tool(name="zotero_status", description="Verifica conectividade do perfil Zotero configurado.")
async def zotero_status(
    library_profile: Annotated[str, Field(pattern=r"^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$")],
) -> Dict[str, Any]:
    try:
        return await _invoke("zotero_status", {"library_profile": library_profile})
    except ZoteroBridgeError as error:
        return _handle_error(error)


@server.tool(name="zotero_list_collections", description="Lista coleções com paginação por cursor.")
async def zotero_list_collections(
    library_profile: Annotated[str, Field(pattern=r"^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$")],
    cursor: Optional[str] = None,
    limit: Annotated[int, Field(ge=1, le=100)] = 50,
) -> Dict[str, Any]:
    try:
        return await _invoke("zotero_list_collections", {
            "library_profile": library_profile, "cursor": cursor, "limit": limit,
        })
    except ZoteroBridgeError as error:
        return _handle_error(error)


@server.tool(name="zotero_search_items", description="Busca itens bibliográficos de topo por texto e coleção.")
async def zotero_search_items(
    library_profile: Annotated[str, Field(pattern=r"^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$")],
    query: Optional[str] = None,
    collection_key: Optional[Annotated[str, Field(pattern=r"^[A-Z0-9]{8}$")]] = None,
    cursor: Optional[Annotated[str, Field(pattern=r"^[0-9]+$")]] = None,
    limit: Annotated[int, Field(ge=1, le=100)] = 50,
) -> Dict[str, Any]:
    try:
        return await _invoke("zotero_search_items", {
            "library_profile": library_profile, "query": query,
            "collection_key": collection_key, "cursor": cursor, "limit": limit,
        })
    except ZoteroBridgeError as error:
        return _handle_error(error)


@server.tool(name="zotero_get_item", description="Obtém metadados e anexos-filhos de um item Zotero.")
async def zotero_get_item(
    library_profile: Annotated[str, Field(pattern=r"^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$")],
    item_key: Annotated[str, Field(pattern=r"^[A-Z0-9]{8}$")],
) -> Dict[str, Any]:
    try:
        return await _invoke("zotero_get_item", {
            "library_profile": library_profile, "item_key": item_key,
        })
    except ZoteroBridgeError as error:
        return _handle_error(error)


@server.tool(name="litreview_import_collection", description="Simula ou importa uma coleção para um corpus pré-autorizado.")
async def litreview_import_collection(
    library_profile: Annotated[str, Field(pattern=r"^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$")],
    collection_key: Annotated[str, Field(pattern=r"^[A-Z0-9]{8}$")],
    corpus_id: Annotated[str, Field(pattern=r"^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$")],
    include_pdfs: bool = False,
    dry_run: bool = True,
) -> Dict[str, Any]:
    try:
        return await _invoke("litreview_import_collection", {
            "library_profile": library_profile, "collection_key": collection_key,
            "corpus_id": corpus_id, "include_pdfs": include_pdfs, "dry_run": dry_run,
        })
    except ZoteroBridgeError as error:
        return _handle_error(error)


def main() -> None:
    server.run(transport="stdio")


if __name__ == "__main__":
    main()
