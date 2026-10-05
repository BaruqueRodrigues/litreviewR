"""Contract and real R subprocess tests for the MCP/R bridge."""
from __future__ import annotations
import json
from pathlib import Path
import pytest
from bridge import RscriptBridge, ZoteroBridgeError, ZoteroInvalidResponseError, ZoteroTimeoutError

ENTRYPOINT_PATH = Path(__file__).parent.parent / "r_entrypoint.R"

@pytest.fixture
def bridge() -> RscriptBridge:
    return RscriptBridge(entrypoint_path=ENTRYPOINT_PATH, default_timeout=20.0)

def test_contract_schema_rejects_backend_arguments_and_missing_profiles(bridge):
    with pytest.raises(ZoteroBridgeError) as error:
        bridge._validate_request("zotero_status", {"backend": "local"})
    assert error.value.code == "INVALID_ARGUMENT"
    with pytest.raises(ZoteroBridgeError) as error:
        bridge._validate_request("zotero_get_item", {"library_profile": "main"})
    assert error.value.code == "INVALID_ARGUMENT"

def test_contract_envelope_is_strict():
    success = {"format_version":"1.0.0","ok":True,"data":{"available":True},"error":None,"warnings":[]}
    assert RscriptBridge._parse_envelope(json.dumps(success)) == success
    for bad in (
        {"ok":True,"data":{},"error":None,"warnings":[]},
        {**success,"error":{}},
        {**success,"warnings":""},
    ):
        with pytest.raises(ZoteroInvalidResponseError):
            RscriptBridge._parse_envelope(json.dumps(bad))

def test_bridge_checks_envelope_against_frozen_schema(bridge, tmp_path):
    invalid_schema = tmp_path / "envelope.schema.json"
    invalid_schema.write_text('{"type":"object","required":["contract_marker"]}', encoding="utf-8")
    bridge.envelope_schema_path = invalid_schema

    with pytest.raises(ZoteroInvalidResponseError):
        bridge._validate_envelope({
            "format_version":"1.0.0", "ok":True, "data":{}, "error":None, "warnings":[]
        })

def test_bridge_finds_schema_in_installed_package_layout(tmp_path, monkeypatch):
    import bridge as bridge_module
    package_root = tmp_path / "library" / "litreviewR"
    bridge_dir = package_root / "mcp" / "zotero"
    bridge_dir.mkdir(parents=True)
    installed_schema = package_root / "schemas" / "zotero" / "request.schema.json"
    installed_schema.parent.mkdir(parents=True)
    installed_schema.write_text("{}", encoding="utf-8")
    installed_envelope = package_root / "schemas" / "zotero" / "envelope.schema.json"
    installed_envelope.write_text("{}", encoding="utf-8")
    monkeypatch.setattr(bridge_module, "__file__", str(bridge_dir / "bridge.py"))

    resolved = RscriptBridge()

    assert resolved.schema_path == installed_schema
    assert resolved.envelope_schema_path == installed_envelope

@pytest.mark.asyncio
async def test_r_entrypoint_returns_typed_error_instead_of_synthetic_success(bridge, monkeypatch):
    monkeypatch.setenv("LITREVIEW_ZOTERO_PROFILES_JSON", json.dumps({
        "web-no-key":{"backend":"web","library_type":"user","library_id":"123456",
                      "api_key_env":"LITREVIEW_TEST_MISSING_KEY"}
    }))
    monkeypatch.delenv("LITREVIEW_TEST_MISSING_KEY", raising=False)
    with pytest.raises(ZoteroBridgeError) as error:
        await bridge.execute("zotero_status", {"library_profile":"web-no-key"})
    assert error.value.code == "INVALID_CONFIG"
    assert "connected" not in error.value.message.lower()

@pytest.mark.asyncio
async def test_profile_and_corpus_ids_are_allowlisted_before_subprocess(monkeypatch):
    import server
    monkeypatch.setenv("LITREVIEW_ZOTERO_PROFILES_JSON", "{}")
    monkeypatch.setenv("LITREVIEW_ZOTERO_CORPORA_JSON", "{}")
    class NoCallBridge:
        called = False
        async def execute(self, operation, arguments):
            self.called = True
            return {}
    stub = NoCallBridge()
    monkeypatch.setattr(server, "_bridge", stub)
    result = await server.zotero_status(library_profile="not-configured")
    assert result["error"]["code"] == "INVALID_CONFIG"
    assert stub.called is False
    result = await server.litreview_import_collection(
        library_profile="also-not-configured", collection_key="COLL1234", corpus_id="arbitrary-path")
    assert result["error"]["code"] == "INVALID_CONFIG"
    assert stub.called is False

@pytest.mark.asyncio
async def test_bridge_rejects_malformed_or_unknown_contract_fields(bridge):
    with pytest.raises(ZoteroBridgeError) as error:
        await bridge.execute("arbitrary_operation", {"library_profile":"main"})
    assert error.value.code == "INVALID_ARGUMENT"
    with pytest.raises(ZoteroBridgeError) as error:
        await bridge.execute("zotero_status", {"library_profile":"main","base_url":"http://localhost"})
    assert error.value.code == "INVALID_ARGUMENT"

@pytest.mark.asyncio
async def test_bridge_timeout_terminates_subprocess(tmp_path):
    sleep_r = tmp_path / "sleep.R"
    sleep_r.write_text("Sys.sleep(10)\ncat('{}\\n')\n", encoding="utf-8")
    schema = Path(__file__).parents[3] / "inst/schemas/zotero/request.schema.json"
    slow = RscriptBridge(entrypoint_path=sleep_r, schema_path=schema, default_timeout=0.2)
    with pytest.raises(ZoteroTimeoutError):
        await slow.execute("zotero_status", {"library_profile":"main"})
