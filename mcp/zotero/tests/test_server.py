"""MCP stdio integration test with a deterministic in-process HTTP fixture."""
from __future__ import annotations
import json
import os
import subprocess
import sys
from pathlib import Path
from typing import Any

import pytest
from mcp import ClientSession
from mcp.client.stdio import StdioServerParameters, stdio_client

MCP_DIR = Path(__file__).parent.parent
PACKAGE_ROOT = MCP_DIR.parents[1]
FIXTURE_DIR = PACKAGE_ROOT / "tests/testthat/fixtures/zotero"

def _fixture(relative: str) -> Any:
    return json.loads((FIXTURE_DIR / relative).read_text(encoding="utf-8"))

def _make_pdf(path: Path) -> None:
    code = ("args <- commandArgs(TRUE); grDevices::pdf(args[[1L]]); "
            "graphics::plot.new(); graphics::text(.5,.5,'Synthetic MCP PDF'); "
            "grDevices::dev.off()")
    subprocess.run(["Rscript", "-e", code, str(path)], check=True, capture_output=True)

def _fixture_transport(pdf_path: Path) -> dict[str, Any]:
    article = _fixture("contracts/item.json")
    article["library"]["id"] = 123456
    collection = _fixture("local/collection.json")
    collection["library"]["id"] = 123456
    attachment = {
        "key":"ATTACH12", "version":7, "library":{"type":"user","id":123456},
        "data":{"key":"ATTACH12","version":7,"itemType":"attachment",
                "parentItem":"ABCD1234","contentType":"application/pdf",
                "linkMode":"imported_file","filename":"synthetic.pdf"},
    }
    headers = {"zotero-api-version":"3","zotero-server-id":"mcp-fixture-instance",
               "last-modified-version":"7"}
    page_headers = {**headers, "total-results":"1"}
    return {
        "__root__":{"status":200,"headers":headers,"body":{"version":"3"}},
        "users/123456/collections":{"status":200,"headers":page_headers,"body":[collection]},
        "users/123456/items/top":{"status":200,"headers":page_headers,"body":[article]},
        "users/123456/collections/COLL1234/items/top":{
            "status":200,"headers":page_headers,"body":[article]},
        "users/123456/items/ABCD1234":{"status":200,"headers":headers,"body":article},
        "users/123456/items/ABCD1234/children":{
            "status":200,"headers":page_headers,"body":[attachment]},
        "users/123456/items/ATTACH12/file/view/url":{
            "status":200,"headers":{"content-type":"text/plain"},"body":pdf_path.as_uri()},
    }

def _tool_data(result: Any) -> dict[str, Any]:
    assert not getattr(result, "isError", False)
    payload = result.model_dump(by_alias=True)
    structured = payload.get("structuredContent")
    if structured is not None:
        if set(structured) == {"result"}:
            structured = structured["result"]
        assert "error" not in structured, structured
        return structured
    blocks = [item.get("text") for item in payload.get("content", []) if item.get("text")]
    assert len(blocks) == 1
    decoded = json.loads(blocks[0])
    assert "error" not in decoded, decoded
    return decoded

@pytest.mark.asyncio
async def test_stdio_runs_synthetic_zotero_to_idempotent_pdf_import(tmp_path: Path) -> None:
    pdf = tmp_path / "synthetic.pdf"
    _make_pdf(pdf)
    corpus = tmp_path / "authorized-corpus"
    env = os.environ.copy()
    env.update({
        "LITREVIEW_ZOTERO_PROFILES_JSON": json.dumps({
            "fixture":{"backend":"local","library_type":"user","library_id":"123456",
                       "base_url":"http://127.0.0.1:23119/api/",
                       "instance_id":"synthetic-test-instance"},
        }),
        "LITREVIEW_ZOTERO_CORPORA_JSON": json.dumps({"case":str(corpus)}),
        "LITREVIEW_ZOTERO_TEST_MODE":"1",
        "LITREVIEW_ZOTERO_TEST_TRANSPORT_JSON":json.dumps(_fixture_transport(pdf)),
    })
    parameters = StdioServerParameters(command=sys.executable, args=["-m","server"],
                                       env=env, cwd=MCP_DIR)
    async with stdio_client(parameters) as (read_stream, write_stream):
        async with ClientSession(read_stream, write_stream) as session:
            await session.initialize()
            listed = await session.list_tools()
            assert {tool.name for tool in listed.tools} == {
                "zotero_status","zotero_list_collections","zotero_search_items",
                "zotero_get_item","litreview_import_collection"}

            status = _tool_data(await session.call_tool(
                "zotero_status", {"library_profile":"fixture"}))
            assert status.get("available") is True and status.get("backend") == "local", status

            collections = _tool_data(await session.call_tool(
                "zotero_list_collections", {"library_profile":"fixture","limit":50}))
            assert collections["items"][0]["key"] == "COLL1234"

            search = _tool_data(await session.call_tool(
                "zotero_search_items", {"library_profile":"fixture","query":"synthetic"}))
            assert search["items"][0]["key"] == "ABCD1234"

            item = _tool_data(await session.call_tool(
                "zotero_get_item", {"library_profile":"fixture","item_key":"ABCD1234"}))
            assert item["item"]["key"] == "ABCD1234"
            assert item["children"][0]["key"] == "ATTACH12"

            dry = _tool_data(await session.call_tool("litreview_import_collection", {
                "library_profile":"fixture","collection_key":"COLL1234","corpus_id":"case",
                "include_pdfs":True,"dry_run":True}))
            assert dry["dry_run"] is True and not corpus.exists()

            first = _tool_data(await session.call_tool("litreview_import_collection", {
                "library_profile":"fixture","collection_key":"COLL1234","corpus_id":"case",
                "include_pdfs":True,"dry_run":False}))
            assert first["counts"]["imported"] == 1
            assert first["documents"][0]["acquisition_status"] == "acquired"
            copied = Path(first["documents"][0]["path"])
            assert copied.is_file() and copied.resolve() != pdf.resolve()

            second = _tool_data(await session.call_tool("litreview_import_collection", {
                "library_profile":"fixture","collection_key":"COLL1234","corpus_id":"case",
                "include_pdfs":True,"dry_run":False}))
            assert second["counts"]["unchanged"] == 1
            assert second["documents"][0]["sha256"] == first["documents"][0]["sha256"]


@pytest.mark.asyncio
async def test_stdio_web_profile_uses_synthetic_transport_without_leaking_key() -> None:
    collection = _fixture("local/collection.json")
    collection["library"]["id"] = 123456
    api_headers = {"zotero-api-version":"3","zotero-server-id":"web-fixture-instance"}
    fixtures = {
        "users/123456/items/top": {
            "status":200,"headers":api_headers,"body":{"version":"3"},
        },
        "users/123456/collections": {
            "status":200,
            "headers":{**api_headers,"total-results":"1"},
            "body":[collection],
        },
    }
    synthetic_key = "synthetic-web-key-do-not-leak"
    env = os.environ.copy()
    env.update({
        "LITREVIEW_ZOTERO_PROFILES_JSON": json.dumps({
            "web-fixture": {
                "backend":"web","library_type":"user","library_id":"123456",
                "base_url":"https://api.zotero.org/",
                "api_key_env":"LITREVIEW_TEST_ZOTERO_API_KEY",
            },
        }),
        "LITREVIEW_ZOTERO_TEST_MODE":"1",
        "LITREVIEW_ZOTERO_TEST_TRANSPORT_JSON":json.dumps(fixtures),
        "LITREVIEW_TEST_ZOTERO_API_KEY":synthetic_key,
    })
    parameters = StdioServerParameters(command=sys.executable, args=["-m","server"],
                                       env=env, cwd=MCP_DIR)
    async with stdio_client(parameters) as (read_stream, write_stream):
        async with ClientSession(read_stream, write_stream) as session:
            await session.initialize()
            status = _tool_data(await session.call_tool(
                "zotero_status", {"library_profile":"web-fixture"}))
            collections = _tool_data(await session.call_tool(
                "zotero_list_collections", {"library_profile":"web-fixture","limit":10}))

    assert status["available"] is True and status["backend"] == "web", status
    assert collections["items"][0]["key"] == "COLL1234"
    assert synthetic_key not in json.dumps({"status":status,"collections":collections})
