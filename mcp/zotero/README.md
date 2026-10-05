# Servidor MCP local para Zotero e litreviewR

O servidor expõe cinco ferramentas MCP de leitura e importação local. O cliente
Zotero e o processamento do corpus ficam no R; Python valida o contrato 1.0.0,
controla o subprocesso `Rscript` e publica as ferramentas por `stdio`.

## Requisitos

- Clone ou checkout do repositório `litreviewR`. A instalação do pacote R não
  instala os arquivos Python na biblioteca R.
- R com o pacote `jsonlite`; `Rscript` deve estar no `PATH`.
- Python 3.10 ou superior.

## Configuração

Crie ambiente virtual e instale as dependências fixadas:

~~~sh
python -m venv mcp/zotero/.venv
mcp/zotero/.venv/bin/pip install -r mcp/zotero/requirements.txt
~~~

Configure no processo que inicia o servidor perfis de biblioteca e raízes de
corpus autorizadas. `api_key_env` guarda somente o nome de uma variável; a
credencial Web fica nessa variável do ambiente.

~~~sh
export LITREVIEW_ZOTERO_PROFILES_JSON='{"desktop":{"backend":"local","library_type":"user","library_id":"0","instance_id":"desktop-pessoal"},"web":{"backend":"web","library_type":"user","library_id":"123456","api_key_env":"ZOTERO_API_KEY"}}'
export LITREVIEW_ZOTERO_CORPORA_JSON='{"revisao":"/caminho/absoluto/corpus-revisao"}'
# Configure ZOTERO_API_KEY por um gerenciador de segredos quando o perfil Web for usado.
~~~

Inicie a partir da raiz do checkout:

~~~sh
mcp/zotero/.venv/bin/python mcp/zotero/server.py
~~~

O protocolo MCP usa `stdio`; logs e diagnósticos vão para `stderr`. As
ferramentas recebem IDs de perfis/corpora, não URLs, caminhos arbitrários ou
credenciais. `litreview_import_collection` começa com `dry_run=true`.

## Testes

~~~sh
cd mcp/zotero
.venv/bin/python -m pytest -q
# Só o teste MCP → Web → Rscript:
.venv/bin/python -m pytest -q tests/test_server.py::test_stdio_web_profile_uses_synthetic_transport_without_leaking_key
~~~

A suíte usa fixtures sintéticas, uma chave fictícia, um PDF temporário e o
subprocesso R real. Ela não precisa de conta Zotero, Zotero Desktop aberto ou
rede. Os testes atravessam os perfis local e Web; o fluxo local exercita status,
coleções, busca, item, simulação, importação e reimportação idempotente.

Não defina `LITREVIEW_ZOTERO_TEST_MODE` nem
`LITREVIEW_ZOTERO_TEST_TRANSPORT_JSON` no servidor normal; essas variáveis
existem apenas para o transporte fixture dos testes.
