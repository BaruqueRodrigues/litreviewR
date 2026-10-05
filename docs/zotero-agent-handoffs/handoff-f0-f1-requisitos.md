# Estado de integração F0–F7

Data: 5 de outubro de 2026. Contrato consumido: Zotero 1.0.0.

O núcleo R F0/F1/F2/F4 e as frentes Web F3, MCP F5 e documentação F6 estão
integrados no checkout principal. A interface Web implementa
.zotero_web_request(config, path, query, expect_json) sobre
.zotero_http_request, sem alterar a API HTTP comum. A ponte valida schemas e
chama as funções R públicas reais por subprocesso.

Perfis e diretórios de corpus são allowlists no ambiente do processo MCP:
LITREVIEW_ZOTERO_PROFILES_JSON e LITREVIEW_ZOTERO_CORPORA_JSON. Os valores
dos perfis contêm nomes de variáveis de credencial, nunca os segredos. O cliente
MCP envia apenas identificadores permitidos. Não passe URL ou caminho arbitrário
em chamadas de ferramenta.

A suíte inclui um ensaio MCP stdio completo com transporte HTTP sintético:
status, coleções, busca, item e filhos, simulação, importação de PDF e repetição
idempotente. O ambiente LITREVIEW_ZOTERO_TEST_MODE=1 e
LITREVIEW_ZOTERO_TEST_TRANSPORT_JSON pertence exclusivamente aos testes; não
defina essas variáveis ao iniciar o servidor para uso normal.

Pendências: piloto com biblioteca real escolhida pelo usuário e aceitação de
compatibilidade com as versões locais do Zotero que forem suportadas.
