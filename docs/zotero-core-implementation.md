# Integração Zotero — entrega e verificação

Contrato 1.0.0. Estado atualizado em 2026-10-05.

O núcleo R (F0, F1, F2, F4 e integração F7), o backend Web (F3), a ponte e o
servidor MCP (F5), e a documentação operacional (F6) estão integrados nesta
cópia. O teste automatizado exercita o caminho MCP stdio → subprocesso Rscript
→ cliente litreviewR, com transporte HTTP substituído por fixtures sintéticas.
O piloto com uma biblioteca Zotero real continua pendente.

## Comportamento

- Cliente Zotero somente leitura via HTTP GET, com paginação, limites de
  resposta/arquivo, erros tipados, credencial Web obtida pelo nome de variável
  configurado e retentativas limitadas para erros transitórios.
- Identidade canônica por biblioteca e chave Zotero; fallback por instância
  local persistente; metadados normalizados preservam origem e criadores
  institucionais.
- Importação por coleção com simulação padrão, manifesto e repetição
  idempotente. BibTeX continua suportado pela importação existente.
- PDFs locais copiados e identificados por SHA-256, com originais intactos;
  anexos indisponíveis são registrados.
- MCP expõe cinco ferramentas por stdio. Perfis de biblioteca e raízes de
  corpus são allowlists do operador no ambiente do servidor; argumentos MCP
  não recebem credenciais nem caminhos arbitrários.
- Ponte verifica os envelopes e schemas do contrato 1.0.0, mantém stdout
  reservado ao protocolo e invoca Rscript sem shell. O entrypoint chama as
  funções R reais; não há fallback que simule sucesso.

## Verificações executadas

- `devtools::document(roclets = c("rd", "namespace"))`: concluído.
- `devtools::test(reporter = "summary", stop_on_failure = TRUE)`: 624
  expectativas passaram; 0 falhas e 0 erros.
- Suíte Python/MCP: 10 testes passaram, incluindo negociação stdio, perfis
  local e Web, importação de PDF e reimportação idempotente com Rscript real.
- O teste MCP usa somente ambiente temporário e fixtures determinísticas; não
  exige Zotero Desktop, conta ou acesso de rede.
- `devtools::check(document=FALSE, vignettes=FALSE, cran=FALSE, remote=FALSE)`:
  0 erros, 1 aviso e 0 notas. O único aviso lista código R preexistente com
  caracteres não ASCII.
- A tentativa com `vignettes=TRUE` parou na construção porque Pandoc não está
  instalado neste ambiente; nenhuma vignette chegou a ser renderizada.
- Não foi realizado piloto com Zotero real. A validação ao vivo de acesso
  local/Web, diferenças de versões e anexos continua pendente.

## Operação e limites

Consulte [contrato e fixtures](zotero-contracts.md), a
[vignette](../vignettes/zotero.Rmd) e o [roteiro do piloto](zotero-pilot.md).
Para iniciar MCP, use um checkout do repositório: a instalação do pacote R não
instala o servidor Python na biblioteca R. Instale as dependências de
`mcp/zotero/requirements.txt`, configure `LITREVIEW_ZOTERO_PROFILES_JSON` e
`LITREVIEW_ZOTERO_CORPORA_JSON` no ambiente local do servidor e execute
`python mcp/zotero/server.py` a partir da raiz do checkout. Defina a chave Web
somente na variável cujo nome aparece em `api_key_env`. Não configure
`LITREVIEW_ZOTERO_TEST_MODE` fora dos testes.

Nenhuma credencial ou dado de biblioteca real foi incluído. O entrypoint de
teste sintético existe somente para testes locais e depende de variáveis de
ambiente explícitas.
