# Contrato Zotero/litreviewR 1.0.0

Estado: congelado para implementação F1/F2/F4 e consumo F3/F5. Data: 2026-10-05.
Toda mudança incompatível exige nova versão e decisão do integrador.

## API pública R

```r
zotero_config(backend = "local", library_type = "user", library_id = "0",
  base_url = NULL, api_key_env = "ZOTERO_API_KEY", instance_id = NULL,
  timeout = 30, max_response_bytes = 10485760, max_file_bytes = 104857600,
  max_pages = 1000L, .transport = NULL)
zotero_status(config)
zotero_list_collections(config, cursor = NULL, limit = 50L)
zotero_search_items(config, query = NULL, collection_key = NULL,
  cursor = NULL, limit = 50L)
zotero_get_item(config, item_key)
zotero_normalize_item(item, context)
zotero_import_collection(config, collection_key, corpus_dir,
  include_pdfs = FALSE, dry_run = TRUE)
```

Configuração é objeto de classe `zotero_config`. IDs de biblioteca são strings
decimais; `0` só é permitido para usuário local. Chaves de itens e coleções
são oito caracteres maiúsculos alfanuméricos. Limite de página: inteiro 1–100.
Cursor: string decimal de offset (`"50"`); rejeitar URLs, negativos e frações.
Coleções não incluem subcoleções automaticamente. Busca usa `q` (quicksearch
Zotero) e consulta somente itens de topo, filtrando notas/anexos dos artigos.
Sem promessa de busca de texto integral. Requisições usam limit/start explícitos.

Base local permitida: `http://localhost:23119/api/`,
`http://127.0.0.1:23119/api/` ou `http://[::1]:23119/api/`.
Base Web: somente `https://api.zotero.org/`. Não aceitar usuário/senha,
query/fragmentos ou outras portas. Nenhuma operação escreve no Zotero.
`api_key_env` é nome de variável; a configuração não guarda o valor secreto.
`instance_id` é identificador persistente fornecido pelo usuário quando Zotero
antigo não oferece server ID e não se conhece o ID real da biblioteca.
Temporários, manifesto e logs nunca guardam credenciais.

## Interface comum HTTP (F1; F3 consome sem editar)

```r
.zotero_abort(code, message, retryable = FALSE, details = NULL)
.zotero_request(config, path, query = list(), expect_json = TRUE)
.zotero_context(config, item = NULL)
.zotero_library_path(config)
.zotero_validate_key(key, field = "item_key")
```

`.zotero_abort` lança condição com classes `zotero_error`, `error`, `condition`
e campos `code`, `message`, `retryable`, `details`. Nunca embutir corpo HTTP,
chave, headers de autenticação ou mensagens brutas do transporte em erros.

`.zotero_request` devolve `list(status, headers, body)`. Headers têm nomes
minúsculos; JSON é decodificado com `simplifyVector = FALSE`; resposta textual
é string UTF-8. Paths são relativos, construídos internamente, sem `..`, URL,
query ou fragmento. Método fixo GET. F1 aplica timeout, limite de bytes,
sem redirects automáticos, valida JSON e status. O retorno padrão não imprime.
Para backend Web, F1 despacha para `.zotero_web_request(config, path, query,
expect_json)` quando essa função existir. Ausente: `BACKEND_UNAVAILABLE`.
F3 implementa essa função no arquivo reservado `R/zotero-web.R`, reutilizando
`.zotero_http_request` e os erros comuns; não adiciona exports nem muda F1.

`.zotero_http_request(config, path, query = list(), expect_json = TRUE,
headers = list())` executa o GET usando o transporte e aplica validações comuns.
Headers adicionais são exclusivamente internos; F3 fornece a chave Web em
header nessa camada. Nenhum parâmetro público aceita headers arbitrários.

Transporte injetável `.transport(request)` recebe lista com `method = "GET"`,
`url` final sem query, `query` lista, `headers` lista, `timeout`,
`max_response_bytes`. Retorna `list(status = inteiro, headers = lista nomeada,
body = raw ou string)`. Proibir objeto R já decodificado como body.
O transporte padrão respeita limite durante download, desabilita redirects e
usa user-agent próprio. Testes substituem o transporte sem rede.

Estado observado é guardado em ambiente interno `config$state`, separado da
configuração serializável. Cache nunca é compartilhado entre backend/instância.
`.zotero_context` usa library do item quando disponível, configurações e o
server ID observado. Não faz chamadas extras. F1 deve observar headers e
rejeitar mudança de server ID dentro da mesma configuração com
`VERSION_CONFLICT`; para outra instância, criar configuração nova.

## Retornos de leitura

`zotero_status`: lista com `available = TRUE`, `backend`, `api_version`,
`server_id` (string ou NULL), `context`, `capabilities` (read e local_files).
Em falha lança zotero_error; a ponte transforma em erro estruturado.
No local faz GET da raiz da API; somente versão 3 é suportada. Capacidade de
anexos é condicional à resposta do endpoint, não garantia de arquivo existente.

Listagens: `list(items = list(...), next_cursor = string ou NULL,
total = inteiro ou NULL, context = contexto, library_version = inteiro ou NULL)`.
Itens são wrappers Zotero originais. Coleções conservam hierarquia. Total vem
de `Total-Results`. Próximo offset é calculado com total/quantidade retornada;
ignorar URLs Link para navegação. Se total não existe e página está cheia,
permitir mais uma consulta. Não aceitar mais itens que limit em uma resposta.

`zotero_get_item`: `list(item = wrapper original, children = list(wrappers),
context = contexto)`. Filhos são obtidos com paginação limitada a max_pages.
Não consultar filhos de nota/anexo. Limites estourados geram `LIMIT_EXCEEDED`.

## Contexto, identidade e mapeamento (F2)

Contexto: `backend`, `library_type`, `library_id` (ID real ou NULL),
`server_id` (string ou NULL), `instance_id` (string ou NULL).
Ao resolver usuário local `0`, só usar library.id positivo do wrapper original
(com tipo compatível). Caso não resolvido, precisar de server_id ou instance_id.
Não criar um ID global usando o alias 0. Sem identidade suficiente: erro
`IDENTITY_UNRESOLVED`. ID real: `zotero_user_123_ABCD1234` ou equivalente group.
Fallback: `zotero_local_<instance>_user_ABCD1234`; <instance> deve ser SHA-256
hexadecimal do server_id/instance_id (via digest) para evitar colisões por
sanitização. Fallback não tem reconciliação Web automática.

`zotero_normalize_item` recebe wrapper `{key, version, library, data, ...}`.
Retorna NULL para `attachment`/`note`; demais tipos retornam lista compatível
com contrato article existente. Campos:
`article_id`, `id` (igual), `id_source = "zotero"`, `source_key`, `title`
(string ou NULL), `author` (lista de creators preservada), `date_original`,
`year` (string de quatro dígitos ou NULL), `doi` normalizado ou NULL,
`journal`, `abstract`, `url`, `entry_type`, `tags` (lista), `collections`
(lista), `duplicate_candidates` (vetor character), `provenance` e `raw`.
`provenance` contém backend, biblioteca resolvida, chave, versão,
server_id/instance_id; raw preserva o wrapper sem normalização.
Datas sem ano inequívoco ficam NULL; não inventar autor/DOI/ano.
Duplicatas por DOI/título são sinalizadas na importação, nunca fundidas.

## Anexos e importação (F4)

F4 implementa `.zotero_attachment_source(config, attachment_key)` chamando
`.zotero_request` no path construído
`users|groups/<id>/items/<key>/file/view/url`, `expect_json = FALSE`, para local.
Aceitar apenas `file:///...` ou `file://localhost/...` retornados pelo endpoint,
sem query/fragmento, decodificar percent-encoding e validar arquivo legível.
Destino nunca pode apontar para o original. Web não deve ser implementado
pela F4 nesta entrega: estado `not_available`, salvo hook futuro contratado
com F3. Arquivos locais vinculados também podem ser resolvidos pela API.

Importação opera exclusivamente na coleção indicada, percorre páginas, obtém
filhos por item bibliográfico e normaliza. Metadata-only não consulta filhos
nem arquivos. Dry-run não cria diretório, lock, temporário nem manifesto;
pode ler biblioteca e manifesto existente. Retorna sempre o manifesto planejado.
PDFs ausentes/falhos não abortam outros artigos. Acesso/configuração e coleta
incompleta de páginas são erros globais: não publicar manifesto parcial como
snapshot completo. Detectar library_version diferente entre páginas quando
headers disponíveis; versão não observada: consistência `unknown`.

Layout: `corpus_dir/manifest.json`, `corpus_dir/pdfs/<document_id>.pdf` e
lock transitório `corpus_dir/.zotero-import.lock/`. Document ID deriva da
identidade de artigo e chave de anexo. Não usar título/nome remoto no destino.
Copiar, validar PDF real com pdftools e calcular SHA-256 com digest;
`acquisition_status = acquired|not_available|failed` e
`extraction_status = pending` conforme contrato existente. Documento sem
arquivo usa path de destino planejado e sha256 = NULL.

Validar root/destinos contra symlinks e traversal. Rejeitar manifesto/lock
incompatível ou corpora de outra biblioteca/backend/instância com
`VERSION_CONFLICT`. Trava adquirida atomicamente por dir.create; só remover
trava adquirida pela própria execução. Escrita por tempfile na mesma pasta e
rename; manifesto é commit final. Para PDFs alterados usar nome versionado por
hash para não invalidar manifesto anterior em falha. Reexecução compara
conteúdo de metadados e hash, não apenas version. Não remover itens antigos
nem arquivos que sumiram da coleção. Guardar entradas antigas como retidas;
atualizar apenas entradas da importação atual.

Manifesto: `format_version = "1.0.0"`, `backend`, `context`, `collection_key`,
`dry_run`, `started_at`, `finished_at`, `consistency` (`checked|unknown`),
`articles` lista, `documents` lista, `results` lista,
`counts = {imported, updated, unchanged, failed, not_available}`,
`warnings` lista. Cada result: `article_id`, `status` (imported/updated/unchanged),
`attachment_results` lista. Contagens imported/updated/unchanged referem-se a
artigos; failed/not_available referem-se a anexos. Limites de tamanho são
aplicados antes da leitura/cópia. Reimportar sem mudanças preserva IDs e PDFs.

## Erros e envelope da ponte (F5 implementa)

Códigos: `INVALID_CONFIG`, `INVALID_ARGUMENT`, `CONNECTION_UNAVAILABLE`,
`ACCESS_DENIED`, `NOT_FOUND`, `RATE_LIMITED`, `INVALID_RESPONSE`,
`ATTACHMENT_UNAVAILABLE`, `VERSION_CONFLICT`, `WRITE_FAILED`, `CORPUS_LOCKED`,
`LIMIT_EXCEEDED`, `IDENTITY_UNRESOLVED`, `BACKEND_UNAVAILABLE`.
Retryable só para falha transitória: conexão, limites de requisição e 5xx.
Erros HTTP 5xx usam CONNECTION_UNAVAILABLE; 401/403 ACCESS_DENIED;
404 NOT_FOUND; 412/428 VERSION_CONFLICT; 429 RATE_LIMITED;
outros 4xx INVALID_RESPONSE. Nenhum erro revela credenciais/corpo remoto.

Pedido JSON: `{format_version, operation, arguments}`. Operation é uma das
cinco ferramentas do plano; configuration/library/corpus são resolvidos por
IDs de perfis locais da F5, nunca valores secretos enviados pelo modelo.
Envelope sucesso: `{format_version:"1.0.0", ok:true, data:objeto,
error:null, warnings:[]}`. Falha: `ok:false,data:null,error:{code,message,
retryable,details:null},warnings:[]`. Um único JSON na stdout por Rscript;
diagnósticos na stderr. Arrays vazios devem permanecer [] após JSON.
Schema do envelope e fixtures estão nos diretórios F0.
F5 valida inputs com allowlist de campos/operações e não deixa o pedido chamar
funções R arbitrárias. Não serializar config$state nem funções de transporte.

## Pendências externas

Backend Web, MCP, ponte executável, tutorial e piloto pertencem a F3/F5/F6.
Roda-se MVP local com mocks antes de ensaio real. Não afirmar validação Web/MCP
sem entrega externa e checks da integração. Não configurar escrita Zotero.

## Fontes e capacidades

- https://www.zotero.org/support/dev/web_api/v3/local_api
- https://www.zotero.org/support/dev/web_api/v3/basics

A API local requer habilitação nas preferências. Recursos observados devem ser
reportados conforme versão instalada; versões locais/Web não são comparáveis.

## Esclarecimentos compatíveis da integração 1.0.0

- `source_key` normalizado é `zotero:<item_key>`; o ID global continua em
  article_id e a chave original em provenance.key. `entry_type` conserva
  data.itemType do Zotero, sem conversão para tipo BibTeX.
- `document.path` é caminho absoluto utilizável pelos consumidores do corpus.
  `relative_path` é campo adicional interno `pdfs/...`, validado contra a raiz.
  Ao ler o manifesto, reconstruir path a partir da raiz atual e relative_path.
- Os totais/cursors da API contam objetos brutos. A lista pública de artigos
  pode ser menor após filtrar notas/anexos; o cursor válido é a referência para
  continuar a paginação, inclusive quando items está vazio.

- Em HTTP 429/5xx, `error$details$retry_after_seconds` conserva uma dica numérica
  válida de Retry-After quando disponível. Headers/corpo brutos não entram no
  erro. O cliente comum não espera nem repete sozinho; F3 controla orçamento e
  retentativas e pode ler Backoff nos headers de respostas bem-sucedidas.
