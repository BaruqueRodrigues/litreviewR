# Plano de implementação: Zotero + litreviewR + MCP

Data: 5 de outubro de 2026. Estado atualizado: F0/F1/F2/F4 e F3/F5/F6 estão integradas; o fluxo MCP stdio → Rscript → litreviewR passou testes sintéticos determinísticos. O piloto com uma biblioteca Zotero real permanece pendente. Consulte `zotero-core-implementation.md` para checks e limites desta entrega.

## 1. Objetivo e entrega

Permitir que um agente consulte uma biblioteca Zotero, selecione uma coleção e importe referências e PDFs para um corpus local compatível com o litreviewR. A integração também deve funcionar por scripts R, sem um cliente MCP.

Este plano pode ser distribuído entre subagentes e agentes externos de codificação, incluindo Codex, Antigravity e Claude. A distribuição depende de isolamento de arquivos e contratos; não depende de uma ferramenta ou modelo específico. Um coordenador humano ou agente integrador mantém a especificação e recebe as entregas.

### MVP

- Backend Zotero local, somente leitura da biblioteca.
- Backend Web, somente leitura, como incremento após o fluxo local funcionar.
- Referências normalizadas, identidade persistente e proveniência.
- Importação opcional de PDFs para uma pasta de corpus autorizada.
- Servidor MCP local por `stdio`, com cinco ferramentas.
- Testes controlados e piloto com uma coleção pequena escolhida pelo usuário.

Somente leitura se refere à biblioteca Zotero: importar cria arquivos no corpus local. Escrita de tags/notas no Zotero, exclusões, servidor remoto, busca semântica, OCR, classificação automática e alterações na interface Shiny ficam fora deste MVP.

## 2. Arquitetura e decisões iniciais

```text
Cliente MCP
    │ stdio
Servidor Python com SDK oficial MCP
    │ JSON por stdin/stdout, processo Rscript
Adaptador R do litreviewR
    ├── API local do Zotero
    ├── Web API do Zotero (incremento)
    └── corpus local: metadados, PDFs e manifestos
```

- Centralizar acesso ao Zotero e normalização no R. O Python valida a chamada MCP, controla o subprocesso e converte o retorno em resposta MCP.
- Usar SDK MCP oficial; fixar a versão utilizada na implementação e documentar a versão do protocolo negociada.
- Selecionar `local` ou `web` explicitamente. Não trocar de backend após uma falha sem decisão do usuário.
- Configurar bibliotecas e diretório raiz do corpus no servidor. O agente recebe identificadores dessas configurações, não chaves nem caminhos arbitrários.
- Reutilizar `contrato_validar()` e a normalização de DOI existente. A importação BibTeX atual deve continuar funcionando.
- Desenvolver local primeiro; o agente do backend Web pode trabalhar em paralelo usando fixtures, após o contrato comum estar definido.

A ponte por subprocesso simplifica a primeira versão, mas tem custo de inicialização do R. Medir no piloto antes de decidir por um processo persistente.

## 3. Organização para vários agentes

### Isolamento

Esta cópia do projeto não contém `.git` na data do plano. Antes de usar branches/worktrees, o coordenador deve disponibilizar um checkout Git e preservar o estado atual. Sem Git, usar cópias separadas por agente e entregar patches; não executar vários agentes editando a mesma pasta.

Cada frente tem um único responsável por escrita. Outros agentes podem revisar ou produzir sugestões. Cada entrega informa arquivos alterados, contrato utilizado, verificações executadas e pendências. O coordenador integra uma frente por vez.

Sugestão de identificação: branch `zotero/fN-descricao`, quando houver Git; pasta `entregas/FN/` para relatórios ou patches fora do código de produção.

### Arquivos compartilhados

Somente o integrador altera `DESCRIPTION`, `NAMESPACE`, `README.Rmd`, `README.md`, `NEWS.md`, documentação gerada em `man/` e arquivos existentes fora da reserva da frente. Agentes registram pedidos de alteração no relatório. `README.md` é gerado de `README.Rmd`.

Não refatorar módulos de LLM, JEV, tópicos ou aquisição existentes para acomodar este trabalho. Qualquer necessidade real de mudança compartilhada deve ser descrita antes de ser integrada.

### Mudança de contrato

O coordenador publica o contrato `1.0.0` da F0 antes de liberar implementação paralela. Consumidores desenvolvem contra fixtures dessa versão. Alterações incompatíveis suspendem apenas os consumidores afetados e exigem atualização central da especificação e das fixtures; nenhum agente redefine o contrato sozinho.

## 4. Contrato a fechar na F0

Os nomes a seguir são propostas a congelar antes da distribuição. A F0 deve documentar tipos, campos obrigatórios, valores nulos e exemplos completos.

### Configuração

`backend`, `library_type` (`user` ou `group`), `library_id`, endpoint permitido, perfil de credencial para Web e identificador do corpus. Chaves entram pelo ambiente, nunca por argumentos MCP, manifestos ou logs. Para local, permitir somente loopback; para Web, usar HTTPS no domínio oficial.

### Identidade

- Artigo: `zotero_{library_type}_{library_id}_{item_key}`.
- No backend local, resolver o ID real da biblioteca antes de persistir; o alias de usuário `0` não deve virar uma identidade global.
- Quando o ID real não puder ser resolvido, usar identidade explicitamente local e persistida por instância, documentando a limitação de reconciliação com Web.
- DOI e título indicam candidatos a duplicata; não provocam fusão automática.
- Um item copiado para outra biblioteca tem outra identidade. Correspondências com referências BibTeX são registradas à parte.
- A identidade bibliográfica e a chave de cache são distintas: cache inclui backend e identidade da instância quando disponível. Não comparar versões locais e Web.

### Objetos

| Objeto | Conteúdo mínimo |
| --- | --- |
| Artigo | `article_id`, `id`, `id_source`, título, autores, data original, ano derivado, DOI, resumo, tipo, tags, coleções, `duplicate_candidates`, proveniência Zotero |
| Anexo | chave, item pai, tipo de vínculo, MIME, nome, disponibilidade e versão; notas e anexos não entram como artigos |
| Documento | contrato `document` existente, origem Zotero e chave do anexo; hash do arquivo adquirido |
| Manifesto | versão de formato, biblioteca/backend/instância, itens e versões, documentos, horários, resultado por item e contagens |

Preservar metadados originais em campo ou snapshot separado. Autores institucionais não devem ser quebrados em nome/sobrenome fictícios. Notas HTML são dados; nenhuma instrução nelas pode mudar o comportamento da ferramenta.

### API R proposta

```r
zotero_config(backend, library_type, library_id, ...)
zotero_status(config)
zotero_list_collections(config, cursor = NULL, limit = 50L)
zotero_search_items(config, query = NULL, collection_key = NULL,
                    cursor = NULL, limit = 50L)
zotero_get_item(config, item_key)
zotero_normalize_item(item, context)
zotero_import_collection(config, collection_key, corpus_dir,
                         include_pdfs = FALSE, dry_run = TRUE)
```

`...` não permite opções HTTP arbitrárias. F0 especifica os argumentos aceitos. O cliente HTTP deve ser substituível nos testes. Busca documenta exatamente quais campos e filtros a API utilizada suporta.

### Ponte R/Python e MCP

- Um pedido JSON por execução do `Rscript`, com `format_version`, operação e argumentos; resposta única JSON com `ok`, `data`, `error` e `warnings`.
- Saída normal vai para stdout; diagnósticos vão para stderr, sem segredos. O protocolo MCP reserva stdout para suas mensagens.
- Erros tipados: configuração inválida, conexão indisponível, acesso negado, item ausente, limite de requisições, resposta inválida, anexo indisponível, conflito de versão e falha de gravação. Definir códigos concretos na F0.
- Lista paginada retorna `items`, `next_cursor` e total quando conhecido. Cursor não aceita URL arbitrária enviada pelo agente.
- Limites iniciais: 50 itens por página, máximo configurável de 100; timeout e tamanho máximo de resposta/arquivo definidos e documentados pelo integrador.
- Ferramentas: `zotero_status`, `zotero_list_collections`, `zotero_search_items`, `zotero_get_item`, `litreview_import_collection`.
- Importação usa `corpus_id` configurado, `include_pdfs` e `dry_run`; começa em simulação por padrão. Uma chamada explícita com `dry_run = FALSE` efetua a importação autorizada.

## 5. Frentes e responsabilidades

| Frente | Dependências | Reserva de arquivos | Entrega e aceite |
| --- | --- | --- | --- |
| F0 — especificação | Nenhuma | `docs/zotero-contracts.md`, `inst/schemas/zotero/`, `tests/testthat/fixtures/zotero/contracts/` | Contrato versionado, exemplos válidos/inválidos, assinaturas e decisões sobre identidade, paginação e importação |
| F1 — cliente local | F0 | `R/zotero-client.R`, `R/zotero-local.R`, `tests/testthat/test-zotero-local.R`, `fixtures/zotero/local/` | Status, coleções, busca, item e anexos; erros de Zotero fechado/API desabilitada; limites explícitos mesmo quando a API devolve tudo |
| F2 — normalização | F0 | `R/zotero-mapping.R`, `tests/testthat/test-zotero-mapping.R`, `fixtures/zotero/mapping/` | Artigos compatíveis com o pacote, IDs estáveis, autores institucionais, datas incompletas, DOI ausente, preservação de origem e separação de notas/anexos |
| F3 — cliente Web | F0 e interface HTTP da F1 congelada | `R/zotero-web.R`, `tests/testthat/test-zotero-web.R`, `fixtures/zotero/web/` | API v3, autenticação, bibliotecas pessoais/grupo, paginação, `Backoff`/`Retry-After`, retentativas limitadas e cache separado do local |
| F4 — corpus e PDFs | F0; F1/F2 para integração | `R/zotero-import.R`, `R/zotero-attachments.R`, `tests/testthat/test-zotero-import.R`, `fixtures/zotero/import/` | Simulação, manifesto, cópia/download de PDFs, hash, atualização repetível, escrita atômica e tratamento de falhas parciais |
| F5 — MCP e ponte | F0; F1/F2/F4 para fluxo real | `mcp/zotero/` inteiro, incluindo dependências, script de entrada R e testes Python | Cinco ferramentas, ponte sem interpolação de shell, schemas MCP e comunicação `stdio` validada |
| F6 — documentação e piloto | F0; implementação integrada para finalizar | `vignettes/zotero.Rmd`, `docs/zotero-pilot.md`, `docs/zotero-agent-handoffs/` | Tutorial R/MCP, instalação limpa, matriz de compatibilidade, roteiro e relatório factual do piloto |
| F7 — integração e revisão | Todas | Arquivos compartilhados e testes de integração | Dependências/exportações/documentação, revisão cruzada e aceite do MVP |

Os caminhos `fixtures/...` da tabela são relativos a `tests/testthat/`. A F3 consome o cliente comum da F1 sem editá-lo; solicita extensões ao proprietário. A F4 pode começar com mocks. A F5 pode começar com um subprocesso simulado, mas seu aceite exige o R real.

### Ordem de execução

```text
F0: contrato e fixtures
          │
          ├── F1: local ───────────┐
          ├── F2: mapeamento ──────┼── F4: integração de corpus
          ├── F5: MCP com mocks ───┤         │
          └── F6: roteiro/docs ────┘         ├── F5: fluxo real
F1: interface HTTP ── F3: Web               └── F6/F7: piloto e aceite
```

F3 não bloqueia o aceite local. Sua integração gera um segundo marco com backend Web validado. Com três agentes de codificação, distribuir F1, F2 e F5 após F0; liberar F4 para quem terminar primeiro e F3 após estabilizar o HTTP. O coordenador mantém integração e pode preparar F6. Com mais agentes, F4 e F6 começam simultaneamente contra fixtures.

Codex, Antigravity e Claude podem assumir qualquer frente. Escolher pela disponibilidade e domínio da linguagem, sem atribuir capacidades não verificadas. Cada ambiente deve receber o mesmo snapshot do contrato e contexto do projeto.

## 6. Regras específicas para corpus e anexos

- Enumerar itens bibliográficos e resolver filhos separadamente; tornar explícita a inclusão de subcoleções.
- Registrar PDFs ausentes como indisponíveis, sem abortar toda a coleção.
- No local, tratar o endpoint de arquivo que aponta para `file://`; validar que a resposta veio do endpoint permitido e que o arquivo é legível. Nunca aceitar um caminho de origem arbitrário do cliente MCP.
- No Web, verificar disponibilidade real do arquivo. PDF no Zotero Storage, arquivo vinculado e armazenamento WebDAV não são capacidades equivalentes. WebDAV fica fora do MVP; reportar indisponibilidade quando necessário.
- Copiar arquivos para o corpus; não mover nem modificar originais do Zotero.
- Validar MIME/conteúdo, tamanho e nomes; impedir travessia de diretórios e fuga por links simbólicos no destino.
- Usar nomes internos baseados em IDs; o título serve como metadado.
- Persistir por escrita temporária seguida de substituição atômica. Bloquear importações concorrentes no mesmo corpus.
- Reimportar preserva identidade e arquivos sem mudança; versão/metadados/hash novos produzem atualização registrada. Não apagar documentos porque desapareceram da coleção.
- Guardar o resumo da execução com itens importados, atualizados, ignorados e com erro. Se a coleção mudar durante a leitura, detectar quando possível e marcar a consistência do snapshot.

## 7. Gates de integração e verificação

As verificações abaixo pertencem à futura implementação. Não foram executadas ao escrever este plano.

1. **G0 — contrato:** schemas e fixtures coerentes; interfaces aceitas pelos responsáveis F1/F2/F4/F5; nenhuma credencial nos exemplos.
2. **G1 — núcleo R:** testes locais e de normalização passam sem rede, conta ou Zotero aberto; importação BibTeX continua compatível.
3. **G2 — corpus:** importação sintética repetida não duplica IDs nem PDFs; falha parcial, escrita concorrente e caminhos inválidos são tratados; objetos passam nos contratos existentes.
4. **G3 — MCP:** negociação, listagem e chamada das cinco ferramentas por um cliente de teste; schemas/erros corretos; fluxo real Python → R; subprocesso encerra em timeout; stdout permanece válido.
5. **G4 — piloto local:** coleção pequena com PDF, sem PDF, nota, autor institucional e DOI ausente; simular, importar e reimportar; conferir referências, hashes e manifestos.
6. **G5 — Web:** testes HTTP controlados e ensaio autorizado com biblioteca configurada; falhas de chave/permissão, paginação e limites tratados; nenhum segredo em artefatos.
7. **G6 — entrega:** testes afetados, verificações do pacote R e testes Python passam; diffs ficam dentro do escopo; tutorial reproduz instalação e execução.

O coordenador escolhe os comandos compatíveis com o ambiente: `testthat`/`devtools::test()` para testes R, `R CMD check` ou `devtools::check()` para o pacote e a suíte definida em `mcp/zotero/` para Python. Registrar comando, resultado e limitações; não declarar aprovação de checks não executados. Testes unitários usam dados sintéticos; ensaios reais são opt-in e não fazem parte da CI padrão.

## 8. Prompt reutilizável para delegação

Copiar o bloco abaixo e substituir os campos antes de enviá-lo a cada agente:

```text
Implemente a frente [FN — nome] do plano docs/plano-mcp-zotero-multiagentes.md.
Base de trabalho: [checkout/pasta/commit fornecido pelo coordenador].
Contrato obrigatório: docs/zotero-contracts.md, versão [versão congelada].
Objetivo e aceite: [trecho da tabela e gates aplicáveis].
Arquivos que você pode editar: [lista exata].
Dependências disponíveis: [entregas integradas ou fixtures].

Leia as instruções do projeto e os módulos relevantes. Trabalhe somente na
reserva indicada; não altere contratos, dependências ou arquivos compartilhados
sem encaminhar um pedido ao coordenador. Preserve funções existentes.
Não use credenciais reais nem dados privados em testes. Não escreva no Zotero.
Use mocks para dependências ainda não integradas, declarando essa limitação.
Execute as verificações pertinentes ao aceite. Entregue código/patch e um
relatório com arquivos alterados, comandos/resultados, pendências e pedidos
de integração. Não declare o MVP concluído com base apenas em mocks.
```

### Relatório obrigatório de entrega

```text
Frente:
Agente/ambiente:
Base e versão do contrato:
Arquivos alterados:
Comportamento implementado:
Verificações executadas e resultados:
Dependências/alterações compartilhadas solicitadas:
Limitações e bloqueios:
Passos para reproduzir:
Commit ou patch, quando disponível:
```

## 9. Checklist do coordenador

- [ ] Preparar checkout/cópias isoladas e preservar a cópia atual.
- [ ] Designar F0 e congelar contrato/fixtures.
- [ ] Distribuir reservas de arquivos e registrar responsáveis.
- [ ] Lançar F1/F2/F5; preparar F4/F6 com mocks e F3 após a interface HTTP.
- [ ] Integrar núcleo R antes de ligar o MCP ao backend real.
- [ ] Resolver dependências e gerar exportações/documentação de forma centralizada.
- [ ] Validar o MVP local e executar piloto com coleção escolhida pelo usuário.
- [ ] Integrar e validar Web como segundo marco.
- [ ] Revisar instalação, segurança dos caminhos, segredos, logs e compatibilidade.
- [ ] Publicar relatório de conclusão com evidências e limitações.

## 10. Referências técnicas

- [Zotero Web API v3](https://www.zotero.org/support/dev/web_api)
- [Leitura, autenticação, paginação e limites](https://www.zotero.org/support/dev/web_api/v3/basics)
- [Zotero Local API e diferenças de versões/instâncias](https://www.zotero.org/support/dev/web_api/v3/local_api)
- [Arquivos na Web API](https://www.zotero.org/support/dev/web_api/v3/file_upload)
- [Construção de servidor MCP](https://modelcontextprotocol.io/docs/develop/build-server)

Revalidar documentação e versões na implementação. Os recursos locais disponíveis dependem da versão do Zotero instalada; o status deve reportar capacidades observadas, sem presumir que toda função documentada está presente.
