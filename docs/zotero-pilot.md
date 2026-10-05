# Roteiro de piloto: Zotero e litreviewR

- Versão do contrato: 1.0.0
- Estado: pendente para biblioteca Zotero real
- Teste sintético: concluído no fluxo cliente MCP → stdio → Rscript → litreviewR

O teste sintético não substitui o piloto com uma biblioteca escolhida pelo
operador. Nenhuma conta, credencial, Zotero Desktop ou rede é necessária para a
suíte automatizada.

## Casos a validar no piloto

| Caso | Cobertura | Aceite |
|---|---|---|
| Item com PDF local | Importar e reimportar a coleção | PDF copiado, hash SHA-256 no manifesto e original preservado |
| Item sem PDF | Importar metadados | Artigo permanece importado e ausência do anexo fica registrada |
| Nota com HTML | Coleção com nota/anotação | Nota não aparece como artigo nem altera metadados |
| Autor institucional | Artigo com criador corporativo | Nome institucional preservado como um criador |
| Item sem DOI | Metadados sem DOI | Identidade Zotero e proveniência preservadas |
| Simulação | dry_run = TRUE | Nenhum diretório, manifesto ou PDF criado |
| Reimportação | Repetir a mesma coleção | Sem duplicatas e documentos inalterados marcados como unchanged |

## Execução

Escolha uma biblioteca e uma coleção pequenas que cubram esses casos. Configure
no servidor apenas perfis autorizados em LITREVIEW_ZOTERO_PROFILES_JSON e raízes
de corpus autorizadas em LITREVIEW_ZOTERO_CORPORA_JSON. Para Web, disponibilize
a chave apenas na variável de ambiente cujo nome consta em api_key_env. Não
grave a chave nos parâmetros da ferramenta nem no relatório.

Comece por status e listagem de coleções. Confira a chave da coleção e rode
primeiro a importação com dry_run = TRUE. Confirme que o diretório não mudou.
Com autorização do operador, execute dry_run = FALSE, confira manifest.json e
PDFs, registre hashes e repita a chamada para verificar idempotência. Faça a
mesma leitura da biblioteca somente; não altere itens no Zotero.

## Registro do piloto real

~~~text
Data:
Operador:
Backend (local ou web):
ID de biblioteca (sem segredo):
Nome e chave da coleção:
Número de itens:
Casos cobertos:
Resultado da simulação:
Resultado da importação:
Hashes e status de reimportação:
Versões de Zotero, R, litreviewR e cliente MCP:
Falhas ou limitações:
~~~
