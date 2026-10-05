# litreviewR 0.1.0

## Integração Zotero local

- Adicionado conector R somente leitura para a API local do Zotero, com
  configuração explícita, paginação e transporte substituível para testes.
- Adicionada normalização bibliográfica com identidade persistente e proveniência.
- Adicionada importação de coleções e PDFs para corpus local, com simulação,
  manifesto, SHA-256, trava de escrita e reimportação sem duplicação.
- Adicionado backend Web v3 sobre o transporte HTTP comum, com credenciais por
  variável de ambiente e tentativas limitadas para falhas transitórias.
- Adicionado servidor MCP `stdio` e ponte JSON/Rscript com validação do contrato,
  perfis e diretórios de corpus configurados localmente.
- Publicado contrato 1.0.0 e fluxo MCP sintético de consulta, simulação,
  cópia de PDF e reimportação idempotente.

## Documentação e divulgação

- Reescrito o README com instalação, primeiros passos, casos de uso para
  ciências sociais e políticas, integração por agentes e limites metodológicos.
- Adicionadas quatro vignettes cobrindo as funções exportadas por etapa de
  trabalho.
- Incluídos metadados de URL e relatos de problemas no GitHub e instrução de
  citação do pacote.
- Atualizada a descrição do pacote para refletir a API implementada e os
  limites dos backends de aquisição e modelagem.
