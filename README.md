
<!-- README.md is generated from README.Rmd. Please edit that file -->

# 📚 litreviewR

O **`litreviewR`** é um pacote R que automatiza o processo de **revisão
de literatura científica** e prepara suas referências e textos para
análises estruturadas com modelos de linguagem (LLMs/SLMs) e análise de
tópicos. Ideal para pesquisadores e gestores públicos que precisam
organizar, baixar e interpretar grandes volumes de literatura com
**baixo esforço manual**.

------------------------------------------------------------------------

## 🚀 O que o pacote faz

- Lê arquivos `.bib` e estrutura as referências (`gera_referencia`)

- Busca o DOI automaticamente quando ausente (`descobre_doi_por_titulo`)

- Baixa artigos **nacionais** (SciELO, ANPOCS etc) via
  `baixa_pdf_aberto()`

- Baixa artigos **internacionais** via Sci-Hub com `baixa_pdf_scihub()`

- Usa **roteamento inteligente** com `baixa_pdf_auto()` para escolher o
  melhor método de download

- Extrai **resumos** dos PDFs para avaliação manual
  (`gera_csv_resumos_para_anotacao`)

- Cria **agrupamentos temáticos** dos tópicos via anotação ou clustering
  (`agrupa_topicos`)

- Gera **modelos de tópicos** com LDA, STM ou NMF (`modela_topicos`)

- ## Roda todo o pipeline de análise com `roda_analise_topicos`

## 🚀 Instalação

Você pode instalar a versão de desenvolvimento diretamente do GitHub
com:

``` r
# Instale o devtools, se ainda não tiver
install.packages("devtools")

# Instale o litreviewR
devtools::install_github("BaruqueRodrigues/litreviewR")
```

## 🧠 Pipeline automático de download

A função `baixa_pdf_auto()` identifica automaticamente a origem do
artigo:

- **Artigos nacionais**: detectados por domínio ou `publisher` no
  CrossRef → `baixa_pdf_aberto()`
- **Artigos internacionais** ou com paywall → `baixa_pdf_scihub()`

``` r
library(litreviewR)

# Lê o .bib
referencias <- gera_referencia("minhas_referencias.bib")

# Baixa os PDFs automaticamente
baixa_pdf_auto(referencias, diretorio = "pdfs")
```

🧠 Pipeline completo de análise de tópicos

Se você já possui os PDFs, use roda_analise_topicos() para:

Ler os arquivos PDF Preprocessar os textos Rodar um modelo de tópicos
(LDA, STM, NMF) Retornar os principais termos por tópico

``` r
resultado <- roda_analise_topicos(
  pasta_pdfs = "pdfs",
  k = 10,
  method = "lda",       # ou "stm", "nmf"
  idioma = "portuguese",
  modo = "agrupado"
)

# Ver os tópicos
resultado$topicos
```

## ✨ Funções principais

### Para Pegar artigos

📚 gera_referencia()

Para quê serve? Transforma um arquivo .bib (BibTeX) em uma lista
organizada com título, DOI (se existir) e um ID amigável para nomear
arquivos.

Caso prático: “Baixei 100 referências do Mendeley e quero transformar
isso em uma lista de tarefas para baixar os artigos automaticamente.”

``` r
# Extrai as referências do .bib
refs <- gera_referencia("referencias.bib")
```

🇧🇷 baixa_pdf_aberto()

Para quê serve? Baixa o PDF de artigos nacionais de acesso aberto, como
da SciELO, ANPOCS, USP e outros repositórios acadêmicos brasileiros.

Caso prático: “Tenho vários artigos da Revista Dados e Opinião Pública,
que já são abertos. Não preciso do Sci-Hub.”

``` r
# Baixa um artigo nacional (SciELO)
baixa_pdf_aberto(refs)
```

🌍 baixa_pdf_scihub()

Para quê serve? Baixa o PDF de artigos internacionais ou com paywall
usando o Sci-Hub, a partir do DOI.

Caso prático: “Preciso do PDF de um artigo da Cambridge University
Press, mas meu instituto não tem acesso.”

``` r
baixa_pdf_scihub(refs[[10]], diretorio = "pdfs")
```

🧠 baixa_pdf_auto()

Para quê serve? Escolhe automaticamente o melhor método de download, com
base no DOI ou no título, e no país/origem do artigo.

Caso prático: “Tenho um mix de artigos nacionais e internacionais. Só
quero que o script descubra onde baixar.”

``` r
baixa_pdf_auto(refs, diretorio = "pdfs")
```

### Para Analisar artigos

📄 gera_csv_resumos_para_anotacao()

Para quê serve? Quando você tem dezenas de PDFs e quer ler só os resumos
para organizar em áreas temáticas ou correntes teóricas.

Caso prático: “Sou coordenador de pesquisa e preciso dividir 120 artigos
entre 3 assistentes. Quero que cada um anote qual área do conhecimento o
artigo pertence.”

``` r
# Gera CSV com resumos dos PDFs para anotação manual
gera_csv_resumos_para_anotacao("pdfs", caminho_saida = "anotacao.csv")
```

🧠 agrupa_topicos()

Para quê serve? Agrupa os tópicos extraídos automaticamente em
categorias maiores, usando ou um arquivo CSV manual ou agrupamento
automático com clustering.

Caso prático: “Extraí 15 tópicos de uma coleção de artigos. Agora quero
agrupá-los em 3 grandes correntes: institucionalismo, economia política
e opinião pública.”

``` r
# Agrupa tópicos manualmente ou via clustering
agrupa_topicos(topicos = resultado$topicos, metodo = "cluster", n_clusters = 4)
```

🧾 extrai_topicos()

Para quê serve? Roda um modelo LDA simples a partir de PDFs e retorna os
principais termos por tópico.

Caso prático: “Baixei os PDFs, agora quero uma ideia geral sobre os
temas que aparecem mais nos artigos.”

``` r
topicos <- extrai_topicos("pdfs", k = 8, modo = "agrupado")
```

🏗 cria_dtm() + 🧩 modela_topicos()

Para quê servem? Separadamente permitem mais controle e personalização
sobre o pipeline. Use quando quiser testar diferentes abordagens com a
mesma base textual.

Caso prático: “Quero comparar o desempenho de LDA com NMF sobre a mesma
base de documentos.”

``` r
dtm <- cria_dtm("pdfs", idioma = "portuguese")
modelo_lda <- modela_topicos(dtm, k = 10, method = "lda")
modelo_nmf <- modela_topicos(dtm, k = 10, method = "nmf")
```

``` r
# metadados deve conter colunas como 'ano_publicacao'
modelo_stm <- modela_topicos(dtm, k = 10, method = "stm", metadados = dados_artigos)
```

🤖 modela_topicos(method = “stm”) + metadados

Para quê serve? Permite usar variáveis como ano, autor, tipo de
periódico como covariáveis no modelo, com o método STM.

Caso prático: “Quero ver se os temas mudam com o tempo, comparando os
tópicos de artigos publicados antes e depois de 2010.

## 🔧 Em desenvolvimento

- Integração com modelos LLMs e SLMs (GPT, Claude, BERT etc)

  - Classificação temática automática

  - Sumarização de artigos

  - Extração de tópicos

- Visualizações com Shiny ou Quarto

- Mapeamento de co-citações e redes semânticas

## 👤 Autor

Desenvolvido por [Baruque
Rodrigues](https://github.com/baruqrodrigues)  
Coordenador de Operacoes e cientista de dados interessado automacoes,
money and politics, estatistica forense e NLP.

## 📜 Licença

MIT © 2025 — Você pode usar, modificar e redistribuir livremente com os
devidos créditos.
