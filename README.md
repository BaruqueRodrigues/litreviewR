
<!-- README.md is generated from README.Rmd. Please edit that file -->

<details>
  <summary>📚 Sumário 📚</summary>

  - [Instalação](#-instalação-)  
  - [O que o pacote faz?](#-o-que-o-pacote-faz-)  
  - [Pipeline inteligente de download](#-pipeline-inteligente-de-download-)  
  - [Exemplos das Funções principais](#exemplos-das-funções-principais)  
  - [Em desenvolvimento](#-em-desenvolvimento-)  
  - [Autor](#-autor)  
  - [Licença](#-licença)  

</details>

---

#  📚 litreviewR 📚

O **`litreviewR`** é um pacote R que automatiza o processo de
**revisão de literatura científica** e prepara suas referências para
análises com modelos de linguagem (LLMs/SLMs). Ele ajuda pesquisadores a
transformar arquivos `.bib` em pipelines reprodutíveis de coleta,
download e análise textual de artigos acadêmicos!

---

## 🚀 Instalação 🚀

Você pode instalar a versão de desenvolvimento diretamente do GitHub
com:

``` r
# Instale o devtools, se ainda não tiver
install.packages("devtools")

# Instale o litreviewR
devtools::install_github("baruquerodrigues/litreviewR")
```

---

## 🤔 O que o pacote faz? 🤔

- `gera_referencia()`: Lê arquivos `.bib` e estrutura as referências ☑️

- `descobre_doi_por_titulo()`: Busca o DOI automaticamente quando ausente ☑️

- `baixa_pdf_aberto()`: Baixa artigos nacionais (**SciELO, USP, ANPOCS**) ☑️

- `baixa_pdf_scihub()`: Baixa artigos internacionais via **Sci-Hub** ☑️

- `baixa_pdf_auto()`: Usa roteamento inteligente para decidir o
método ideal ☑️

### 🧠 Pipeline inteligente de download 🧠

A função `baixa_pdf_auto()` identifica *automaticamente* a origem do
artigo!

- **Artigos nacionais**: detectados por domínio ou `publisher` no
  *CrossRef* ➡ `baixa_pdf_aberto()`

- **Artigos internacionais** ou com *paywall* ➡ `baixa_pdf_scihub()`

### Exemplos das Funções principais

- Gerando referências e baixando PDFs

``` r
library(litreviewR)

# Lê o .bib
referencias <- gera_referencia("minhas_referencias.bib")

# Baixa os PDFs automaticamente
baixa_pdf_auto(referencias, diretorio = "pdfs")
```

- Gerando referências e baixando conforme origem do artigo

``` r
# Extrai as referências do .bib
refs <- gera_referencia("referencias.bib")

# Baixa um artigo nacional (SciELO)
baixa_pdf_aberto(refs)

# Baixa um artigo via Sci-Hub
baixa_pdf_scihub(refs)

# Usa o pipeline automático inteligente
baixa_pdf_auto(refs)
```

---

## 🔧 Em desenvolvimento 🔧

- Integração com modelos LLMs e SLMs (GPT, Claude, BERT etc)

  - Classificação temática automática

  - Sumarização de artigos

  - Extração de tópicos

- Visualizações com Shiny ou Quarto

- Mapeamento de co-citações e redes semânticas

---

## 👤 Autor

Desenvolvido por [Baruque Rodrigues](https://github.com/baruqrodrigues).

Coordenador de Operacoes e cientista de dados interessado automações,
money and politics, estatística forense e NLP.

---

## 📜 Licença

MIT © 2025 — Você pode usar, modificar e redistribuir livremente com os
devidos créditos.

---
