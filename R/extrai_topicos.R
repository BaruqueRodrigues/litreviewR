#' Extrai tópicos representativos de PDFs via LDA
#'
#' Lê PDFs de uma pasta, extrai o texto, aplica pré-processamento com stopwords no idioma desejado,
#' realiza modelagem de tópicos com LDA e retorna os principais termos por tópico.
#'
#' @param pasta_pdfs Caminho da pasta contendo os arquivos PDF.
#' @param k Número de tópicos a ser extraído (default = 10).
#' @param max_termos Número de termos por tópico (default = 10).
#' @param idioma Idioma para remoção de stopwords (ex: "portuguese", "english", "spanish").
#' @param modo Modo de retorno: `"agrupado"` (padrão) ou `"detalhado"` (com beta por termo).
#'
#' @return Um tibble com tópicos e termos, agrupados ou detalhados.
#' @export
extrai_topicos <- function(pasta_pdfs, k = 10, max_termos = 10, idioma = "portuguese", modo = c("agrupado", "detalhado")) {
  modo <- match.arg(modo)

  if (!requireNamespace("pdftools", quietly = TRUE)) stop("Instale o pacote 'pdftools'.")
  if (!requireNamespace("tm", quietly = TRUE)) stop("Instale o pacote 'tm'.")
  if (!requireNamespace("topicmodels", quietly = TRUE)) stop("Instale o pacote 'topicmodels'.")
  if (!requireNamespace("tidytext", quietly = TRUE)) stop("Instale o pacote 'tidytext'.")

  arquivos <- list.files(pasta_pdfs, pattern = "\\.pdf$", full.names = TRUE)

  textos <- purrr::map_chr(arquivos, ~ {
    raw <- tryCatch(pdftools::pdf_text(.x), error = function(e) NA)
    paste(raw, collapse = " ")
  })

  corpus <- tm::VCorpus(tm::VectorSource(textos))
  corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
  corpus <- tm::tm_map(corpus, tm::removePunctuation)
  corpus <- tm::tm_map(corpus, tm::removeNumbers)
  corpus <- tm::tm_map(corpus, tm::removeWords, tm::stopwords(idioma))
  corpus <- tm::tm_map(corpus, tm::stripWhitespace)

  dtm <- tm::DocumentTermMatrix(corpus)
  dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]

  modelo <- topicmodels::LDA(dtm, k = k, control = list(seed = 1234))

  termos <- tidytext::tidy(modelo, matrix = "beta") %>%
    dplyr::group_by(topic) %>%
    dplyr::slice_max(beta, n = max_termos) %>%
    dplyr::ungroup()

  if (modo == "detalhado") {
    return(dplyr::rename(termos, topico = topic))
  }

  # modo agrupado
  agrupado <- termos %>%
    dplyr::group_by(topic) %>%
    dplyr::summarise(texto = paste(term, collapse = " "), .groups = "drop") %>%
    dplyr::rename(topico = topic)

  return(agrupado)
}
