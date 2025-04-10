#' Cria uma DocumentTermMatrix a partir de PDFs brutos
#'
#' Lê os PDFs de uma pasta, aplica pré-processamento (limpeza, stopwords, etc.)
#' e retorna uma DocumentTermMatrix pronta para modelagem de tópicos.
#'
#' @param pasta_pdfs Caminho da pasta contendo os arquivos PDF.
#' @param idioma Idioma para remoção de stopwords. Ex: "portuguese", "english".
#' @param min_freq Frequência mínima para manter um termo. Default = 2.
#'
#' @return Uma DocumentTermMatrix.
#' @export
cria_dtm <- function(pasta_pdfs, idioma = "portuguese", min_freq = 2) {

  arquivos <- list.files(pasta_pdfs, pattern = "\\.pdf$", full.names = TRUE)

  textos <- purrr::map_chr(arquivos, ~ {
    texto <- tryCatch(pdftools::pdf_text(.x), error = function(e) NA)
    paste(texto, collapse = " ")
  })

  corpus <- tm::VCorpus(tm::VectorSource(textos))
  corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
  corpus <- tm::tm_map(corpus, tm::removePunctuation)
  corpus <- tm::tm_map(corpus, tm::removeNumbers)
  corpus <- tm::tm_map(corpus, tm::removeWords, tm::stopwords(idioma))
  corpus <- tm::tm_map(corpus, tm::stripWhitespace)

  dtm <- tm::DocumentTermMatrix(corpus)

  termos_freq <- slam::col_sums(dtm)
  termos_mantidos <- termos_freq >= min_freq
  dtm <- dtm[, termos_mantidos]

  dtm <- dtm[slam::row_sums(dtm) > 0, ]

  return(dtm)
}
