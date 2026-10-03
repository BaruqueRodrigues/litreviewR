#' Cria uma DocumentTermMatrix a partir de PDFs ou textos
#'
#' Processa textos ou PDFs de uma pasta, aplica pré-processamento (minúsculas,
#' pontuação, números, stopwords no idioma desejado e frequência mínima de termos)
#' e retorna uma DocumentTermMatrix pronta para modelagem, preservando identificadores
#' de documentos e registrando o mapa de exclusões de documentos vazios.
#'
#' @param pasta_pdfs Caminho da pasta contendo arquivos PDF (opcional se `textos` for informado).
#' @param textos Vetor nomeado de textos (opcional, alternativo a `pasta_pdfs`). Se nomeado,
#'   os nomes são preservados como `doc_id`.
#' @param doc_ids Vetor opcional de IDs de documentos para alinhar com `textos`.
#' @param idioma Idioma para remoção de stopwords (ex: "portuguese", "english"). Default = "portuguese".
#' @param min_freq Frequência mínima para manter um termo. Default = 2.
#'
#' @return Um objeto `DocumentTermMatrix` com atributo `exclusoes` (tabela de documentos excluídos).
#' @export
cria_dtm <- function(pasta_pdfs = NULL,
                     textos = NULL,
                     doc_ids = NULL,
                     idioma = "portuguese",
                     min_freq = 2) {

  if (!is.numeric(min_freq) || length(min_freq) != 1L || is.na(min_freq) ||
      min_freq < 1 || min_freq %% 1 != 0) {
    stop("`min_freq` deve ser um inteiro positivo.", call. = FALSE)
  }
  if (!is.character(idioma) || length(idioma) != 1L || is.na(idioma) || !nzchar(idioma)) {
    stop("`idioma` deve ser uma string não vazia.", call. = FALSE)
  }
  motivos <- NULL

  if (!is.null(pasta_pdfs) && is.character(pasta_pdfs) && length(pasta_pdfs) == 1L) {
    arquivos <- list.files(pasta_pdfs, pattern = "\\.pdf$", full.names = TRUE)
    if (length(arquivos) == 0L) {
      stop("Nenhum arquivo PDF encontrado na pasta fornecida: ", pasta_pdfs, call. = FALSE)
    }

    if (is.null(doc_ids)) {
      doc_ids <- basename(tools::file_path_sans_ext(arquivos))
    } else if (length(doc_ids) != length(arquivos)) {
      stop("O número de `doc_ids` deve corresponder ao número de PDFs.", call. = FALSE)
    }

    extraidos <- purrr::map(arquivos, function(arq) tryCatch(pdftools::pdf_text(arq), error = function(e) e))
    motivos <- vapply(extraidos, function(x) if (inherits(x, "error")) "erro_leitura_pdf" else "texto_vazio", character(1))
    textos <- vapply(extraidos, function(x) if (inherits(x, "error")) NA_character_ else paste(x, collapse = " "), character(1))
  }

  if (is.null(textos) || !is.character(textos)) {
    stop("Forneça `pasta_pdfs` válida ou um vetor textual em `textos`.", call. = FALSE)
  }

  if (length(textos) == 0L) {
    stop("O corpus de textos está vazio.", call. = FALSE)
  }

  if (is.null(doc_ids)) {
    doc_ids <- names(textos) %||% paste0("doc_", seq_along(textos))
  }
  if (!is.character(doc_ids) || length(doc_ids) != length(textos) || anyNA(doc_ids) || any(!nzchar(trimws(doc_ids)))) {
    stop("`doc_ids` deve conter um ID textual não vazio para cada texto.", call. = FALSE)
  }
  if (anyDuplicated(doc_ids)) {
    stop("`doc_ids` duplicados não são permitidos; resolva colisões antes de criar a DTM.", call. = FALSE)
  }

  # Registra exclusões iniciais (textos NA ou vazios antes do processamento)
  exclusoes_lista <- list()
  validos_idx <- which(!is.na(textos) & nzchar(trimws(textos)))

  if (length(validos_idx) < length(textos)) {
    invalidos_idx <- setdiff(seq_along(textos), validos_idx)
    exclusoes_lista[[length(exclusoes_lista) + 1L]] <- tibble::tibble(
      doc_id = doc_ids[invalidos_idx],
      motivo = if (is.null(motivos)) "texto_vazio_ou_ilegivel" else motivos[invalidos_idx]
    )
  }

  if (length(validos_idx) == 0L) {
    exclusoes_df <- do.call(rbind, exclusoes_lista)
    .dtm_stop_empty(exclusoes_df)
  }

  textos_validos <- textos[validos_idx]
  ids_validos <- doc_ids[validos_idx]

  # Construção do corpus e pré-processamento via tm
  corpus <- tm::VCorpus(tm::VectorSource(textos_validos))
  corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
  corpus <- tm::tm_map(corpus, tm::removePunctuation)
  corpus <- tm::tm_map(corpus, tm::removeNumbers)
  corpus <- tm::tm_map(corpus, tm::removeWords, tm::stopwords(idioma))
  corpus <- tm::tm_map(corpus, tm::stripWhitespace)

  dtm <- tm::DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf)))
  dtm$dimnames$Docs <- ids_validos

  # Filtragem de frequência mínima
  if (ncol(dtm) > 0L && min_freq > 1L) {
    termos_freq <- slam::col_sums(dtm)
    termos_mantidos <- termos_freq >= min_freq
    dtm <- dtm[, termos_mantidos]
  }

  # Linhas com soma zero após limpeza
  somas_linhas <- slam::row_sums(dtm)
  linhas_positivas <- which(somas_linhas > 0)

  if (length(linhas_positivas) < nrow(dtm)) {
    linhas_zeradas <- which(somas_linhas == 0)
    exclusoes_lista[[length(exclusoes_lista) + 1L]] <- tibble::tibble(
      doc_id = ids_validos[linhas_zeradas],
      motivo = "vazio_apos_limpeza"
    )
    dtm <- dtm[linhas_positivas, ]
  }

  if (nrow(dtm) == 0L || ncol(dtm) == 0L) {
    exclusoes_df <- if (length(exclusoes_lista)) do.call(rbind, exclusoes_lista) else
      tibble::tibble(doc_id = ids_validos, motivo = "vazio_apos_limpeza")
    .dtm_stop_empty(exclusoes_df)
  }

  exclusoes_df <- if (length(exclusoes_lista) > 0L) {
    do.call(rbind, exclusoes_lista)
  } else {
    tibble::tibble(doc_id = character(), motivo = character())
  }

  attr(dtm, "exclusoes") <- exclusoes_df
  class(dtm) <- c("litreview_dtm", class(dtm))

  dtm
}

.dtm_stop_empty <- function(exclusoes) {
  condition <- structure(
    list(message = "Nenhum texto utilizável no corpus após checagem inicial e limpeza.",
         call = NULL, exclusoes = exclusoes),
    class = c("litreview_empty_corpus", "error", "condition")
  )
  stop(condition)
}

#' Obter tabela de exclusões de uma DTM
#'
#' @param dtm DocumentTermMatrix gerada por `cria_dtm`.
#' @return Tibble contendo `doc_id` e `motivo` da exclusão.
#' @export
dtm_obter_exclusoes <- function(dtm) {
  attr(dtm, "exclusoes") %||% tibble::tibble(doc_id = character(), motivo = character())
}
