#' Exportar resumos encontrados em PDFs para anotação manual
#'
#' Procura marcadores `Resumo` e `Abstract`, interrompe antes de palavras-chave
#' ou da introdução e identifica recortes incertos. Não substitui resumo
#' ausente por cabeçalho ou início arbitrário do artigo.
#'
#' @param pasta_pdfs Pasta com PDFs.
#' @param caminho_saida Arquivo CSV de saída.
#' @param ocr Tentar OCR quando não houver camada textual. Requer suporte de
#'   OCR disponível para `pdftools::pdf_ocr_text()`.
#' @param idioma_ocr Idioma passado ao OCR, caso ativado.
#' @return Tibble gravado em CSV e retornado invisivelmente.
#' @export
gera_csv_resumos_para_anotacao <- function(pasta_pdfs,
                                           caminho_saida = "resumos_para_anotacao.csv",
                                           ocr = FALSE,
                                           idioma_ocr = "por+eng") {
  if (!dir.exists(pasta_pdfs)) stop("`pasta_pdfs` deve ser uma pasta existente.", call. = FALSE)
  arquivos <- sort(list.files(pasta_pdfs, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE))
  if (!length(arquivos)) stop("Nenhum PDF foi encontrado em `pasta_pdfs`.", call. = FALSE)

  rows <- purrr::map(arquivos, function(arquivo) {
    pages <- tryCatch(pdftools::pdf_text(arquivo), error = function(e) e)
    status <- "texto_extraido"
    origem <- "pdf_text"
    texto <- if (inherits(pages, "error")) "" else paste(pages, collapse = "\n")
    if (inherits(pages, "error")) status <- "erro_leitura"
    if (!nzchar(trimws(texto)) && !inherits(pages, "error")) {
      status <- "sem_camada_textual"
      if (isTRUE(ocr)) {
        ocr_text <- tryCatch(pdftools::pdf_ocr_text(arquivo, language = idioma_ocr), error = function(e) e)
        if (inherits(ocr_text, "error")) {
          status <- "ocr_indisponivel_ou_falhou"
        } else {
          texto <- paste(ocr_text, collapse = "\n")
          origem <- "ocr"
          status <- if (nzchar(trimws(texto))) "ocr_concluido" else "ocr_sem_texto"
        }
      }
    }
    resumo_info <- .extrair_resumo(texto)
    if (!nzchar(resumo_info$resumo) && status %in% c("texto_extraido", "ocr_concluido")) status <- "marcador_resumo_ausente"
    if (nzchar(resumo_info$resumo) && isTRUE(resumo_info$incerto)) status <- "recorte_incerto"
    idioma <- if (nzchar(resumo_info$resumo) && requireNamespace("cld2", quietly = TRUE)) {
      tryCatch(cld2::detect_language(resumo_info$resumo), error = function(e) NA_character_)
    } else NA_character_
    tibble::tibble(
      id = tools::file_path_sans_ext(basename(arquivo)),
      caminho = normalizePath(arquivo, mustWork = TRUE),
      resumo = resumo_info$resumo,
      origem_resumo = if (nzchar(resumo_info$resumo)) origem else NA_character_,
      status_extracao = status,
      idioma = idioma,
      anotacao_tematica = "",
      corrente_teorica = ""
    )
  })
  result <- dplyr::bind_rows(rows)
  dir.create(dirname(caminho_saida), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(result, caminho_saida, na = "")
  cli::cli_alert_success("CSV salvo em: {caminho_saida}")
  invisible(result)
}

.extrair_resumo <- function(texto) {
  if (!is.character(texto) || length(texto) != 1L || is.na(texto) || !nzchar(trimws(texto))) {
    return(list(resumo = "", incerto = TRUE))
  }
  linhas <- strsplit(gsub("\\r", "", texto), "\\n", fixed = FALSE)[[1]]
  marcador <- "^\\s*(resumo|abstract)\\b\\s*[:.\\-]?\\s*|\\b(resumo|abstract)\\b\\s*[:.\\-]?\\s*$"
  inicio <- grep(marcador, linhas, ignore.case = TRUE, perl = TRUE)[1]
  if (is.na(inicio)) return(list(resumo = "", incerto = TRUE))
  primeira <- sub("^.*\\b(resumo|abstract)\\b\\s*[:.\\-]?\\s*", "", linhas[[inicio]], ignore.case = TRUE, perl = TRUE)
  trecho <- c(primeira, if (inicio < length(linhas)) linhas[seq.int(inicio + 1L, length(linhas))])
  delimitador <- "\\b(palavras[- ]chave|keywords?|introdu[cç][aã]o|introduction)\\b"
  termina <- grep(delimitador, trecho, ignore.case = TRUE, perl = TRUE)[1]
  incerto <- is.na(termina)
  if (!is.na(termina)) {
    linha <- trecho[[termina]]
    pos <- regexpr(delimitador, linha, ignore.case = TRUE, perl = TRUE)[[1]]
    prefixo <- if (pos > 1L) trimws(substr(linha, 1L, pos - 1L)) else ""
    trecho <- c(if (termina > 1L) trecho[seq_len(termina - 1L)] else character(), prefixo)
    # Cabeçalhos de duas colunas às vezes aparecem após o texto da coluna anterior.
    incerto <- pos > 1L
  }
  resumo <- stringr::str_squish(paste(trecho, collapse = " "))
  list(resumo = resumo, incerto = incerto || !nzchar(resumo))
}
