#' Gera um CSV com resumos extraídos de PDFs para anotação manual
#'
#' Extrai os primeiros 1000 caracteres de cada PDF, detecta o idioma, e exporta um arquivo
#' CSV com colunas de anotação manual para área temática e corrente teórica.
#'
#' @param pasta_pdfs Caminho para a pasta com os arquivos PDF.
#' @param caminho_saida Caminho para salvar o arquivo CSV. Default: "resumos_para_anotacao.csv"
#'
#' @return Salva o CSV no diretório indicado.
#' @export
gera_csv_resumos_para_anotacao <- function(pasta_pdfs, caminho_saida = "pdfs/resumos_para_anotacao.csv") {

  arquivos <- list.files(pasta_pdfs, pattern = "\\.pdf$", full.names = TRUE)

  resumos_df <- purrr::map_dfr(arquivos, function(arquivo) {
    texto <- tryCatch(pdftools::pdf_text(arquivo), error = function(e) return(NA))
    texto <- paste(texto, collapse = " ")
    texto <- stringr::str_squish(texto)

    # Regex para pegar "Resumo" ou "Abstract" com tolerância
    padrao_resumo <- "(?i)(resumo|abstract)[\\s:\\-\\n]*"
    match <- stringr::str_locate(texto, stringr::regex(padrao_resumo, multiline = TRUE))

    if (!is.na(match[1,1])) {
      inicio <- match[1,2] + 1
      resumo <- substr(texto, inicio, inicio + 1500)
    } else {
      resumo <- substr(texto, 1, 1000)
    }

    idioma <- tryCatch(cld2::detect_language(resumo), error = function(e) NA)

    # Ajuda no debugging
    cat("Arquivo:", fs::path_file(arquivo), "| Idioma detectado:", idioma, "\n")

    tibble::tibble(
      id = fs::path_file(arquivo),
      resumo = resumo,
      idioma = idioma,
      anotacao_tematica = "",
      corrente_teorica = ""
    )
  })

  readr::write_csv(resumos_df, caminho_saida)
  cli::cli_alert_success("Arquivo salvo em: {caminho_saida}")
  return(invisible(resumos_df))
}
