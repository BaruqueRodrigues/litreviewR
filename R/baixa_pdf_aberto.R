#' Baixar e validar o PDF ligado a um DOI em fonte aberta suportada
#'
#' Nesta versão, o redirecionamento do DOI é aceito quando leva ao portal
#' SciELO. A função retorna um resultado estruturado; uma falha nunca é
#' representada como sucesso ou como `NULL`.
#'
#' @param referencia Lista contendo título e, opcionalmente, DOI e ID.
#' @param diretorio Diretório de destino.
#' @param delay Pausa em segundos após a tentativa.
#' @param .get Transporte HTTP substituível para testes.
#' @param .baixar Função interna substituível para validar o arquivo retornado.
#' @return Resultado com ID, status, caminho, fonte, URL e motivo de falha.
#' @export
baixa_pdf_aberto <- function(referencia, diretorio = ".", delay = 2,
                             .get = httr::GET,
                             .baixar = .baixar_pdf_validado) {
  id <- .referencia_id(referencia)
  if (!is.numeric(delay) || length(delay) != 1L || is.na(delay) || delay < 0) {
    stop("`delay` deve ser um número não negativo.", call. = FALSE)
  }
  titulo <- referencia$title %||% ""
  titulo_valido <- is.character(titulo) && length(titulo) == 1L && !is.na(titulo) && nzchar(trimws(titulo))
  doi <- normaliza_doi(referencia$doi %||% "")
  if (!nzchar(doi) && titulo_valido) doi <- descobre_doi_por_titulo(titulo)
  if (is.null(doi) || !nzchar(doi)) {
    return(.download_result(id, "indisponivel", source = "aberto",
                            reason = "Não foi possível confirmar um DOI para a referência."))
  }

  doi_url <- paste0("https://doi.org/", utils::URLencode(doi, reserved = FALSE))
  resposta <- tryCatch(
    .get(doi_url, httr::user_agent("litreviewR/0.2.0"), httr::timeout(30)),
    error = function(e) e
  )
  if (inherits(resposta, "error")) {
    return(.download_result(id, "erro", source = "aberto", url = doi_url,
                            reason = conditionMessage(resposta)))
  }
  status_http <- tryCatch(httr::status_code(resposta), error = function(e) NA_integer_)
  if (is.na(status_http) || status_http < 200L || status_http >= 300L) {
    return(.download_result(id, "erro", source = "aberto", url = doi_url,
                            reason = paste("DOI retornou HTTP", status_http)))
  }
  url_artigo <- tryCatch(resposta$url %||% "", error = function(e) "")
  if (!grepl("(^|\\.)scielo\\.br(/|$)", sub("^https?://", "", url_artigo), ignore.case = TRUE)) {
    return(.download_result(id, "indisponivel", source = "aberto", url = url_artigo,
                            reason = "O redirecionamento não terminou em um portal SciELO suportado."))
  }
  pdf_url <- paste0(url_artigo, if (grepl("\\?", url_artigo)) "&" else "?", "format=pdf&lang=pt")
  destino <- fs::path(diretorio, .safe_pdf_name(id))
  resultado <- .baixar(pdf_url, destino, id, "aberto", referer = url_artigo)
  if (delay > 0) Sys.sleep(delay)
  resultado
}
