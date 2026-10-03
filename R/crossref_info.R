#' Consulta metadados de um artigo via CrossRef
#'
#' Busca metadados como \code{publisher}, \code{type} e \code{container-title} a partir de um DOI.
#'
#' @param doi String com o DOI do artigo.
#' @param timeout Limite HTTP em segundos.
#' @param .get Transporte HTTP substituível para testes.
#'
#' @return Uma lista com os campos \code{publisher}, \code{container}, \code{type} e \code{url}.
#' @export
crossref_info <- function(doi, timeout = 20, .get = httr::GET) {
  doi <- normaliza_doi(doi)
  if (!nzchar(doi)) return(NULL)
  url <- paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = FALSE))

  res <- tryCatch({
    .get(url, httr::user_agent("litreviewR/0.2.0"), httr::timeout(timeout))
  }, error = function(e) return(NULL))

  if (is.null(res) || httr::status_code(res) < 200L || httr::status_code(res) >= 300L) return(NULL)

  content <- tryCatch(httr::content(res, as = "parsed", type = "application/json", encoding = "UTF-8"),
                      error = function(e) NULL)
  info <- content$message
  if (!is.list(info)) return(NULL)

  list(
    publisher = info$publisher %||% NA,
    container = info$`container-title`[[1]] %||% NA,
    type      = info$type %||% NA,
    url       = info$URL %||% NA
  )
}
