#' Consulta metadados de um artigo via CrossRef
#'
#' Busca metadados como \code{publisher}, \code{type} e \code{container-title} a partir de um DOI.
#'
#' @param doi String com o DOI do artigo.
#'
#' @return Uma lista com os campos \code{publisher}, \code{container}, \code{type} e \code{url}.
#' @export
crossref_info <- function(doi) {
  url <- paste0("https://api.crossref.org/works/", doi)

  res <- tryCatch({
    httr::GET(url, httr::user_agent("litreviewR/0.1"))
  }, error = function(e) return(NULL))

  if (is.null(res) || res$status_code != 200) return(NULL)

  content <- httr::content(res, as = "parsed")
  info <- content$message

  list(
    publisher = info$publisher %||% NA,
    container = info$`container-title`[[1]] %||% NA,
    type      = info$type %||% NA,
    url       = info$URL %||% NA
  )
}
