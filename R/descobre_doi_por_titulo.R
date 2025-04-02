#' Encontra o DOI via titulo do artigo
#'
#' @param titulo titulo do artigo
#'
#' @returns DOI do artigo
#' @export
#'
descobre_doi_por_titulo <- function(titulo) {
  query <- utils::URLencode(titulo)
  url <- paste0("https://api.crossref.org/works?query.bibliographic=", query)

  res <- httr::GET(url, httr::user_agent("litreviewR/0.1"))
  json <- httr::content(res, as = "parsed")

  items <- json$message$items
  if (length(items) > 0 && !is.null(items[[1]]$DOI)) {
    return(items[[1]]$DOI)
  } else {
    return(NULL)
  }
}
