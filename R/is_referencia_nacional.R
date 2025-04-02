#' Verifica se o artigo é de origem nacional com base no DOI
#'
#' Utiliza o redirecionamento do DOI para verificar se o artigo está hospedado em um domínio nacional,
#' e também verifica o publisher via CrossRef como fallback.
#'
#' @param doi String com o DOI do artigo.
#'
#' @return TRUE se for nacional, FALSE caso contrário.
#' @export
is_referencia_nacional <- function(doi) {
  if (is.null(doi) || doi == "") return(FALSE)

  # 1. Usa cache se disponível
  if (exists(doi, envir = .litreview_cache)) {
    final_url <- get(doi, envir = .litreview_cache)
  } else {
    url_doi <- paste0("https://doi.org/", doi)
    res <- tryCatch(httr::GET(url_doi, httr::user_agent("Mozilla/5.0")), error = function(e) return(NULL))
    if (is.null(res)) return(FALSE)
    final_url <- res$url
    assign(doi, final_url, envir = .litreview_cache)
  }

  dominios_nacionais <- c("scielo.br", "revistas.usp.br", "anpocs.com", "periodicos.capes.gov.br")
  if (any(stringr::str_detect(final_url, dominios_nacionais))) {
    return(TRUE)
  }

  # Fallback: consulta CrossRef
  meta <- crossref_info(doi)
  editoras_nacionais <- c("SciELO", "Associação Nacional de Pós-Graduação", "Universidade de São Paulo", "Revista de Sociologia e Política")

  if (!is.null(meta$publisher) && any(stringr::str_detect(meta$publisher, editoras_nacionais))) {
    return(TRUE)
  }

  return(FALSE)
}
