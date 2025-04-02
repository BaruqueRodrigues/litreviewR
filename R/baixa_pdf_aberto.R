#' Baixa PDF de artigos hospedados em portais nacionais de acesso aberto (ex: SciELO)
#'
#' A função tenta baixar automaticamente o PDF de um artigo hospedado em repositórios nacionais,
#' como a SciELO. Se o DOI não estiver presente, tenta buscar por título via CrossRef.
#'
#' @param referencia Uma lista contendo ao menos `title`, e opcionalmente `doi` e `id`.
#' @param diretorio Diretório onde o PDF será salvo. Default é `"."`.
#' @param delay Tempo em segundos entre as requisições (para respeitar o servidor). Default: `2`.
#'
#' @return O PDF é salvo no diretório indicado. Retorna `invisible(NULL)`.
#' @export
baixa_pdf_aberto <- function(referencia, diretorio = ".", delay = 2) {
  fs::dir_create(diretorio)

  id <- referencia$id %||% gsub("[^a-zA-Z0-9]", "_", referencia$title)
  doi <- referencia$doi

  if (is.null(doi) || doi == "") {
    cli::cli_alert_info("Referência '{id}' não possui DOI. Buscando por título...")
    doi <- descobre_doi_por_titulo(referencia$title)

    if (is.null(doi)) {
      cli::cli_alert_warning("Não foi possível encontrar o DOI para: {referencia$title}")
      return(invisible(NULL))
    }

    cli::cli_alert_success("DOI encontrado: {doi}")
  }

  url_doi <- paste0("https://doi.org/", doi)

  res <- tryCatch({
    httr::GET(url_doi, httr::user_agent("Mozilla/5.0"))
  }, error = function(e) {
    cli::cli_alert_danger("Erro ao acessar DOI '{doi}': {e$message}")
    return(NULL)
  })

  if (is.null(res) || res$status_code != 200) {
    cli::cli_alert_danger("Não foi possível acessar a página do DOI '{doi}'.")
    return(invisible(NULL))
  }

  url_artigo <- res$url

  if (!grepl("scielo.br", url_artigo)) {
    cli::cli_alert_info("Artigo não está hospedado na SciELO: {url_artigo}")
    return(invisible(NULL))
  }

  pdf_url <- paste0(url_artigo, "?format=pdf&lang=pt")
  caminho <- fs::path(diretorio, paste0(id, ".pdf"))

  download <- tryCatch({
    httr::GET(pdf_url,
              httr::write_disk(caminho, overwrite = TRUE),
              httr::user_agent("Mozilla/5.0"))
  }, error = function(e) {
    cli::cli_alert_danger("Erro ao baixar PDF de '{pdf_url}': {e$message}")
    return(NULL)
  })

  if (!is.null(download) && download$status_code == 200) {
    cli::cli_alert_success("PDF salvo como: {caminho}")
  } else {
    cli::cli_alert_danger("Erro ao salvar PDF de '{pdf_url}'")
  }

  Sys.sleep(delay)
  invisible(NULL)
}
