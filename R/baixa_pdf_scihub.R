#' Baixa o PDF de um artigo acadêmico usando o Sci-Hub, a partir de um DOI ou título
#'
#' Esta função acessa o Sci-Hub com base no DOI ou título da referência, extrai o link do PDF e realiza o download para o diretório especificado.
#'
#' @param referencia Uma lista com pelo menos os campos `title` (obrigatório) e `doi` (opcional). O campo `id` será usado como nome do arquivo salvo.
#' @param diretorio Diretório onde o PDF será salvo. Será criado caso não exista. Padrão: `"."`.
#' @param delay Tempo (em segundos) de espera após cada download, para evitar bloqueios. Padrão: `2`.
#'
#' @return Nenhum valor de retorno. O PDF será salvo no diretório especificado.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ref_com_doi <- list(
#' id = "exemplo_doi",
#' title = "Quid Pro Quo: Builders, Politicians, and Election Finance in India",
#' doi = "10.2139/ssrn.1972987" )
#' ref_sem_doi <- list(
#'   id = "exemplo_sem_doi",
#'     title = "Quid pro quo: Builders, politicians, and election finance in India")
#'
#' baixa_pdf_scihub(ref_com_doi, diretorio = "pdfs")
#' baixa_pdf_scihub(ref_sem_doi, diretorio = "pdfs") }
baixa_pdf_scihub <- function(referencia, diretorio = ".", delay = 2) {
  fs::dir_create(diretorio)

  id <- referencia$id %||% base::gsub("[^a-zA-Z0-9]", "_", referencia$title)
  base_url <- "https://sci-hub.se/"

  busca <- if (!base::is.null(referencia$doi) && referencia$doi != "") {
    referencia$doi
  } else if (!base::is.null(referencia$title) && referencia$title != "") {
    referencia$title
  } else {
    cli::cli_alert_warning("Referência '{id}' não possui DOI nem título.")
    return(base::invisible(NULL))
  }

  res <- base::tryCatch({
    if (!base::is.null(referencia$doi) && referencia$doi != "") {
      httr::GET(base::paste0(base_url, busca), httr::user_agent("Mozilla/5.0"))
    } else {
      httr::POST(base_url, body = list(request = busca), encode = "form",
                 httr::user_agent("Mozilla/5.0"))
    }
  }, error = function(e) {
    cli::cli_alert_danger("Erro ao acessar o Sci-Hub para '{busca}': {e$message}")
    return(NULL)
  })

  if (base::is.null(res) || res$status_code != 200) {
    cli::cli_alert_danger("Não foi possível acessar o Sci-Hub para '{busca}'.")
    return(base::invisible(NULL))
  }

  html <- xml2::read_html(res)

  pdf_node <- rvest::html_node(html, xpath = "//embed[contains(@src, '.pdf')]") %||%
    rvest::html_node(html, xpath = "//iframe[contains(@src, '.pdf')]")

  if (base::is.na(pdf_node)) {
    cli::cli_alert_info("PDF não encontrado para '{busca}' (nenhum <embed> ou <iframe> com .pdf).")
    return(base::invisible(NULL))
  }

  pdf_url <- rvest::html_attr(pdf_node, "src")
  if (!stringr::str_starts(pdf_url, "http")) {
    pdf_url <- base::paste0("https:", pdf_url)
  }

  caminho <- fs::path(diretorio, base::paste0(id, ".pdf"))

  base::tryCatch({
    download <- httr::GET(pdf_url,
                          httr::write_disk(caminho, overwrite = TRUE),
                          httr::user_agent("Mozilla/5.0"))
    if (download$status_code == 200) {
      cli::cli_alert_success("PDF salvo como: {caminho}")
    } else {
      cli::cli_alert_danger("Erro ao baixar PDF: status {download$status_code}")
    }
  }, error = function(e) {
    cli::cli_alert_danger("Erro ao baixar PDF de '{pdf_url}': {e$message}")
  })

  base::Sys.sleep(delay)
  base::invisible(NULL)
}


