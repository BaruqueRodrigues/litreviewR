#' Baixar PDF pelo Sci-Hub com lista de domínios configurável
#'
#' Os endereços são tentados na ordem fornecida em `urls`. O padrão vem de
#' `getOption("litreviewR.scihub_urls")` e usa `https://sci-hub.se`; configure
#' a opção com os domínios atuais quando o endereço mudar.
#'
#' @param referencia Lista com título e, opcionalmente, DOI e ID.
#' @param diretorio Diretório de destino.
#' @param delay Pausa em segundos entre domínios.
#' @param urls URLs base do serviço, em ordem de tentativa.
#' @param .get Transporte GET substituível para testes.
#' @param .post Transporte POST substituível para testes.
#' @param .baixar Função interna substituível para validar o arquivo retornado.
#' @return Resultado estruturado da tentativa de aquisição.
#' @export
baixa_pdf_scihub <- function(
    referencia, diretorio = ".", delay = 2,
    urls = getOption("litreviewR.scihub_urls", "https://sci-hub.se"),
    .get = httr::GET, .post = httr::POST,
    .baixar = .baixar_pdf_validado) {
  id <- .referencia_id(referencia)
  if (!is.numeric(delay) || length(delay) != 1L || is.na(delay) || delay < 0) {
    stop("`delay` deve ser um número não negativo.", call. = FALSE)
  }
  if (!is.character(urls) || !length(urls) || anyNA(urls) || any(!nzchar(urls))) {
    stop("`urls` deve conter ao menos um endereço base válido.", call. = FALSE)
  }
  urls <- sub("/+$", "", urls)
  doi <- normaliza_doi(referencia$doi %||% "")
  busca <- if (nzchar(doi)) doi else referencia$title %||% ""
  if (!is.character(busca) || length(busca) != 1L || is.na(busca) || !nzchar(busca)) {
    return(.download_result(id, "indisponivel", source = "scihub",
                            reason = "Referência sem DOI ou título."))
  }
  destino <- fs::path(diretorio, .safe_pdf_name(id))
  falhas <- character()

  for (base_url in urls) {
    pagina_url <- paste0(base_url, "/")
    resposta <- tryCatch({
      if (nzchar(doi)) {
        .get(paste0(pagina_url, utils::URLencode(busca, reserved = FALSE)),
             httr::user_agent("litreviewR/0.2.0"), httr::timeout(30))
      } else {
        .post(pagina_url, body = list(request = busca), encode = "form",
              httr::user_agent("litreviewR/0.2.0"), httr::timeout(30))
      }
    }, error = function(e) e)
    if (inherits(resposta, "error")) {
      falhas <- c(falhas, paste0(base_url, ": ", conditionMessage(resposta)))
      if (delay > 0 && base_url != utils::tail(urls, 1L)) Sys.sleep(delay)
      next
    }
    status_http <- tryCatch(httr::status_code(resposta), error = function(e) NA_integer_)
    if (is.na(status_http) || status_http < 200L || status_http >= 300L) {
      falhas <- c(falhas, paste0(base_url, ": HTTP ", status_http))
      if (delay > 0 && base_url != utils::tail(urls, 1L)) Sys.sleep(delay)
      next
    }

    html <- tryCatch(xml2::read_html(resposta), error = function(e) NULL)
    if (is.null(html)) {
      falhas <- c(falhas, paste0(base_url, ": HTML inválido"))
      next
    }
    pagina_final <- tryCatch(resposta$url %||% pagina_url, error = function(e) pagina_url)
    pdf_url <- .extrair_url_pdf_scihub(html, pagina_final)
    if (is.null(pdf_url)) {
      falhas <- c(falhas, paste0(base_url, ": sem link embed/iframe"))
      next
    }
    resultado <- .baixar(pdf_url, destino, id, "scihub", referer = pagina_final)
    if (resultado$status %in% c("sucesso", "ja_existente")) return(resultado)
    falhas <- c(falhas, paste0(base_url, ": ", resultado$reason %||% "download inválido"))
    if (delay > 0 && base_url != utils::tail(urls, 1L)) Sys.sleep(delay)
  }
  .download_result(id, "erro", source = "scihub", reason = paste(falhas, collapse = " | "))
}

.extrair_url_pdf_scihub <- function(html, base_url) {
  nodes <- tryCatch(rvest::html_elements(html, xpath = "//embed[@src] | //iframe[@src]"),
                    error = function(e) NULL)
  if (is.null(nodes) || !length(nodes)) return(NULL)
  srcs <- rvest::html_attr(nodes, "src")
  srcs <- srcs[!is.na(srcs) & nzchar(srcs)]
  if (!length(srcs)) return(NULL)
  pdf_candidate <- grep("\\.pdf([?#].*)?$", srcs, ignore.case = TRUE)
  src <- if (length(pdf_candidate)) srcs[[pdf_candidate[[1]]]] else srcs[[1]]
  tryCatch(xml2::url_absolute(src, base_url), error = function(e) NULL)
}
