#' Função mestre que decide entre baixar PDF via SciELO ou Sci-Hub
#'
#' Recebe uma lista de referências (com título e DOI) e decide automaticamente se deve usar
#' \code{baixa_pdf_aberto()} (para artigos nacionais) ou \code{baixa_pdf_scihub()} (para internacionais).
#' Ao final, exibe um resumo com o status de cada tentativa.
#'
#' @param referencias Uma lista de listas, onde cada entrada deve conter ao menos `title` e opcionalmente `doi` e `id`.
#' @param diretorio Diretório onde os PDFs serão salvos.
#' @param delay Delay entre as requisições, em segundos. Default: `2`.
#'
#' @return Retorna `invisible(NULL)`. Os PDFs são salvos no diretório indicado.
#' @export
baixa_pdf_auto <- function(referencias, diretorio = "pdfs", delay = 2) {
  fs::dir_create(diretorio)

  resultados <- purrr::map(referencias, function(ref) {
    id  <- ref$id %||% gsub("[^a-zA-Z0-9]", "_", ref$title)
    doi <- ref$doi %||% ""

    cli::cli_h1("🔍 Processando: {id}")

    resultado <- tryCatch({
      if (!is.null(doi) && doi != "" && is_referencia_nacional(doi)) {
        cli::cli_alert_info("Artigo detectado como nacional. Usando SciELO/aberto.")
        baixa_pdf_aberto(ref, diretorio = diretorio, delay = delay)
      } else {
        cli::cli_alert_info("Artigo internacional ou sem DOI. Usando Sci-Hub.")
        baixa_pdf_scihub(ref, diretorio = diretorio, delay = delay)
      }
      list(id = id, status = "sucesso")
    }, error = function(e) {
      cli::cli_alert_danger("Erro ao baixar '{id}': {e$message}")
      list(id = id, status = "erro")
    })

    cli::cli_rule()
    resultado
  })

  resumo <- purrr::transpose(resultados)

  total   <- length(resultados)
  sucessos <- sum(resumo$status == "sucesso")
  erros   <- total - sucessos

  cli::cli_h2("📊 Resumo do download")
  cli::cli_alert_success("Total de artigos: {total}")
  cli::cli_alert_success("Baixados com sucesso: {sucessos}")
  cli::cli_alert_warning("Falhas: {erros}")

  if (erros > 0) {
    ids_falhos <- unlist(resumo$id[resumo$status == "erro"])
    cli::cli_alert_danger("Artigos com falha no download:")
    purrr::walk(ids_falhos, ~cli::cli_alert_bullet(.x))
  }

  invisible(NULL)
}
