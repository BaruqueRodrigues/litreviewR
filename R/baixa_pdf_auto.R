#' Pipeline automático de download de PDFs de artigos acadêmicos
#'
#' Esta função recebe uma lista de referências bibliográficas (com título e opcionalmente DOI e ID),
#' detecta automaticamente se o artigo é nacional ou internacional e realiza o download do PDF
#' usando a função mais apropriada: \code{baixa_pdf_aberto()} para SciELO e similares, ou \code{baixa_pdf_scihub()}
#' para artigos internacionais.
#'
#' Se o DOI não estiver presente, a função tenta identificá-lo via título usando a API da CrossRef.
#'
#' @param referencias Lista de referências (cada item deve ser uma lista com `title`, e opcionalmente `doi`, `id`)
#' @param diretorio Caminho para o diretório onde os PDFs serão salvos.
#' @param delay Tempo de espera (em segundos) entre os downloads. Padrão: 2.
#'
#' @return Salva os PDFs no diretório indicado e imprime um resumo do processo. Retorna `invisible(NULL)`.
#' @export
baixa_pdf_auto <- function(referencias, diretorio = "pdfs", delay = 2) {
  fs::dir_create(diretorio)

  # Garante que referencias seja uma lista de listas
  if (!is.list(referencias[[1]])) {
    referencias <- list(referencias)
  }

  resultados <- purrr::map(referencias, function(ref) {
    id <- ref$id %||% gsub("[^a-zA-Z0-9]", "_", ref$title)
    doi <- ref$doi %||% ""

    cli::cli_h1("🔍 Processando: {id}")

    # Detecta o DOI automaticamente se estiver ausente
    if (is.null(doi) || doi == "") {
      cli::cli_alert_info("Referência '{id}' não possui DOI. Buscando por título...")
      doi <- descobre_doi_por_titulo(ref$title)
      if (!is.null(doi)) {
        cli::cli_alert_success("DOI encontrado: {doi}")
        ref$doi <- doi
      } else {
        cli::cli_alert_warning("Não foi possível encontrar DOI para: {ref$title}")
      }
    }

    resultado <- tryCatch({
      if (!is.null(ref$doi) && is_referencia_nacional(ref$doi)) {
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
  total <- length(resultados)
  sucessos <- sum(resumo$status == "sucesso")
  erros <- total - sucessos

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
