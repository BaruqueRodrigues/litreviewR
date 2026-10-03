#' Baixar referências por fontes configuráveis e validar cada PDF
#'
#' As fontes são tentadas na ordem de `fontes`. Os valores aceitos são
#' `local`, `url`, `aberto` e `scihub`; Sci-Hub só é consultado depois das fontes
#' locais e abertas quando essa ordem padrão é usada.
#'
#' @param referencias Lista de referências ou uma referência individual.
#' @param diretorio Diretório de destino dos PDFs.
#' @param delay Pausa em segundos entre referências.
#' @param fontes Ordem das fontes: `local`, `url`, `aberto`, `scihub`.
#' @return Lista de resultados por referência, contendo ID, status, caminho,
#'   fonte, URL, motivo de falha e vetor `attempts` com o resultado de cada fonte tentada.
#' @export
baixa_pdf_auto <- function(referencias, diretorio = "pdfs", delay = 2,
                           fontes = getOption("litreviewR.download_sources",
                                              c("local", "url", "aberto", "scihub"))) {
  if (!is.numeric(delay) || length(delay) != 1L || is.na(delay) || delay < 0) {
    stop("`delay` deve ser um número não negativo.", call. = FALSE)
  }
  fontes_validas <- c("local", "url", "aberto", "scihub")
  if (!is.character(fontes) || !length(fontes) || anyNA(fontes) ||
      any(!fontes %in% fontes_validas) || anyDuplicated(fontes)) {
    stop("`fontes` deve ser uma sequência sem duplicatas de local, url, aberto e scihub.", call. = FALSE)
  }
  refs <- .normalizar_referencias_download(referencias)
  if (!length(refs)) return(list())
  fs::dir_create(diretorio)

  resultados <- purrr::map2(refs, seq_along(refs), function(ref, index) {
    if (!is.list(ref)) {
      return(.download_result(paste0("referencia_", index), "erro",
                              reason = "Cada referência deve ser uma lista nomeada."))
    }
    id <- .referencia_id(ref, index)
    destino <- fs::path(diretorio, .safe_pdf_name(id))
    if (.is_valid_pdf(destino)) {
      result <- .download_result(id, "ja_existente", destino, "local")
      result$attempts <- list(.download_result(id, "ja_existente", destino, "local"))
      return(result)
    }
    ultimo <- .download_result(id, "erro", reason = "Nenhuma fonte produziu um PDF válido.")
    tentativas <- list()
    cli::cli_h1("Processando referência: {id}")

    for (fonte in fontes) {
      atual <- tryCatch({
        if (fonte == "local") {
          origem <- ref$path %||% ref$pdf_path %||% NULL
          if (is.null(origem) || length(origem) != 1L || is.na(origem) || !file.exists(origem)) {
            .download_result(id, "indisponivel", source = "local", reason = "PDF local não informado ou inexistente.")
          } else if (!.is_valid_pdf(origem)) {
            .download_result(id, "erro", source = "local", path = origem, reason = "O arquivo local não é um PDF legível.")
          } else if (normalizePath(origem, mustWork = TRUE) == normalizePath(destino, mustWork = FALSE)) {
            .download_result(id, "ja_existente", destino, "local")
          } else {
            copied <- file.copy(origem, destino, overwrite = FALSE)
            if (copied && .is_valid_pdf(destino)) .download_result(id, "sucesso", destino, "local")
            else {
              if (file.exists(destino) && !.is_valid_pdf(destino)) unlink(destino)
              .download_result(id, "erro", source = "local", reason = "Não foi possível copiar e validar o PDF local.")
            }
          }
        } else if (fonte == "url") {
          url <- ref$pdf_url %||% ref$url_pdf %||% NULL
          if (is.null(url) || length(url) != 1L || is.na(url) || !grepl("^https?://", url, ignore.case = TRUE)) {
            .download_result(id, "indisponivel", source = "url", reason = "URL direta de PDF não informada.")
          } else {
            .baixar_pdf_validado(url, destino, id, "url")
          }
        } else if (fonte == "aberto") {
          baixa_pdf_aberto(ref, diretorio = diretorio, delay = 0)
        } else {
          baixa_pdf_scihub(ref, diretorio = diretorio, delay = 0)
        }
      }, error = function(e) .download_result(id, "erro", source = fonte, reason = conditionMessage(e)))
      tentativas[[length(tentativas) + 1L]] <- atual

      if (is.list(atual) && identical(atual$status, "sucesso")) {
        ultimo <- atual
        break
      }
      if (is.list(atual) && identical(atual$status, "ja_existente")) {
        ultimo <- atual
        break
      }
      if (is.list(atual)) ultimo <- atual
    }
    if (ultimo$status %in% c("sucesso", "ja_existente")) {
      cli::cli_alert_success("PDF válido: {ultimo$path}")
    } else {
      cli::cli_alert_warning("Falha em {id}: {ultimo$reason %||% 'motivo não informado'}")
    }
    ultimo$attempts <- tentativas
    if (delay > 0 && index < length(refs)) Sys.sleep(delay)
    ultimo
  })

  sucesso <- vapply(resultados, function(x) x$status %in% c("sucesso", "ja_existente"), logical(1))
  cli::cli_h2("Resumo da aquisição")
  cli::cli_text("Referências: {length(resultados)} · PDFs válidos: {sum(sucesso)} · pendentes/falhas: {sum(!sucesso)}")
  resultados
}
