.download_result <- function(id, status, path = NULL, source = NULL,
                             url = NULL, reason = NULL) {
  list(id = as.character(id)[1], article_id = as.character(id)[1], status = status, path = path,
       source = source, url = url, reason = reason)
}

.is_valid_pdf <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !file.exists(path) || isTRUE(file.info(path)$isdir) ||
      is.na(file.info(path)$size) || file.info(path)$size < 8L) return(FALSE)
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  signature <- tryCatch(readBin(con, "raw", n = 5L), error = function(e) raw())
  if (!identical(signature, charToRaw("%PDF-"))) return(FALSE)
  info <- tryCatch(pdftools::pdf_info(path), error = function(e) NULL)
  is.list(info) && is.numeric(info$pages) && length(info$pages) == 1L &&
    is.finite(info$pages) && info$pages > 0
}

.safe_pdf_name <- function(id) {
  id <- gsub("[^A-Za-z0-9._-]+", "_", as.character(id)[1])
  id <- gsub("^\\.+|\\.+$", "", id)
  if (!nzchar(id)) id <- "artigo"
  paste0(id, ".pdf")
}

.baixar_pdf_validado <- function(url, destino, id, fonte, timeout = 60,
                                 referer = NULL, .get = httr::GET) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  if (.is_valid_pdf(destino)) {
    return(.download_result(id, "ja_existente", destino, fonte, url))
  }

  temporario <- tempfile(pattern = ".litreviewR-", tmpdir = dirname(destino),
                         fileext = ".part")
  on.exit(unlink(temporario), add = TRUE)
  args <- list(url = url,
    httr::write_disk(temporario, overwrite = TRUE),
    httr::user_agent("litreviewR/0.2.0"),
    httr::timeout(timeout))
  if (!is.null(referer) && nzchar(referer)) args <- c(args, list(httr::add_headers(Referer = referer)))
  resposta <- tryCatch(do.call(.get, args), error = function(e) e)
  if (inherits(resposta, "error")) {
    return(.download_result(id, "erro", source = fonte, url = url,
                            reason = conditionMessage(resposta)))
  }
  status_http <- tryCatch(httr::status_code(resposta), error = function(e) NA_integer_)
  if (is.na(status_http) || status_http < 200L || status_http >= 300L) {
    return(.download_result(id, "erro", source = fonte, url = url,
                            reason = paste("HTTP", status_http)))
  }
  if (!.is_valid_pdf(temporario)) {
    return(.download_result(id, "erro", source = fonte, url = url,
                            reason = "A resposta não é um PDF legível com ao menos uma página."))
  }
  if (file.exists(destino)) unlink(destino)
  moved <- file.rename(temporario, destino)
  if (!moved) {
    copied <- file.copy(temporario, destino, overwrite = TRUE)
    if (copied) unlink(temporario)
    moved <- copied
  }
  if (!moved || !.is_valid_pdf(destino)) {
    if (file.exists(destino) && !.is_valid_pdf(destino)) unlink(destino)
    return(.download_result(id, "erro", source = fonte, url = url,
                            reason = "Não foi possível finalizar o PDF validado."))
  }
  .download_result(id, "sucesso", destino, fonte, url)
}

.normalizar_referencias_download <- function(referencias) {
  if (is.null(referencias) || !length(referencias)) return(list())
  if (is.list(referencias) && !is.null(names(referencias)) &&
      any(names(referencias) %in% c("title", "doi", "id", "path", "pdf_path"))) {
    return(list(referencias))
  }
  if (!is.list(referencias)) stop("`referencias` deve ser uma lista de referências.", call. = FALSE)
  referencias
}

.referencia_id <- function(ref, index = 1L) {
  if (!is.null(ref$id) && length(ref$id) == 1L && !is.na(ref$id) && nzchar(ref$id)) {
    return(as.character(ref$id))
  }
  if (!is.null(ref$doi) && length(ref$doi) == 1L && !is.na(ref$doi) && nzchar(ref$doi)) {
    return(gsub("[^A-Za-z0-9]+", "_", ref$doi))
  }
  if (!is.null(ref$title) && length(ref$title) == 1L && !is.na(ref$title) && nzchar(ref$title)) {
    return(paste0("artigo_", .hash_text(paste(ref$title, ref$author %||% "", ref$year %||% ""))))
  }
  paste0("artigo_", index)
}

.hash_text <- function(x) {
  bytes <- as.integer(charToRaw(enc2utf8(paste(x, collapse = " "))))
  value <- 0
  for (byte in bytes) value <- (value * 131 + byte) %% 2147483647
  sprintf("%08x", as.integer(value))
}
