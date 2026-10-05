# Internal access to Zotero attachment files. This module deliberately resolves
# only the local file URL returned by Zotero; it never downloads arbitrary URLs.

.zotero_attachment_source <- function(config, attachment_key) {
  .zotero_validate_key(attachment_key, field = "attachment_key")

  backend <- config$backend
  if (identical(backend, "web")) {
    return(list(status = "not_available", path = NULL,
                reason = "web_attachment_unavailable"))
  }
  if (!identical(backend, "local")) {
    .zotero_abort("INVALID_CONFIG", "Backend Zotero nao suportado.")
  }

  library_path <- .zotero_library_path(config)
  if (!is.character(library_path) || length(library_path) != 1L ||
      is.na(library_path) || !nzchar(library_path) ||
      !grepl("^(users|groups)/[0-9]+$", library_path)) {
    .zotero_abort("INVALID_CONFIG", "Caminho da biblioteca Zotero invalido.")
  }
  path <- paste0(sub("/$", "", library_path), "/items/",
                 attachment_key, "/file/view/url")
  response <- .zotero_request(config, path, expect_json = FALSE)
  if (!is.list(response) || !is.character(response$body) ||
      length(response$body) != 1L || is.na(response$body)) {
    .zotero_abort("INVALID_RESPONSE", "Resposta de arquivo Zotero invalida.")
  }

  file_url <- trimws(response$body)
  local_path <- .zotero_file_url_path(file_url)
  if (is.null(local_path)) {
    return(list(status = "not_available", path = NULL,
                reason = "local_file_url_unavailable"))
  }
  if (!file.exists(local_path) || dir.exists(local_path) ||
      file.access(local_path, 4L) != 0L) {
    return(list(status = "not_available", path = NULL,
                reason = "local_file_unavailable"))
  }

  list(status = "available", path = local_path, reason = NULL)
}

.zotero_file_url_path <- function(file_url) {
  if (!is.character(file_url) || length(file_url) != 1L || is.na(file_url) ||
      !nzchar(file_url) || grepl("[?#]", file_url) ||
      grepl("%(?![[:xdigit:]]{2})", file_url, perl = TRUE)) {
    return(NULL)
  }

  if (grepl("^file:///", file_url, ignore.case = TRUE)) {
    encoded_path <- substring(file_url, 8L)
  } else if (grepl("^file://localhost/", file_url, ignore.case = TRUE)) {
    encoded_path <- substring(file_url, 17L)
  } else {
    return(NULL)
  }

  decoded <- tryCatch(utils::URLdecode(encoded_path), error = function(e) NULL)
  if (!is.character(decoded) || length(decoded) != 1L || is.na(decoded) ||
      !nzchar(decoded) || !startsWith(decoded, "/") ||
      grepl("[[:cntrl:]]", decoded)) {
    return(NULL)
  }
  decoded
}
