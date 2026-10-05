#!/usr/bin/env Rscript

.zotero_error_codes <- c(
  "INVALID_CONFIG", "INVALID_ARGUMENT", "CONNECTION_UNAVAILABLE", "ACCESS_DENIED",
  "NOT_FOUND", "RATE_LIMITED", "INVALID_RESPONSE", "ATTACHMENT_UNAVAILABLE",
  "VERSION_CONFLICT", "WRITE_FAILED", "CORPUS_LOCKED", "LIMIT_EXCEEDED",
  "IDENTITY_UNRESOLVED", "BACKEND_UNAVAILABLE"
)

.zotero_fail <- function(code, message) {
  condition <- structure(
    list(message = message, call = NULL, code = code, retryable = FALSE, details = NULL),
    class = c("zotero_error", "error", "condition")
  )
  stop(condition)
}

.zotero_script_root <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(file_arg)) return(NULL)
  script <- normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
  root <- normalizePath(file.path(dirname(script), "..", ".."), mustWork = FALSE)
  if (dir.exists(file.path(root, "R"))) root else NULL
}

.zotero_load_functions <- function() {
  root <- .zotero_script_root()
  if (!is.null(root)) {
    r_files <- sort(list.files(file.path(root, "R"), pattern = "\\.[rR]$", full.names = TRUE))
    runtime <- new.env(parent = globalenv())
    for (file in r_files) sys.source(file, envir = runtime, keep.source = FALSE)
    required <- c("zotero_config", "zotero_status", "zotero_list_collections",
                  "zotero_search_items", "zotero_get_item", "zotero_import_collection")
    if (!all(vapply(required, exists, logical(1), envir = runtime, mode = "function",
                    inherits = FALSE))) {
      .zotero_fail("BACKEND_UNAVAILABLE", "O núcleo R não contém as funções Zotero necessárias.")
    }
    return(runtime)
  }
  if (requireNamespace("litreviewR", quietly = TRUE)) {
    runtime <- asNamespace("litreviewR")
    return(runtime)
  }
  .zotero_fail("BACKEND_UNAVAILABLE", "Instale litreviewR ou execute o servidor a partir do checkout do pacote.")
}

.zotero_read_map <- function(env_name, required = TRUE) {
  text <- Sys.getenv(env_name, unset = "")
  if (!nzchar(text)) {
    if (required) .zotero_fail("INVALID_CONFIG", paste0("Configure ", env_name, " no ambiente do servidor."))
    return(list())
  }
  tryCatch({
    value <- jsonlite::fromJSON(text, simplifyVector = FALSE)
    if (!is.list(value) || (length(value) && (is.null(names(value)) || any(!nzchar(names(value)))))) {
      .zotero_fail("INVALID_CONFIG", paste0(env_name, " deve ser um objeto JSON nomeado."))
    }
    value
  }, error = function(error) {
    if (inherits(error, "zotero_error")) stop(error)
    .zotero_fail("INVALID_CONFIG", paste0(env_name, " contém JSON inválido."))
  })
}

.zotero_validate_identifier <- function(value, field) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$", value)) {
    .zotero_fail("INVALID_ARGUMENT", paste0(field, " inválido."))
  }
  value
}

.zotero_fixture_transport <- function(base_url) {
  fixtures <- .zotero_read_map("LITREVIEW_ZOTERO_TEST_TRANSPORT_JSON")
  function(request) {
    base <- sub("/+$", "", base_url)
    prefix <- paste0(base, "/")
    if (!startsWith(request$url, prefix)) {
      .zotero_fail("INVALID_ARGUMENT", "Fixture HTTP recusou uma origem inesperada.")
    }
    path <- substring(request$url, nchar(prefix) + 1L)
    if (!nzchar(path)) path <- "__root__"
    response <- fixtures[[path]]
    if (!is.list(response) || is.null(response$status) || is.null(response$body)) {
      .zotero_fail("CONNECTION_UNAVAILABLE", "Fixture HTTP não definiu esta rota.")
    }
    body <- response$body
    if (is.list(body)) {
      body <- as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"))
    }
    list(status = as.integer(response$status),
         headers = response$headers %||% list(), body = body)
  }
}

.zotero_profile_config <- function(runtime, profile_id) {
  profile_id <- .zotero_validate_identifier(profile_id, "library_profile")
  profiles <- .zotero_read_map("LITREVIEW_ZOTERO_PROFILES_JSON")
  profile <- profiles[[profile_id]]
  if (!is.list(profile)) .zotero_fail("INVALID_CONFIG", "Perfil Zotero não autorizado neste servidor.")
  allowed <- c("backend", "library_type", "library_id", "base_url", "api_key_env",
               "instance_id", "timeout", "max_response_bytes", "max_file_bytes", "max_pages")
  required <- c("backend", "library_type", "library_id")
  if (length(setdiff(required, names(profile)))) {
    .zotero_fail("INVALID_CONFIG", "Perfil Zotero precisa definir backend, library_type e library_id.")
  }
  if (length(setdiff(names(profile), allowed))) {
    .zotero_fail("INVALID_CONFIG", "O perfil contém campos não permitidos.")
  }
  if (any(tolower(names(profile)) %in% c("api_key", "credential", "token", "secret"))) {
    .zotero_fail("INVALID_CONFIG", "Perfis guardam nomes de variáveis, nunca credenciais.")
  }
  profile$api_key_env <- profile$api_key_env %||% "ZOTERO_API_KEY"
  if (identical(Sys.getenv("LITREVIEW_ZOTERO_TEST_MODE"), "1")) {
    profile$.transport <- .zotero_fixture_transport(profile$base_url %||% "http://127.0.0.1:23119/api/")
  }
  tryCatch(do.call(runtime$zotero_config, profile), zotero_error = function(error) stop(error))
}

.zotero_corpus_path <- function(corpus_id) {
  corpus_id <- .zotero_validate_identifier(corpus_id, "corpus_id")
  corpora <- .zotero_read_map("LITREVIEW_ZOTERO_CORPORA_JSON")
  path <- corpora[[corpus_id]]
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !startsWith(path, "/") || grepl("[[:cntrl:]]", path) ||
      grepl("(^|/)\\.\\.(/|$)", path)) {
    .zotero_fail("INVALID_CONFIG", "Corpus não autorizado ou caminho absoluto inválido.")
  }
  path
}

.zotero_operation_fields <- list(
  zotero_status = c("library_profile"),
  zotero_list_collections = c("library_profile", "cursor", "limit"),
  zotero_search_items = c("library_profile", "query", "collection_key", "cursor", "limit"),
  zotero_get_item = c("library_profile", "item_key"),
  litreview_import_collection = c("library_profile", "collection_key", "corpus_id",
                                 "include_pdfs", "dry_run")
)

.zotero_dispatch <- function(runtime, request) {
  if (!is.list(request) || !identical(request$format_version, "1.0.0") ||
      !is.character(request$operation) || length(request$operation) != 1L ||
      !request$operation %in% names(.zotero_operation_fields) ||
      !is.list(request$arguments)) {
    .zotero_fail("INVALID_ARGUMENT", "Pedido não corresponde ao contrato Zotero 1.0.0.")
  }
  operation <- request$operation
  args <- request$arguments
  fields <- .zotero_operation_fields[[operation]]
  if (length(setdiff(names(args), fields))) {
    .zotero_fail("INVALID_ARGUMENT", "O pedido contém argumentos não permitidos.")
  }
  required <- if (operation == "zotero_get_item") c("library_profile", "item_key") else if (
    operation == "litreview_import_collection") c("library_profile", "collection_key", "corpus_id") else
      "library_profile"
  if (length(setdiff(required, names(args)))) {
    .zotero_fail("INVALID_ARGUMENT", "Faltam argumentos obrigatórios no pedido.")
  }

  config <- .zotero_profile_config(runtime, args$library_profile)
  switch(operation,
    zotero_status = runtime$zotero_status(config),
    zotero_list_collections = runtime$zotero_list_collections(
      config, cursor = args$cursor, limit = args$limit %||% 50L
    ),
    zotero_search_items = runtime$zotero_search_items(
      config, query = args$query, collection_key = args$collection_key,
      cursor = args$cursor, limit = args$limit %||% 50L
    ),
    zotero_get_item = runtime$zotero_get_item(config, args$item_key),
    litreview_import_collection = runtime$zotero_import_collection(
      config, collection_key = args$collection_key,
      corpus_dir = .zotero_corpus_path(args$corpus_id),
      include_pdfs = args$include_pdfs %||% FALSE,
      dry_run = args$dry_run %||% TRUE
    )
  )
}

.zotero_safe_details <- function(details) {
  if (!is.list(details)) return(NULL)
  allowed <- intersect(names(details), c("http_status", "retry_after_seconds"))
  details[allowed]
}

.zotero_write_envelope <- function(ok, data = NULL, error = NULL, warnings = character()) {
  warning_array <- I(as.list(as.character(warnings)))
  envelope <- list(
    format_version = "1.0.0",
    ok = isTRUE(ok),
    data = data,
    error = error,
    warnings = warning_array
  )
  json <- jsonlite::toJSON(envelope, auto_unbox = TRUE, null = "null", na = "null")
  cat(as.character(json), "\n")
}

.zotero_handle <- function() {
  input <- paste(readLines(file("stdin"), warn = FALSE), collapse = "\n")
  request <- tryCatch(
    jsonlite::fromJSON(input, simplifyVector = FALSE),
    error = function(error) .zotero_fail("INVALID_ARGUMENT", "O pedido não contém JSON válido.")
  )
  tryCatch({
    runtime <- .zotero_load_functions()
    data <- .zotero_dispatch(runtime, request)
    .zotero_write_envelope(TRUE, data = data)
  }, error = function(error) {
    if (inherits(error, "zotero_error") && error$code %in% .zotero_error_codes) {
      message <- conditionMessage(error)
      retryable <- isTRUE(error$retryable)
      details <- .zotero_safe_details(error$details)
      code <- error$code
    } else {
      message <- "Falha interna ao executar a operação Zotero."
      retryable <- FALSE
      details <- NULL
      code <- "INVALID_RESPONSE"
    }
    .zotero_write_envelope(FALSE, data = NULL,
      error = list(code = code, message = message, retryable = retryable, details = details))
  })
}

suppressPackageStartupMessages({
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cat("{\"format_version\":\"1.0.0\",\"ok\":false,\"data\":null,\"error\":{\"code\":\"BACKEND_UNAVAILABLE\",\"message\":\"Pacote jsonlite não instalado.\",\"retryable\":false,\"details\":null},\"warnings\":[]}\n")
    quit(status = 0L)
  }
})
.zotero_handle()
