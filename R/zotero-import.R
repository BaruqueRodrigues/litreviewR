# Internal helpers for safe, repeatable corpus imports.

.zotero_import_abort <- function(code, message, retryable = FALSE, details = NULL) {
  if (exists(".zotero_abort", mode = "function", inherits = TRUE)) {
    return(.zotero_abort(code, message, retryable = retryable, details = details))
  }
  condition <- structure(
    list(message = message, call = NULL, code = code,
         retryable = retryable, details = details),
    class = c("zotero_error", "error", "condition")
  )
  stop(condition)
}

.zotero_import_time <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
}

.zotero_import_scalar_logical <- function(x, field) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .zotero_import_abort("INVALID_ARGUMENT", sprintf("%s deve ser TRUE ou FALSE.", field))
  }
  x
}

.zotero_import_max_file_bytes <- function(config) {
  limit <- config$max_file_bytes
  if (is.null(limit)) limit <- 100 * 1024^2
  if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) ||
      !is.finite(limit) || limit < 1 || limit %% 1 != 0) {
    .zotero_import_abort("INVALID_CONFIG", "max_file_bytes da configuracao e invalido.")
  }
  as.numeric(limit)
}

.zotero_import_absolute_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    .zotero_import_abort("INVALID_ARGUMENT", "corpus_dir deve ser um caminho nao vazio.")
  }
  path <- path.expand(path)
  if (.Platform$OS.type == "windows") path <- chartr("\\", "/", path)
  if (nchar(path) > 1L) path <- sub("/+$", "", path)
  if (grepl("[[:cntrl:]]", path)) {
    .zotero_import_abort("INVALID_ARGUMENT", "corpus_dir contem caracteres invalidos.")
  }
  if (grepl("(^|/)\\.\\.(/|$)", path)) {
    .zotero_import_abort("INVALID_ARGUMENT", "corpus_dir nao pode conter traversal.")
  }
  absolute <- startsWith(path, "/") ||
    (.Platform$OS.type == "windows" && grepl("^[A-Za-z]:/", path))
  if (!absolute) path <- paste0(getwd(), "/", path)
  prefix <- if (startsWith(path, "/")) "/" else substr(path, 1L, 3L)
  rest <- if (identical(prefix, "/")) substring(path, 2L) else substring(path, 4L)
  parts <- strsplit(rest, "/", fixed = TRUE)[[1L]]
  parts <- parts[nzchar(parts) & parts != "."]
  if (any(parts == "..")) {
    .zotero_import_abort("INVALID_ARGUMENT", "corpus_dir nao pode conter traversal.")
  }
  lexical <- if (!length(parts)) prefix else if (identical(prefix, "/"))
    paste0(prefix, paste(parts, collapse = "/")) else
      paste0(prefix, paste(parts, collapse = "/"))
  if (.zotero_import_is_link(lexical)) {
    .zotero_import_abort("INVALID_ARGUMENT", "corpus_dir nao pode ser um caminho simbolico.")
  }
  if (file.exists(lexical) || dir.exists(lexical)) {
    return(normalizePath(lexical, winslash = "/", mustWork = TRUE))
  }

  ancestor <- lexical
  tail <- character()
  while (!file.exists(ancestor) && !dir.exists(ancestor)) {
    parent <- dirname(ancestor)
    if (identical(parent, ancestor)) break
    tail <- c(basename(ancestor), tail)
    ancestor <- parent
  }
  if (!file.exists(ancestor) && !dir.exists(ancestor)) {
    .zotero_import_abort("INVALID_ARGUMENT", "Nao foi possivel resolver o diretorio pai do corpus.")
  }
  resolved <- normalizePath(ancestor, winslash = "/", mustWork = TRUE)
  if (length(tail)) paste(resolved, paste(tail, collapse = "/"), sep = "/") else resolved
}

.zotero_import_is_link <- function(path) {
  target <- tryCatch(Sys.readlink(path), warning = function(w) "",
                     error = function(e) "")
  is.character(target) && length(target) == 1L && !is.na(target) && nzchar(target)
}

.zotero_import_path_parts <- function(path) {
  if (.Platform$OS.type == "windows" && grepl("^[A-Za-z]:/", path)) {
    prefix <- substr(path, 1L, 3L)
    rest <- substring(path, 4L)
  } else {
    prefix <- "/"
    rest <- substring(path, 2L)
  }
  parts <- strsplit(rest, "/", fixed = TRUE)[[1L]]
  parts <- parts[nzchar(parts)]
  out <- character()
  current <- prefix
  for (part in parts) {
    current <- if (endsWith(current, "/")) paste0(current, part) else paste0(current, "/", part)
    out <- c(out, current)
  }
  out
}

.zotero_import_check_path <- function(path, code = "VERSION_CONFLICT",
                                      leaf = c("any", "directory", "file")) {
  leaf <- match.arg(leaf)
  parts <- .zotero_import_path_parts(path)
  if (!length(parts)) return(invisible(TRUE))
  for (i in seq_along(parts)) {
    current <- parts[[i]]
    if (.zotero_import_is_link(current)) {
      .zotero_import_abort(code, "O corpus contem um caminho simbolico nao permitido.")
    }
    exists <- file.exists(current) || dir.exists(current)
    if (!exists) next
    info <- file.info(current)
    if (is.na(info$isdir)) {
      .zotero_import_abort(code, "Nao foi possivel validar um caminho do corpus.")
    }
    if (i < length(parts) && !isTRUE(info$isdir)) {
      .zotero_import_abort(code, "Um componente do caminho do corpus nao e diretorio.")
    }
    if (i == length(parts) && leaf == "directory" && !isTRUE(info$isdir)) {
      .zotero_import_abort(code, "Um diretorio esperado do corpus e um arquivo.")
    }
    if (i == length(parts) && leaf == "file" && isTRUE(info$isdir)) {
      .zotero_import_abort(code, "Um arquivo esperado do corpus e diretorio.")
    }
  }
  invisible(TRUE)
}

.zotero_import_context <- function(config, item = NULL) {
  .zotero_import_context_value(.zotero_context(config, item = item))
}

.zotero_import_context_value <- function(context) {
  if (!is.list(context) || is.null(context$backend) ||
      is.null(context$library_type)) {
    .zotero_import_abort("INVALID_RESPONSE", "Contexto de biblioteca Zotero invalido.")
  }
  context$backend <- as.character(context$backend)[[1L]]
  context$library_type <- as.character(context$library_type)[[1L]]
  if (!is.null(context$library_id)) {
    context$library_id <- as.character(context$library_id)[[1L]]
  }
  if (!is.null(context$server_id)) context$server_id <- as.character(context$server_id)[[1L]]
  if (!is.null(context$instance_id)) context$instance_id <- as.character(context$instance_id)[[1L]]
  if (!is.null(context$library_id) && identical(context$library_id, "0")) {
    .zotero_import_abort("IDENTITY_UNRESOLVED", "A biblioteca local ainda usa o alias 0.")
  }
  if (is.null(context$library_id) &&
      (is.null(context$server_id) || !nzchar(context$server_id)) &&
      (is.null(context$instance_id) || !nzchar(context$instance_id))) {
    .zotero_import_abort("IDENTITY_UNRESOLVED", "Nao foi possivel resolver a identidade da biblioteca.")
  }
  list(
    backend = context$backend,
    library_type = context$library_type,
    library_id = context$library_id,
    server_id = context$server_id,
    instance_id = context$instance_id
  )
}

.zotero_import_context_available <- function(context) {
  if (!is.list(context)) return(FALSE)
  id <- context$library_id
  if (!is.null(id) && identical(as.character(id)[[1L]], "0")) return(FALSE)
  (!is.null(id) && nzchar(as.character(id)[[1L]])) ||
    (!is.null(context$server_id) && nzchar(as.character(context$server_id)[[1L]])) ||
    (!is.null(context$instance_id) && nzchar(as.character(context$instance_id)[[1L]]))
}

.zotero_import_same_context <- function(a, b) {
  fields <- c("backend", "library_type", "library_id", "server_id", "instance_id")
  all(vapply(fields, function(field) identical(a[[field]], b[[field]]), logical(1)))
}

.zotero_import_key <- function(wrapper, field = "item_key") {
  key <- wrapper$key
  if (is.null(key) && is.list(wrapper$data)) key <- wrapper$data$key
  if (!is.character(key) || length(key) != 1L || is.na(key) ||
      !grepl("^[A-Z0-9]{8}$", key)) {
    .zotero_import_abort("INVALID_RESPONSE", sprintf("Chave Zotero invalida em %s.", field))
  }
  key
}

.zotero_import_item_type <- function(wrapper) {
  type <- wrapper$data$itemType
  if (is.character(type) && length(type) == 1L && !is.na(type)) type else ""
}

.zotero_import_library_identity <- function(wrapper) {
  library <- wrapper$library
  if (is.list(library)) {
    id <- library$id
    type <- library$type
    if (!is.null(id) && as.character(id)[[1L]] == "0") {
      .zotero_import_abort("IDENTITY_UNRESOLVED", "Um item Zotero contem o alias de biblioteca 0.")
    }
    return(list(id = if (is.null(id)) NULL else as.character(id)[[1L]],
                type = if (is.null(type)) NULL else as.character(type)[[1L]]))
  }
  list(id = NULL, type = NULL)
}

.zotero_import_validate_page_context <- function(page_context, item_context) {
  if (!is.list(page_context)) return(invisible(TRUE))
  page_id <- page_context$library_id
  if (!is.null(page_id) && as.character(page_id)[[1L]] != "0" &&
      !identical(as.character(page_id)[[1L]], item_context$library_id)) {
    .zotero_import_abort("VERSION_CONFLICT", "A identidade da biblioteca mudou durante a importacao.")
  }
  page_server <- page_context$server_id
  if (!is.null(page_server) && !is.null(item_context$server_id) &&
      !identical(as.character(page_server)[[1L]], item_context$server_id)) {
    .zotero_import_abort("VERSION_CONFLICT", "A instancia Zotero mudou durante a importacao.")
  }
  for (field in c("backend", "library_type", "instance_id")) {
    page_value <- page_context[[field]]
    item_value <- item_context[[field]]
    if (!is.null(page_value) && !is.null(item_value) &&
        !identical(as.character(page_value)[[1L]], as.character(item_value)[[1L]])) {
      .zotero_import_abort("VERSION_CONFLICT", "O contexto Zotero mudou durante a importacao.")
    }
  }
  invisible(TRUE)
}

.zotero_import_collect <- function(config, collection_key) {
  max_pages <- config$max_pages
  if (is.null(max_pages)) max_pages <- 1000L
  if (!is.numeric(max_pages) || length(max_pages) != 1L || is.na(max_pages) ||
      max_pages < 1 || max_pages %% 1 != 0) {
    .zotero_import_abort("INVALID_CONFIG", "max_pages da configuracao e invalido.")
  }
  limit <- 100L
  cursor <- NULL
  pages <- 0L
  offset <- 0
  all_wrappers <- list()
  all_contexts <- list()
  seen_keys <- character()
  observed_versions <- character()
  unknown_version <- FALSE
  total_seen <- NULL

  repeat {
    if (pages >= max_pages) {
      .zotero_import_abort("LIMIT_EXCEEDED", "A colecao excede o limite de paginas configurado.")
    }
    page <- zotero_search_items(config, collection_key = collection_key,
                                cursor = cursor, limit = limit)
    pages <- pages + 1L
    if (!is.list(page) || !is.list(page$items)) {
      .zotero_import_abort("INVALID_RESPONSE", "Pagina da colecao Zotero invalida.")
    }
    items <- page$items
    if (length(items) > limit) {
      .zotero_import_abort("INVALID_RESPONSE", "A API retornou mais itens que o limite solicitado.")
    }
    if (!is.null(page$total)) {
      if (!is.numeric(page$total) || length(page$total) != 1L || is.na(page$total) ||
          page$total < 0 || page$total %% 1 != 0) {
        .zotero_import_abort("INVALID_RESPONSE", "Total da pagina Zotero invalido.")
      }
      if (is.null(total_seen)) total_seen <- as.numeric(page$total)
      if (!identical(as.numeric(page$total), total_seen)) {
        .zotero_import_abort("VERSION_CONFLICT", "O total da colecao mudou durante a leitura.")
      }
    }
    version <- page$library_version
    if (is.null(version) || length(version) != 1L || is.na(version)) {
      unknown_version <- TRUE
    } else {
      version <- as.character(version)
      if (length(observed_versions) && !identical(version, observed_versions[[1L]])) {
        .zotero_import_abort("VERSION_CONFLICT", "A biblioteca mudou entre paginas da colecao.")
      }
      observed_versions <- c(observed_versions, version)
    }

    page_context <- page$context
    if (.zotero_import_context_available(page_context)) {
      normalized_page_context <- .zotero_import_context_value(page_context)
      if (length(all_contexts) &&
          !.zotero_import_same_context(all_contexts[[1L]], normalized_page_context)) {
        .zotero_import_abort("VERSION_CONFLICT", "O contexto da biblioteca mudou entre paginas.")
      }
      if (!length(all_contexts)) all_contexts[[1L]] <- normalized_page_context
    }

    for (wrapper in items) {
      if (!is.list(wrapper)) {
        .zotero_import_abort("INVALID_RESPONSE", "A colecao contem um item invalido.")
      }
      key <- .zotero_import_key(wrapper)
      if (key %in% seen_keys) {
        .zotero_import_abort("INVALID_RESPONSE", "A paginacao repetiu um item da colecao.")
      }
      seen_keys <- c(seen_keys, key)
      context <- .zotero_import_context(config, wrapper)
      .zotero_import_validate_page_context(page$context, context)
      if (length(all_contexts) && !.zotero_import_same_context(all_contexts[[1L]], context)) {
        .zotero_import_abort("VERSION_CONFLICT", "A colecao contem itens de bibliotecas diferentes.")
      }
      if (!length(all_contexts)) all_contexts[[1L]] <- context
      all_wrappers[[length(all_wrappers) + 1L]] <- wrapper
    }

    next_cursor <- page$next_cursor
    if (is.null(next_cursor)) {
      if (!is.null(total_seen) && offset + limit < total_seen) {
        .zotero_import_abort("INVALID_RESPONSE", "A coleta da colecao terminou antes do total informado.")
      }
      break
    }
    if (!is.character(next_cursor) || length(next_cursor) != 1L || is.na(next_cursor) ||
        !grepl("^[0-9]+$", next_cursor)) {
      .zotero_import_abort("INVALID_RESPONSE", "Cursor Zotero invalido.")
    }
    next_offset <- as.numeric(next_cursor)
    if (!is.finite(next_offset) || next_offset <= offset || next_offset > offset + limit ||
        (!is.null(total_seen) && next_offset >= total_seen)) {
      .zotero_import_abort("INVALID_RESPONSE", "Cursor Zotero nao monotonico ou fora do limite.")
    }
    cursor <- next_cursor
    offset <- next_offset
  }

  if (!length(all_contexts)) {
    page_context <- if (exists("page", inherits = FALSE)) page$context else NULL
    context <- if (.zotero_import_context_available(page_context))
      .zotero_import_context_value(page_context) else .zotero_import_context(config)
    .zotero_import_validate_page_context(page_context, context)
    all_contexts <- list(context)
  }
  list(wrappers = all_wrappers, context = all_contexts[[1L]],
       consistency = if (unknown_version || !length(observed_versions)) "unknown" else "checked",
       library_version = if (length(observed_versions)) observed_versions[[1L]] else NULL,
       pages = pages)
}

.zotero_import_digest <- function(path) {
  tryCatch(
    digest::digest(object = path, algo = "sha256", serialize = FALSE, file = TRUE),
    error = function(e) .zotero_import_abort("WRITE_FAILED", "Nao foi possivel calcular SHA-256.")
  )
}

.zotero_import_document_id <- function(article_id, attachment_key) {
  digest <- digest::digest(object = paste(article_id, attachment_key, sep = "\n"),
                           algo = "sha256", serialize = FALSE)
  paste0("zotero_", digest)
}

.zotero_import_document_path <- function(root, relative_path, code = "VERSION_CONFLICT") {
  if (!is.character(relative_path) || length(relative_path) != 1L || is.na(relative_path) ||
      !startsWith(relative_path, "pdfs/") || grepl("(^|/)\\.\\.(/|$)|^/", relative_path) ||
      grepl("\\\\", relative_path)) {
    .zotero_import_abort(code, "Manifesto contem caminho de documento inseguro.")
  }
  path <- paste0(root, "/", relative_path)
  .zotero_import_check_path(path, code = code, leaf = "file")
  path
}

.zotero_import_absolute_document_path <- function(root, relative_path) {
  paste0(root, "/", relative_path)
}

.zotero_import_read_manifest <- function(root) {
  manifest_path <- paste0(root, "/manifest.json")
  .zotero_import_check_path(manifest_path, code = "VERSION_CONFLICT", leaf = "file")
  if (!file.exists(manifest_path)) return(NULL)
  info <- file.info(manifest_path)
  if (is.na(info$size) || info$size > 50 * 1024^2) {
    .zotero_import_abort("VERSION_CONFLICT", "Manifesto existente invalido ou excessivamente grande.")
  }
  manifest <- tryCatch(
    jsonlite::fromJSON(manifest_path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  required <- c("format_version", "backend", "context", "collection_key",
                "dry_run", "started_at", "finished_at", "consistency",
                "articles", "documents", "results", "counts", "warnings")
  if (!is.list(manifest) || !all(required %in% names(manifest)) ||
      !identical(manifest$format_version, "1.0.0") ||
      !is.list(manifest$context) || !is.list(manifest$articles) ||
      !is.list(manifest$documents) || !is.list(manifest$results) ||
      !is.logical(manifest$dry_run) || length(manifest$dry_run) != 1L ||
      is.na(manifest$dry_run) ||
      !is.character(manifest$consistency) || length(manifest$consistency) != 1L ||
      !manifest$consistency %in% c("checked", "unknown") ||
      !is.character(manifest$started_at) || length(manifest$started_at) != 1L ||
      !is.character(manifest$finished_at) || length(manifest$finished_at) != 1L ||
      !is.list(manifest$counts) || !is.list(manifest$warnings) ||
      .zotero_import_has_alias_zero(manifest$context) ||
      (exists(".has_credential_field", mode = "function", inherits = TRUE) &&
       .has_credential_field(manifest))) {
    .zotero_import_abort("VERSION_CONFLICT", "O manifesto existente e invalido ou incompativel.")
  }
  count_fields <- c("imported", "updated", "unchanged", "failed", "not_available")
  if (!all(count_fields %in% names(manifest$counts)) ||
      any(!vapply(manifest$counts[count_fields], function(value) {
        is.numeric(value) && length(value) == 1L && !is.na(value) && value >= 0 && value %% 1 == 0
      }, logical(1)))) {
    .zotero_import_abort("VERSION_CONFLICT", "O manifesto contem contagens invalidas.")
  }
  article_ids <- character()
  for (article in manifest$articles) {
    if (!is.list(article) || !is.character(article$article_id) ||
        length(article$article_id) != 1L || is.na(article$article_id) ||
        !nzchar(article$article_id) || grepl("^zotero_(user|group)_0_", article$article_id) ||
        !grepl("^(zotero_(user|group)_[1-9][0-9]*_[A-Z0-9]{8}|zotero_local_[[:xdigit:]]{64}_user_[A-Z0-9]{8})$", article$article_id) ||
        .zotero_import_has_alias_zero(article)) {
      .zotero_import_abort("VERSION_CONFLICT", "O manifesto contem artigo invalido ou identidade alias 0.")
    }
    if (article$article_id %in% article_ids) {
      .zotero_import_abort("VERSION_CONFLICT", "O manifesto contem IDs de artigo duplicados.")
    }
    article_ids <- c(article_ids, article$article_id)
  }
  document_ids <- character()
  for (i in seq_along(manifest$documents)) {
    document <- manifest$documents[[i]]
    required_document <- c("document_id", "article_id", "path", "relative_path",
                            "acquisition_status", "extraction_status", "sha256")
    if (!is.list(document) || !all(required_document %in% names(document)) ||
        !is.character(document$document_id) || length(document$document_id) != 1L ||
        is.na(document$document_id) || !nzchar(document$document_id) ||
        !grepl("^zotero_[[:xdigit:]]{64}$", document$document_id) ||
        !is.character(document$article_id) || length(document$article_id) != 1L ||
        is.na(document$article_id) || !document$article_id %in% article_ids ||
        !is.character(document$path) || length(document$path) != 1L || is.na(document$path) ||
        !is.character(document$relative_path) || length(document$relative_path) != 1L || is.na(document$relative_path) ||
        !is.character(document$acquisition_status) || length(document$acquisition_status) != 1L ||
        is.na(document$acquisition_status) ||
        !document$acquisition_status %in% c("pending", "acquired", "failed", "not_available") ||
        !is.character(document$extraction_status) || length(document$extraction_status) != 1L ||
        is.na(document$extraction_status) ||
        !document$extraction_status %in% c("pending", "extracted", "failed", "unreadable", "empty") ||
        (!is.null(document$sha256) &&
         (!is.character(document$sha256) || length(document$sha256) != 1L ||
          is.na(document$sha256) || !grepl("^[[:xdigit:]]{64}$", document$sha256))) ||
        !grepl(paste0("^pdfs/", document$document_id,
                      "(-[[:xdigit:]]{64})?\\.pdf$"), document$relative_path)) {
      .zotero_import_abort("VERSION_CONFLICT", "O manifesto contem documento invalido.")
    }
    if (document$document_id %in% document_ids) {
      .zotero_import_abort("VERSION_CONFLICT", "O manifesto contem IDs de documento duplicados.")
    }
    document_ids <- c(document_ids, document$document_id)
    .zotero_import_document_path(root, document$relative_path)
    document$path <- .zotero_import_absolute_document_path(root, document$relative_path)
    manifest$documents[[i]] <- document
  }
  result_ids <- character()
  for (result in manifest$results) {
    if (!is.list(result) || !is.character(result$article_id) ||
        length(result$article_id) != 1L || is.na(result$article_id) ||
        !result$article_id %in% article_ids ||
        !is.character(result$status) || length(result$status) != 1L || is.na(result$status) ||
        !result$status %in% c("imported", "updated", "unchanged") ||
        !is.list(result$attachment_results)) {
      .zotero_import_abort("VERSION_CONFLICT", "O manifesto contem resultado invalido.")
    }
    if (result$article_id %in% result_ids) {
      .zotero_import_abort("VERSION_CONFLICT", "O manifesto contem resultados duplicados.")
    }
    for (attachment_result in result$attachment_results) {
      if (!is.list(attachment_result) ||
          !is.character(attachment_result$attachment_key) ||
          length(attachment_result$attachment_key) != 1L ||
          !grepl("^[A-Z0-9]{8}$", attachment_result$attachment_key) ||
          !is.character(attachment_result$status) ||
          length(attachment_result$status) != 1L || is.na(attachment_result$status) ||
          !attachment_result$status %in% c("acquired", "failed", "not_available")) {
        .zotero_import_abort("VERSION_CONFLICT", "O manifesto contem resultado de anexo invalido.")
      }
    }
    result_ids <- c(result_ids, result$article_id)
  }
  manifest
}

.zotero_import_has_alias_zero <- function(x, parent = NULL) {
  if (!is.list(x)) {
    return(!is.null(parent) && parent %in% c("library_id", "library.id") &&
             length(x) == 1L && !is.na(x) && identical(as.character(x), "0"))
  }
  nm <- names(x)
  for (i in seq_along(x)) {
    name <- if (!is.null(nm) && nzchar(nm[[i]])) nm[[i]] else ""
    if (name == "library_id" && !is.null(x[[i]]) && length(x[[i]]) == 1L &&
        !is.na(x[[i]]) && identical(as.character(x[[i]]), "0")) return(TRUE)
    if (name == "article_id" && is.character(x[[i]]) && length(x[[i]]) == 1L &&
        !is.na(x[[i]]) && grepl("^zotero_(user|group)_0_", x[[i]])) return(TRUE)
    if (name == "id" && identical(parent, "library") && !is.null(x[[i]]) &&
        length(x[[i]]) == 1L && !is.na(x[[i]]) && identical(as.character(x[[i]]), "0")) return(TRUE)
    if (is.list(x[[i]]) && .zotero_import_has_alias_zero(x[[i]], name)) return(TRUE)
  }
  FALSE
}

.zotero_import_manifest_compatible <- function(old, context, collection_key) {
  if (is.null(old)) return(invisible(TRUE))
  old_context <- old$context
  if (!identical(as.character(old$backend), context$backend) ||
      !.zotero_import_same_context(
        list(backend = as.character(old_context$backend),
             library_type = as.character(old_context$library_type),
             library_id = if (is.null(old_context$library_id)) NULL else as.character(old_context$library_id),
             server_id = if (is.null(old_context$server_id)) NULL else as.character(old_context$server_id),
             instance_id = if (is.null(old_context$instance_id)) NULL else as.character(old_context$instance_id)),
        context) || !identical(as.character(old$collection_key), collection_key)) {
    .zotero_import_abort("VERSION_CONFLICT", "O diretorio pertence a outra biblioteca ou colecao Zotero.")
  }
  invisible(TRUE)
}

.zotero_import_get_articles <- function(config, wrappers, context) {
  articles <- list()
  article_wrappers <- list()
  for (wrapper in wrappers) {
    if (identical(.zotero_import_item_type(wrapper), "attachment") ||
        identical(.zotero_import_item_type(wrapper), "note")) next
    item_context <- .zotero_import_context(config, wrapper)
    if (!.zotero_import_same_context(context, item_context)) {
      .zotero_import_abort("VERSION_CONFLICT", "A biblioteca do item mudou durante a importacao.")
    }
    article <- zotero_normalize_item(wrapper, item_context)
    if (is.null(article)) next
    if (!is.list(article) || !is.character(article$article_id) ||
        length(article$article_id) != 1L || is.na(article$article_id) ||
        !nzchar(article$article_id) ||
        grepl("^zotero_(user|group)_0_", article$article_id)) {
      .zotero_import_abort("IDENTITY_UNRESOLVED", "O item nao tem uma identidade persistente valida.")
    }
    if (!is.character(article$source_key) || length(article$source_key) != 1L ||
        is.na(article$source_key) || !nzchar(article$source_key)) {
      .zotero_import_abort("INVALID_RESPONSE", "A normalizacao nao retornou source_key.")
    }
    raw_key <- .zotero_import_key(wrapper)
    provenance_key <- article$provenance$key
    if (!is.null(provenance_key) &&
        (!is.character(provenance_key) || length(provenance_key) != 1L ||
         is.na(provenance_key) || !identical(provenance_key, raw_key))) {
      .zotero_import_abort("INVALID_RESPONSE", "A proveniencia nao corresponde a chave Zotero original.")
    }
    if (.zotero_import_has_alias_zero(article)) {
      .zotero_import_abort("IDENTITY_UNRESOLVED", "A normalizacao preservou o alias de biblioteca 0.")
    }
    if (any(vapply(articles, function(existing) identical(existing$article_id, article$article_id), logical(1)))) {
      .zotero_import_abort("INVALID_RESPONSE", "A colecao contem IDs de artigo duplicados.")
    }
    articles[[length(articles) + 1L]] <- article
    article_wrappers[[length(article_wrappers) + 1L]] <- wrapper
  }
  list(articles = .zotero_import_duplicate_candidates(articles),
       wrappers = article_wrappers)
}

.zotero_import_metadata <- function(article) {
  content <- article
  content$raw <- NULL
  if (is.list(content$provenance)) content$provenance$version <- NULL
  tryCatch(as.character(jsonlite::toJSON(content, auto_unbox = TRUE,
                                        null = "null", na = "null",
                                        digits = NA, force = TRUE)),
           error = function(e) "<invalid-metadata>")
}

.zotero_import_duplicate_candidates <- function(articles) {
  if (!length(articles)) return(articles)
  normalized <- lapply(articles, function(article) {
    doi <- article$doi
    title <- article$title
    norm_doi <- if (is.character(doi) && length(doi) == 1L && !is.na(doi))
      tolower(gsub("^https?://(dx\\.)?doi\\.org/", "", trimws(doi))) else ""
    norm_title <- if (is.character(title) && length(title) == 1L && !is.na(title))
      tolower(gsub("[[:space:][:punct:]]+", " ", trimws(title))) else ""
    if (nzchar(norm_title)) norm_title <- gsub("[[:space:]]+", " ", norm_title)
    list(doi = norm_doi, title = norm_title)
  })
  for (i in seq_along(articles)) {
    candidates <- as.character(unlist(articles[[i]]$duplicate_candidates %||% character(),
                                      use.names = FALSE))
    if (i > 1L) {
      for (j in seq_len(i - 1L)) {
        duplicate <- (nzchar(normalized[[i]]$doi) &&
                      identical(normalized[[i]]$doi, normalized[[j]]$doi)) ||
          (nzchar(normalized[[i]]$title) &&
           identical(normalized[[i]]$title, normalized[[j]]$title))
        if (!duplicate) next
        previous <- as.character(unlist(articles[[j]]$duplicate_candidates %||% character(),
                                        use.names = FALSE))
        articles[[j]]$duplicate_candidates <- unique(c(previous, articles[[i]]$article_id))
        candidates <- c(candidates, articles[[j]]$article_id)
      }
    }
    articles[[i]]$duplicate_candidates <- unique(candidates)
  }
  articles
}

.zotero_import_existing_by <- function(items, field, value) {
  if (!is.list(items) || !length(items)) return(NULL)
  for (item in items) {
    if (is.list(item) && identical(item[[field]], value)) return(item)
  }
  NULL
}

.zotero_import_article_status <- function(article, old_articles) {
  previous <- .zotero_import_existing_by(old_articles, "article_id", article$article_id)
  if (is.null(previous)) return("imported")
  if (identical(.zotero_import_metadata(previous), .zotero_import_metadata(article))) "unchanged" else "updated"
}

.zotero_import_attachment_children <- function(config, article, parent_wrapper, context) {
  parent_key <- .zotero_import_key(parent_wrapper, "parent item")
  response <- zotero_get_item(config, parent_key)
  if (!is.list(response) || !is.list(response$children)) {
    .zotero_import_abort("INVALID_RESPONSE", "Lista de anexos do item Zotero invalida.")
  }
  response_item <- response$item
  if (is.list(response_item)) {
    response_key <- .zotero_import_key(response_item, "parent response")
    if (!identical(response_key, parent_key)) {
      .zotero_import_abort("VERSION_CONFLICT", "O item pai mudou durante a consulta de anexos.")
    }
    parent_version <- parent_wrapper$version %||% parent_wrapper$data$version
    returned_version <- response_item$version %||% response_item$data$version
    if (!is.null(parent_version) && !is.null(returned_version) &&
        !identical(as.character(parent_version), as.character(returned_version))) {
      .zotero_import_abort("VERSION_CONFLICT", "O item pai mudou durante a consulta de anexos.")
    }
  }
  response_context <- response$context
  if (is.list(response_context)) {
    for (field in c("backend", "library_type", "library_id", "server_id", "instance_id")) {
      response_value <- response_context[[field]]
      expected_value <- context[[field]]
      if (!is.null(response_value) && !is.null(expected_value) &&
          as.character(response_value)[[1L]] != as.character(expected_value)[[1L]] &&
          !(field == "library_id" && as.character(response_value)[[1L]] == "0")) {
        .zotero_import_abort("VERSION_CONFLICT", "A identidade mudou ao consultar os anexos.")
      }
    }
  }

  children <- list()
  for (child in response$children) {
    if (!is.list(child) || !identical(.zotero_import_item_type(child), "attachment")) next
    child_key <- .zotero_import_key(child, "attachment")
    parent_item <- child$data$parentItem
    if (!is.null(parent_item) &&
        (!is.character(parent_item) || length(parent_item) != 1L ||
         is.na(parent_item) || !identical(parent_item, parent_key))) {
      .zotero_import_abort("INVALID_RESPONSE", "Um anexo nao pertence ao artigo consultado.")
    }
    child_context <- .zotero_import_context(config, child)
    if (!.zotero_import_same_context(context, child_context)) {
      .zotero_import_abort("VERSION_CONFLICT", "Um anexo pertence a outra biblioteca Zotero.")
    }
    library_identity <- .zotero_import_library_identity(child)
    parent_identity <- .zotero_import_library_identity(parent_wrapper)
    if ((!is.null(library_identity$id) && !is.null(parent_identity$id) &&
         !identical(library_identity$id, parent_identity$id)) ||
        (!is.null(library_identity$type) && !is.null(parent_identity$type) &&
         !identical(library_identity$type, parent_identity$type))) {
      .zotero_import_abort("VERSION_CONFLICT", "Um anexo pertence a outra biblioteca Zotero.")
    }
    data <- child$data
    is_pdf <- identical(tolower(data$contentType %||% ""), "application/pdf") ||
      (is.character(data$filename) && length(data$filename) == 1L &&
       grepl("\\.pdf$", data$filename, ignore.case = TRUE)) ||
      (is.character(data$title) && length(data$title) == 1L &&
       grepl("\\.pdf$", data$title, ignore.case = TRUE))
    if (is_pdf) children[[length(children) + 1L]] <- child
  }
  children
}

.zotero_import_existing_document <- function(old_documents, document_id) {
  .zotero_import_existing_by(old_documents, "document_id", document_id)
}

.zotero_import_pdf_valid <- function(path) {
  valid <- tryCatch({
    info <- pdftools::pdf_info(path)
    is.list(info) && is.numeric(info$pages) && length(info$pages) == 1L &&
      !is.na(info$pages) && info$pages > 0
  }, error = function(e) FALSE)
  isTRUE(valid)
}

.zotero_import_status_error <- function(error) {
  code <- if (inherits(error, "zotero_error")) error$code else NULL
  if (!is.null(code) && code %in% c("INVALID_CONFIG", "ACCESS_DENIED",
                                    "CONNECTION_UNAVAILABLE", "VERSION_CONFLICT",
                                    "LIMIT_EXCEEDED", "INVALID_RESPONSE",
                                    "IDENTITY_UNRESOLVED", "BACKEND_UNAVAILABLE")) {
    stop(error)
  }
  if (!is.null(code) && code == "NOT_FOUND") {
    return(list(status = "not_available", path = NULL, reason = "attachment_not_found"))
  }
  if (!is.null(code) && code == "ATTACHMENT_UNAVAILABLE") {
    return(list(status = "not_available", path = NULL, reason = "attachment_unavailable"))
  }
  list(status = "failed", path = NULL, reason = "attachment_read_failed")
}

.zotero_import_existing_pdf_hash <- function(path, max_file_bytes) {
  if (!file.exists(path) || dir.exists(path)) return(list(status = "missing", sha256 = NULL))
  info <- file.info(path)
  if (is.na(info$size)) return(list(status = "unreadable", sha256 = NULL))
  if (info$size > max_file_bytes) return(list(status = "oversized", sha256 = NULL))
  hash <- tryCatch(.zotero_import_digest(path), error = function(e) NULL)
  if (is.null(hash)) return(list(status = "unreadable", sha256 = NULL))
  list(status = "hashed", sha256 = tolower(hash))
}

.zotero_import_old_file_state <- function(root, old_document, max_file_bytes) {
  if (is.null(old_document) || !identical(old_document$acquisition_status, "acquired") ||
      is.null(old_document$relative_path) || is.null(old_document$sha256)) return("missing")
  path <- .zotero_import_document_path(root, old_document$relative_path)
  existing <- .zotero_import_existing_pdf_hash(path, max_file_bytes)
  if (identical(existing$status, "oversized")) return("oversized")
  if (!identical(existing$status, "hashed")) return("missing")
  if (identical(existing$sha256, tolower(old_document$sha256))) "valid" else "changed"
}

.zotero_import_old_file_valid <- function(root, old_document, max_file_bytes) {
  identical(.zotero_import_old_file_state(root, old_document, max_file_bytes), "valid")
}

.zotero_import_old_file_preservable <- function(root, old_document, max_file_bytes) {
  .zotero_import_old_file_state(root, old_document, max_file_bytes) %in% c("valid", "oversized")
}

.zotero_import_copy_pdf <- function(source, root, document_id, source_hash,
                                    old_document = NULL, dry_run = FALSE,
                                    max_file_bytes) {
  pdf_dir <- paste0(root, "/pdfs")
  .zotero_import_check_path(pdf_dir, code = "VERSION_CONFLICT", leaf = "directory")
  prior_valid <- .zotero_import_old_file_valid(root, old_document, max_file_bytes)
  if (prior_valid && identical(tolower(old_document$sha256), tolower(source_hash))) {
    return(list(relative_path = old_document$relative_path, copied = FALSE))
  }

  base_name <- paste0(document_id, ".pdf")
  base_path <- paste0(pdf_dir, "/", base_name)
  desired_name <- base_name
  if (!is.null(old_document) && !is.null(old_document$sha256) &&
      !identical(tolower(old_document$sha256), tolower(source_hash))) {
    desired_name <- paste0(document_id, "-", tolower(source_hash), ".pdf")
  }
  destination <- paste0(pdf_dir, "/", desired_name)
  .zotero_import_check_path(destination, code = "VERSION_CONFLICT", leaf = "file")

  if (file.exists(destination)) {
    existing <- .zotero_import_existing_pdf_hash(destination, max_file_bytes)
    if (identical(existing$status, "hashed") &&
        identical(existing$sha256, tolower(source_hash))) {
      return(list(relative_path = paste0("pdfs/", desired_name), copied = FALSE))
    }
    if (identical(destination, base_path)) {
      desired_name <- paste0(document_id, "-", tolower(source_hash), ".pdf")
      destination <- paste0(pdf_dir, "/", desired_name)
      .zotero_import_check_path(destination, code = "VERSION_CONFLICT", leaf = "file")
      if (file.exists(destination)) {
        existing <- .zotero_import_existing_pdf_hash(destination, max_file_bytes)
        if (identical(existing$status, "hashed") &&
            identical(existing$sha256, tolower(source_hash))) {
          return(list(relative_path = paste0("pdfs/", desired_name), copied = FALSE))
        }
        .zotero_import_abort(if (identical(existing$status, "oversized")) "WRITE_FAILED" else "VERSION_CONFLICT",
                             "Um arquivo PDF versionado possui conteudo inesperado.")
      }
    } else {
      .zotero_import_abort(if (identical(existing$status, "oversized")) "WRITE_FAILED" else "VERSION_CONFLICT",
                           "Um arquivo PDF versionado possui conteudo inesperado.")
    }
  }
  if (isTRUE(dry_run)) return(list(relative_path = paste0("pdfs/", desired_name), copied = FALSE))

  tmp <- tempfile(pattern = ".zotero-pdf-", tmpdir = pdf_dir)
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  copied <- tryCatch(file.copy(source, tmp, overwrite = FALSE), error = function(e) FALSE)
  temp_info <- if (file.exists(tmp)) file.info(tmp) else NULL
  if (!isTRUE(copied) || !file.exists(tmp) || is.null(temp_info) ||
      is.na(temp_info$size) || temp_info$size > max_file_bytes ||
      !identical(tolower(.zotero_import_digest(tmp)), tolower(source_hash)) ||
      !.zotero_import_pdf_valid(tmp)) {
    .zotero_import_abort("WRITE_FAILED", "Nao foi possivel copiar e validar o PDF.")
  }
  .zotero_import_check_path(destination, code = "VERSION_CONFLICT", leaf = "file")
  renamed <- tryCatch(file.rename(tmp, destination), warning = function(w) FALSE,
                      error = function(e) FALSE)
  if (!isTRUE(renamed)) {
    existing <- .zotero_import_existing_pdf_hash(destination, max_file_bytes)
    if (identical(existing$status, "hashed") &&
        identical(existing$sha256, tolower(source_hash))) {
      return(list(relative_path = paste0("pdfs/", desired_name), copied = FALSE))
    }
    .zotero_import_abort("WRITE_FAILED", "Nao foi possivel publicar o arquivo PDF.")
  }
  list(relative_path = paste0("pdfs/", desired_name), copied = TRUE)
}

.zotero_import_acquire_attachment <- function(config, article, parent_wrapper,
                                              child, context, root, old_documents,
                                              dry_run, max_file_bytes) {
  attachment_key <- .zotero_import_key(child, "attachment")
  document_id <- .zotero_import_document_id(article$article_id, attachment_key)
  old_document <- .zotero_import_existing_document(old_documents, document_id)
  source <- tryCatch(.zotero_attachment_source(config, attachment_key),
                     error = function(e) .zotero_import_status_error(e))
  if (!is.list(source) || !source$status %in% c("available", "not_available", "failed")) {
    source <- list(status = "failed", path = NULL, reason = "invalid_attachment_source")
  }
  if (!identical(source$status, "available")) {
    if (.zotero_import_old_file_preservable(root, old_document, max_file_bytes)) {
      document <- old_document
    } else {
      document <- list(
        document_id = document_id,
        article_id = article$article_id,
        path = .zotero_import_absolute_document_path(root, paste0("pdfs/", document_id, ".pdf")),
        relative_path = paste0("pdfs/", document_id, ".pdf"),
        acquisition_status = source$status,
        extraction_status = "pending",
        sha256 = NULL,
        source = "zotero",
        zotero_key = attachment_key,
        zotero_version = child$version %||% child$data$version %||% NULL
      )
    }
    return(list(document = document,
                result = list(attachment_key = attachment_key,
                              document_id = document_id,
                              status = source$status,
                              reason = source$reason %||% NULL)))
  }

  source_path <- source$path
  if (!is.character(source_path) || length(source_path) != 1L || is.na(source_path) ||
      !file.exists(source_path) || dir.exists(source_path) ||
      file.access(source_path, 4L) != 0L) {
    document <- if (.zotero_import_old_file_preservable(root, old_document, max_file_bytes)) old_document else
      list(document_id = document_id, article_id = article$article_id,
           path = .zotero_import_absolute_document_path(root, paste0("pdfs/", document_id, ".pdf")),
           relative_path = paste0("pdfs/", document_id, ".pdf"),
           acquisition_status = "not_available", extraction_status = "pending",
           sha256 = NULL, source = "zotero", zotero_key = attachment_key,
           zotero_version = child$version %||% child$data$version %||% NULL)
    return(list(document = document,
                result = list(attachment_key = attachment_key,
                              document_id = document_id,
                              status = "not_available",
                              reason = "local_file_unavailable")))
  }
  source_info <- file.info(source_path)
  if (is.na(source_info$size) || source_info$size > max_file_bytes) {
    document <- if (.zotero_import_old_file_preservable(root, old_document, max_file_bytes)) old_document else
      list(document_id = document_id, article_id = article$article_id,
           path = .zotero_import_absolute_document_path(root, paste0("pdfs/", document_id, ".pdf")),
           relative_path = paste0("pdfs/", document_id, ".pdf"),
           acquisition_status = "failed", extraction_status = "pending",
           sha256 = NULL, source = "zotero", zotero_key = attachment_key,
           zotero_version = child$version %||% child$data$version %||% NULL)
    return(list(document = document,
                result = list(attachment_key = attachment_key,
                              document_id = document_id, status = "failed",
                              reason = "file_too_large")))
  }
  if (!.zotero_import_pdf_valid(source_path)) {
    document <- if (.zotero_import_old_file_preservable(root, old_document, max_file_bytes)) old_document else
      list(document_id = document_id, article_id = article$article_id,
           path = .zotero_import_absolute_document_path(root, paste0("pdfs/", document_id, ".pdf")),
           relative_path = paste0("pdfs/", document_id, ".pdf"),
           acquisition_status = "failed", extraction_status = "pending",
           sha256 = NULL, source = "zotero", zotero_key = attachment_key,
           zotero_version = child$version %||% child$data$version %||% NULL)
    return(list(document = document,
                result = list(attachment_key = attachment_key,
                              document_id = document_id, status = "failed",
                              reason = "invalid_pdf")))
  }
  source_hash <- tryCatch(.zotero_import_digest(source_path), error = function(e) NULL)
  if (is.null(source_hash)) {
    document <- if (.zotero_import_old_file_preservable(root, old_document, max_file_bytes)) old_document else
      list(document_id = document_id, article_id = article$article_id,
           path = .zotero_import_absolute_document_path(root, paste0("pdfs/", document_id, ".pdf")),
           relative_path = paste0("pdfs/", document_id, ".pdf"),
           acquisition_status = "failed", extraction_status = "pending",
           sha256 = NULL, source = "zotero", zotero_key = attachment_key,
           zotero_version = child$version %||% child$data$version %||% NULL)
    return(list(document = document,
                result = list(attachment_key = attachment_key,
                              document_id = document_id,
                              status = "failed",
                              reason = "source_hash_failed")))
  }
  target_root <- .zotero_import_absolute_path(root)
  source_real <- normalizePath(source_path, winslash = "/", mustWork = TRUE)
  base_target <- paste0(target_root, "/pdfs/", document_id, ".pdf")
  version_target <- paste0(target_root, "/pdfs/", document_id, "-", tolower(source_hash), ".pdf")
  if (identical(source_real, normalizePath(base_target, winslash = "/", mustWork = FALSE)) ||
      identical(source_real, normalizePath(version_target, winslash = "/", mustWork = FALSE))) {
    .zotero_import_abort("INVALID_ARGUMENT", "O arquivo de origem nao pode ser o destino do corpus.")
  }
  copied <- tryCatch(
    .zotero_import_copy_pdf(source_path, root, document_id, source_hash,
                            old_document = old_document, dry_run = dry_run,
                            max_file_bytes = max_file_bytes),
    error = function(e) {
      if (inherits(e, "zotero_error") &&
          !identical(e$code, "WRITE_FAILED")) stop(e)
      NULL
    }
  )
  if (is.null(copied)) {
    document <- if (.zotero_import_old_file_preservable(root, old_document, max_file_bytes)) old_document else
      list(document_id = document_id, article_id = article$article_id,
           path = .zotero_import_absolute_document_path(root, paste0("pdfs/", document_id, ".pdf")),
           relative_path = paste0("pdfs/", document_id, ".pdf"),
           acquisition_status = "failed", extraction_status = "pending",
           sha256 = NULL, source = "zotero", zotero_key = attachment_key,
           zotero_version = child$version %||% child$data$version %||% NULL)
    return(list(document = document,
                result = list(attachment_key = attachment_key,
                              document_id = document_id,
                              status = "failed",
                              reason = "copy_failed")))
  }
  document <- list(
    document_id = document_id,
    article_id = article$article_id,
    path = .zotero_import_absolute_document_path(root, copied$relative_path),
    relative_path = copied$relative_path,
    acquisition_status = "acquired",
    extraction_status = "pending",
    sha256 = tolower(source_hash),
    source = "zotero",
    zotero_key = attachment_key,
    zotero_version = child$version %||% child$data$version %||% NULL
  )
  list(document = document,
       result = list(attachment_key = attachment_key,
                     document_id = document_id,
                     status = "acquired",
                     sha256 = tolower(source_hash),
                     path = .zotero_import_absolute_document_path(root, copied$relative_path)))
}

.zotero_import_acquire_articles <- function(config, current_articles, current_wrappers,
                                            context, root, old_manifest,
                                            dry_run, include_pdfs, max_file_bytes) {
  old_articles <- old_manifest$articles %||% list()
  old_documents <- old_manifest$documents %||% list()
  old_results <- old_manifest$results %||% list()
  articles <- old_articles
  documents <- old_documents
  results <- old_results
  current_results <- list()
  counts <- list(imported = 0L, updated = 0L, unchanged = 0L,
                 failed = 0L, not_available = 0L)

  for (i in seq_along(current_articles)) {
    article <- current_articles[[i]]
    wrapper <- current_wrappers[[i]]
    status <- .zotero_import_article_status(article, old_articles)
    counts[[status]] <- counts[[status]] + 1L
    article_pos <- which(vapply(articles, function(existing) {
      is.list(existing) && identical(existing$article_id, article$article_id)
    }, logical(1)))
    if (length(article_pos)) articles[[article_pos[[1L]]]] <- article else
      articles[[length(articles) + 1L]] <- article

    previous_result <- .zotero_import_existing_by(old_results, "article_id", article$article_id)
    attachment_results <- if (include_pdfs) list() else
      (previous_result$attachment_results %||% list())
    if (include_pdfs) {
      children <- .zotero_import_attachment_children(config, article, wrapper, context)
      for (child in children) {
        acquired <- .zotero_import_acquire_attachment(
          config, article, wrapper, child, context, root, documents,
          dry_run, max_file_bytes
        )
        doc_pos <- which(vapply(documents, function(existing) {
          is.list(existing) && identical(existing$document_id, acquired$document$document_id)
        }, logical(1)))
        if (length(doc_pos)) documents[[doc_pos[[1L]]]] <- acquired$document else
          documents[[length(documents) + 1L]] <- acquired$document
        attachment_results[[length(attachment_results) + 1L]] <- acquired$result
        if (identical(acquired$result$status, "failed")) counts$failed <- counts$failed + 1L
        if (identical(acquired$result$status, "not_available")) counts$not_available <- counts$not_available + 1L
      }
    }
    result <- list(article_id = article$article_id, status = status,
                   attachment_results = attachment_results)
    current_results[[length(current_results) + 1L]] <- result
  }

  for (result in current_results) {
    pos <- which(vapply(results, function(existing) {
      is.list(existing) && identical(existing$article_id, result$article_id)
    }, logical(1)))
    if (length(pos)) results[[pos[[1L]]]] <- result else
      results[[length(results) + 1L]] <- result
  }
  list(articles = articles, documents = documents, results = results, counts = counts)
}

.zotero_import_acquire_lock <- function(root) {
  lock <- paste0(root, "/.zotero-import.lock")
  .zotero_import_check_path(lock, code = "CORPUS_LOCKED", leaf = "directory")
  created <- tryCatch(dir.create(lock, showWarnings = FALSE, recursive = FALSE),
                      warning = function(w) FALSE, error = function(e) FALSE)
  if (!isTRUE(created)) {
    .zotero_import_abort("CORPUS_LOCKED", "Ja existe uma importacao ativa para este corpus.")
  }
  tryCatch(Sys.chmod(lock, mode = "0700"), warning = function(w) NULL,
           error = function(e) NULL)
  token <- paste(Sys.getpid(), basename(tempfile("owner-")), format(Sys.time(), "%s"), sep = "-")
  owner <- paste0(lock, "/.owner")
  wrote <- tryCatch({
    writeLines(token, owner, useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE)
  owner_matches <- file.exists(owner) && !.zotero_import_is_link(owner) &&
    identical(tryCatch(readLines(owner, warn = FALSE), error = function(e) character()), token)
  if (!isTRUE(wrote) || !owner_matches) {
    if (dir.exists(lock) && !.zotero_import_is_link(lock) && !.zotero_import_is_link(owner)) {
      if (file.exists(owner)) unlink(owner)
      unlink(lock, recursive = TRUE)
    }
    .zotero_import_abort("CORPUS_LOCKED", "Nao foi possivel registrar a posse da trava do corpus.")
  }
  list(path = lock, owner = owner, token = token, acquired = TRUE)
}

.zotero_import_release_lock <- function(lock) {
  if (!is.list(lock) || !isTRUE(lock$acquired) || .zotero_import_is_link(lock$path) ||
      .zotero_import_is_link(lock$owner) || !file.exists(lock$owner)) return(invisible(FALSE))
  token <- tryCatch(readLines(lock$owner, warn = FALSE), error = function(e) character())
  if (length(token) != 1L || !identical(token, lock$token)) return(invisible(FALSE))
  if (!isTRUE(file.remove(lock$owner))) return(invisible(FALSE))
  invisible(unlink(lock$path, recursive = TRUE) == 0L)
}

.zotero_import_write_manifest <- function(manifest, root) {
  path <- paste0(root, "/manifest.json")
  .zotero_import_check_path(path, code = "VERSION_CONFLICT", leaf = "file")
  temp <- tempfile(pattern = ".zotero-manifest-", tmpdir = root)
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  json <- tryCatch(jsonlite::toJSON(manifest, auto_unbox = TRUE, null = "null",
                                    na = "null", digits = NA, pretty = TRUE,
                                    force = TRUE),
                   error = function(e) NULL)
  if (is.null(json) || length(json) != 1L) {
    .zotero_import_abort("WRITE_FAILED", "Nao foi possivel serializar o manifesto Zotero.")
  }
  wrote <- tryCatch({
    writeLines(as.character(json), temp, useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(wrote) || !file.exists(temp) || file.info(temp)$size <= 0) {
    .zotero_import_abort("WRITE_FAILED", "Nao foi possivel gravar o manifesto temporario.")
  }
  validated <- tryCatch(jsonlite::fromJSON(temp, simplifyVector = FALSE),
                        error = function(e) NULL)
  if (!is.list(validated) || !identical(validated$format_version, "1.0.0") ||
      !identical(validated$collection_key, manifest$collection_key)) {
    .zotero_import_abort("WRITE_FAILED", "O manifesto temporario nao passou na validacao.")
  }
  renamed <- tryCatch(file.rename(temp, path), warning = function(w) FALSE,
                      error = function(e) FALSE)
  if (!isTRUE(renamed)) {
    .zotero_import_abort("WRITE_FAILED", "Nao foi possivel publicar o manifesto.")
  }
  invisible(path)
}

#' Import Zotero collection into a local corpus
#'
#' Reads one Zotero collection and plans or performs a local corpus update.
#' Metadata-only imports never retrieve attachment children. PDF acquisition is
#' limited to local Zotero file URLs, validates each PDF, and keeps prior
#' documents when an attachment disappears. A complete collection snapshot is
#' required before a manifest is published.
#'
#' @param config A Zotero configuration created by zotero_config().
#' @param collection_key Eight-character Zotero collection key.
#' @param corpus_dir Local destination directory for the manifest and PDFs.
#' @param include_pdfs Whether to retrieve and copy PDF attachments.
#' @param dry_run When TRUE, return the planned manifest without writing files.
#' @return A manifest list with format version, identity, articles, documents,
#'   per-article results, counts, and consistency status.
#' @export
zotero_import_collection <- function(config, collection_key, corpus_dir,
                                     include_pdfs = FALSE, dry_run = TRUE) {
  include_pdfs <- .zotero_import_scalar_logical(include_pdfs, "include_pdfs")
  dry_run <- .zotero_import_scalar_logical(dry_run, "dry_run")
  .zotero_validate_key(collection_key, field = "collection_key")
  if (!inherits(config, "zotero_config")) {
    .zotero_import_abort("INVALID_CONFIG", "config deve ser um objeto zotero_config.")
  }
  root <- .zotero_import_absolute_path(corpus_dir)
  max_file_bytes <- .zotero_import_max_file_bytes(config)
  .zotero_import_check_path(root, code = "INVALID_ARGUMENT", leaf = "directory")
  .zotero_import_check_path(paste0(root, "/manifest.json"),
                            code = "VERSION_CONFLICT", leaf = "file")
  .zotero_import_check_path(paste0(root, "/pdfs"),
                            code = "VERSION_CONFLICT", leaf = "directory")
  lock_path <- paste0(root, "/.zotero-import.lock")
  if (.zotero_import_is_link(lock_path)) {
    .zotero_import_abort("CORPUS_LOCKED", "A trava do corpus e um caminho simbolico.")
  }

  collection <- .zotero_import_collect(config, collection_key)
  current <- .zotero_import_get_articles(config, collection$wrappers, collection$context)

  if (dry_run) {
    old_manifest <- .zotero_import_read_manifest(root)
    .zotero_import_manifest_compatible(old_manifest, collection$context, collection_key)
    plan <- .zotero_import_acquire_articles(
      config, current$articles, current$wrappers, collection$context, root,
      old_manifest, dry_run = TRUE, include_pdfs = include_pdfs,
      max_file_bytes = max_file_bytes
    )
    now <- .zotero_import_time()
    return(list(format_version = "1.0.0", backend = collection$context$backend,
                context = collection$context, collection_key = collection_key,
                dry_run = TRUE, started_at = now, finished_at = now,
                consistency = collection$consistency,
                articles = plan$articles, documents = plan$documents,
                results = plan$results, counts = plan$counts, warnings = list()))
  }

  if (!dir.exists(root)) {
    made <- tryCatch(dir.create(root, recursive = TRUE, showWarnings = FALSE),
                     warning = function(w) FALSE, error = function(e) FALSE)
    if (!isTRUE(made) && !dir.exists(root)) {
      .zotero_import_abort("WRITE_FAILED", "Nao foi possivel criar o diretorio do corpus.")
    }
  }
  .zotero_import_check_path(root, code = "VERSION_CONFLICT", leaf = "directory")
  lock <- .zotero_import_acquire_lock(root)
  on.exit(.zotero_import_release_lock(lock), add = TRUE)

  old_manifest <- .zotero_import_read_manifest(root)
  .zotero_import_manifest_compatible(old_manifest, collection$context, collection_key)
  pdf_dir <- paste0(root, "/pdfs")
  .zotero_import_check_path(pdf_dir, code = "VERSION_CONFLICT", leaf = "directory")
  if (!dir.exists(pdf_dir)) {
    made <- tryCatch(dir.create(pdf_dir, showWarnings = FALSE, recursive = FALSE),
                     warning = function(w) FALSE, error = function(e) FALSE)
    if (!isTRUE(made) && !dir.exists(pdf_dir)) {
      .zotero_import_abort("WRITE_FAILED", "Nao foi possivel criar o diretorio de PDFs.")
    }
  }
  started_at <- .zotero_import_time()
  plan <- .zotero_import_acquire_articles(
    config, current$articles, current$wrappers, collection$context, root,
    old_manifest, dry_run = FALSE, include_pdfs = include_pdfs,
    max_file_bytes = max_file_bytes
  )
  manifest <- list(format_version = "1.0.0", backend = collection$context$backend,
                   context = collection$context, collection_key = collection_key,
                   dry_run = FALSE, started_at = started_at,
                   finished_at = .zotero_import_time(),
                   consistency = collection$consistency,
                   articles = plan$articles, documents = plan$documents,
                   results = plan$results, counts = plan$counts, warnings = list())
  .zotero_import_write_manifest(manifest, root)
  manifest
}
