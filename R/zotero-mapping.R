#' Normalize a Zotero item to a litreviewR article record
#'
#' Zotero notes and attachments are omitted. Bibliographic items retain their
#' original wrapper in `raw`; creator records are preserved without converting
#' institutional names into personal names. Local user libraries configured
#' with the Zotero alias `0` are resolved from the item's original library
#' metadata or receive an instance-scoped ID when a stable instance identifier
#' is available.
#'
#' @param item Zotero item wrapper containing `key`, `version`, `library`, and
#'   `data` fields.
#' @param context List containing `backend`, `library_type`, `library_id`,
#'   `server_id`, and `instance_id`, as returned by the Zotero client.
#' @return `NULL` for a note or attachment; otherwise a normalized article
#'   record with Zotero identity, metadata, provenance, and the unmodified
#'   source wrapper.
#' @export
zotero_normalize_item <- function(item, context) {
  .zotero_mapping_validate_wrapper(item)
  context <- .zotero_mapping_validate_context(context)

  data <- item$data
  item_type <- data$itemType
  if (item_type %in% c("attachment", "note")) return(NULL)

  key <- item$key
  library <- .zotero_mapping_resolve_library(item, context)
  identity <- .zotero_mapping_identity(key, context, library$library_id)

  creators <- data$creators
  if (is.null(creators)) creators <- list()
  if (!is.list(creators)) {
    .zotero_mapping_abort("INVALID_RESPONSE", "A lista de creators do item Zotero \u00e9 inv\u00e1lida.")
  }

  date_original <- .zotero_mapping_optional_text(data$date, "date")
  doi_original <- .zotero_mapping_optional_text(data$DOI, "DOI")
  doi <- if (is.null(doi_original)) NULL else normaliza_doi(doi_original)
  if (identical(doi, "")) doi <- NULL

  list(
    article_id = identity,
    id = identity,
    id_source = "zotero",
    source_key = paste0("zotero:", key),
    title = .zotero_mapping_optional_text(data$title, "title"),
    author = creators,
    date_original = date_original,
    year = .zotero_mapping_year(date_original),
    doi = doi,
    journal = .zotero_mapping_optional_text(data$publicationTitle, "publicationTitle"),
    abstract = .zotero_mapping_optional_text(data$abstractNote, "abstractNote"),
    url = .zotero_mapping_optional_text(data$url, "url"),
    entry_type = item_type,
    tags = .zotero_mapping_tags(data$tags),
    collections = .zotero_mapping_collections(data$collections),
    duplicate_candidates = character(),
    provenance = list(
      backend = context$backend,
      library = list(type = context$library_type, id = library$library_id),
      key = key,
      version = item$version,
      server_id = context$server_id,
      instance_id = context$instance_id
    ),
    raw = item
  )
}

.zotero_mapping_abort <- function(code, message) {
  .zotero_abort(code, message, retryable = FALSE, details = NULL)
}

.zotero_mapping_validate_wrapper <- function(item) {
  if (!is.list(item) || is.null(names(item))) {
    .zotero_mapping_abort("INVALID_ARGUMENT", "`item` deve ser um wrapper Zotero nomeado.")
  }
  required <- c("key", "version", "library", "data")
  if (length(setdiff(required, names(item)))) {
    .zotero_mapping_abort("INVALID_RESPONSE", "O wrapper do item Zotero est\u00e1 incompleto.")
  }
  if (!is.character(item$key) || length(item$key) != 1L || is.na(item$key) ||
      !grepl("^[A-Z0-9]{8}$", item$key)) {
    .zotero_mapping_abort("INVALID_RESPONSE", "A chave do item Zotero \u00e9 inv\u00e1lida.")
  }
  if (!is.numeric(item$version) || length(item$version) != 1L ||
      is.na(item$version) || !is.finite(item$version) || item$version < 0 ||
      item$version %% 1 != 0) {
    .zotero_mapping_abort("INVALID_RESPONSE", "A vers\u00e3o do item Zotero \u00e9 inv\u00e1lida.")
  }
  if (!is.list(item$library) || is.null(names(item$library)) ||
      !is.list(item$data) || is.null(names(item$data))) {
    .zotero_mapping_abort("INVALID_RESPONSE", "A biblioteca ou os dados do item Zotero s\u00e3o inv\u00e1lidos.")
  }
  if (is.null(item$data$itemType) || !is.character(item$data$itemType) ||
      length(item$data$itemType) != 1L || is.na(item$data$itemType) ||
      !nzchar(trimws(item$data$itemType))) {
    .zotero_mapping_abort("INVALID_RESPONSE", "O tipo do item Zotero \u00e9 inv\u00e1lido.")
  }
  if (!is.null(item$data$key) && !identical(item$data$key, item$key)) {
    .zotero_mapping_abort("INVALID_RESPONSE", "A chave no wrapper e nos dados do item Zotero diverge.")
  }
  if (!is.null(item$data$version)) {
    data_version <- item$data$version
    if (!is.numeric(data_version) || length(data_version) != 1L || is.na(data_version) ||
        !is.finite(data_version) || data_version < 0 || data_version %% 1 != 0 ||
        !identical(as.numeric(data_version), as.numeric(item$version))) {
      .zotero_mapping_abort("INVALID_RESPONSE", "A vers\u00e3o no wrapper e nos dados do item Zotero diverge.")
    }
  }
  invisible(item)
}

.zotero_mapping_validate_context <- function(context) {
  if (!is.list(context) || is.null(names(context))) {
    .zotero_mapping_abort("INVALID_ARGUMENT", "`context` deve ser uma lista nomeada.")
  }
  required <- c("backend", "library_type", "library_id", "server_id", "instance_id")
  if (length(setdiff(required, names(context)))) {
    .zotero_mapping_abort("INVALID_ARGUMENT", "`context` n\u00e3o cont\u00e9m os campos exigidos pelo contrato Zotero.")
  }
  if (!is.character(context$backend) || length(context$backend) != 1L ||
      is.na(context$backend) || !context$backend %in% c("local", "web")) {
    .zotero_mapping_abort("INVALID_ARGUMENT", "O backend do contexto Zotero \u00e9 inv\u00e1lido.")
  }
  if (!is.character(context$library_type) || length(context$library_type) != 1L ||
      is.na(context$library_type) || !context$library_type %in% c("user", "group")) {
    .zotero_mapping_abort("INVALID_ARGUMENT", "O tipo de biblioteca do contexto Zotero \u00e9 inv\u00e1lido.")
  }

  library_id <- context$library_id
  if (!is.null(library_id)) {
    if (!is.character(library_id) || length(library_id) != 1L || is.na(library_id) ||
        !grepl("^[0-9]+$", library_id)) {
      .zotero_mapping_abort("INVALID_ARGUMENT", "O ID da biblioteca deve ser uma string decimal ou NULL.")
    }
    library_id <- .zotero_mapping_library_id(library_id)
    if (length(library_id) != 1L || is.na(library_id)) {
      .zotero_mapping_abort("INVALID_ARGUMENT", "O ID da biblioteca deve ser uma string decimal ou NULL.")
    }
    context$library_id <- library_id
    if (identical(library_id, "0") &&
        !(identical(context$backend, "local") && identical(context$library_type, "user"))) {
      .zotero_mapping_abort("INVALID_ARGUMENT", "O alias de biblioteca `0` s\u00f3 vale para usu\u00e1rio local.")
    }
  }
  for (field in c("server_id", "instance_id")) {
    value <- context[[field]]
    if (!is.null(value) && (!is.character(value) || length(value) != 1L ||
                            is.na(value) || !nzchar(value))) {
      .zotero_mapping_abort("INVALID_ARGUMENT", sprintf("`context$%s` deve ser uma string n\u00e3o vazia ou NULL.", field))
    }
  }
  context
}

.zotero_mapping_library_id <- function(value) {
  if (is.null(value)) return(NULL)
  if (is.character(value)) {
    if (length(value) != 1L || is.na(value) || !grepl("^[0-9]+$", value)) return(NA_character_)
    value <- sub("^0+([0-9])", "\\1", value)
    return(value)
  }
  if (is.numeric(value) && length(value) == 1L && !is.na(value) &&
      is.finite(value) && value >= 0 && value %% 1 == 0) {
    return(format(value, scientific = FALSE, trim = TRUE, digits = 22))
  }
  NA_character_
}

.zotero_mapping_resolve_library <- function(item, context) {
  item_library <- item$library
  if (!is.character(item_library$type) || length(item_library$type) != 1L ||
      is.na(item_library$type) || !item_library$type %in% c("user", "group") ||
      !identical(item_library$type, context$library_type)) {
    .zotero_mapping_abort("INVALID_RESPONSE", "O tipo de biblioteca do item n\u00e3o corresponde ao contexto Zotero.")
  }
  item_library_id <- .zotero_mapping_library_id(item_library$id)
  if (length(item_library_id) && is.na(item_library_id)) {
    .zotero_mapping_abort("INVALID_RESPONSE", "O ID da biblioteca no wrapper Zotero \u00e9 inv\u00e1lido.")
  }
  context_library_id <- context$library_id
  if (identical(context_library_id, "0")) context_library_id <- NULL
  if (identical(item_library_id, "0") &&
      !(identical(context$backend, "local") && identical(context$library_type, "user"))) {
    .zotero_mapping_abort("INVALID_RESPONSE", "O wrapper usa um alias de biblioteca incompat\u00edvel.")
  }
  if (!is.null(item_library_id) && item_library_id != "0" &&
      !is.null(context_library_id) && item_library_id != context_library_id) {
    .zotero_mapping_abort("VERSION_CONFLICT", "A biblioteca do item diverge da biblioteca do contexto Zotero.")
  }
  resolved <- if (!is.null(item_library_id) && item_library_id != "0") {
    item_library_id
  } else {
    context_library_id
  }
  if (!is.null(resolved) && identical(resolved, "0")) resolved <- NULL
  list(library_id = resolved)
}

.zotero_mapping_identity <- function(key, context, library_id) {
  if (!is.null(library_id) && nzchar(library_id) && library_id != "0") {
    return(paste("zotero", context$library_type, library_id, key, sep = "_"))
  }
  if (identical(context$backend, "local") && identical(context$library_type, "user")) {
    instance_source <- context$server_id
    if (is.null(instance_source)) instance_source <- context$instance_id
    if (!is.null(instance_source)) {
      instance_hash <- digest::digest(instance_source, algo = "sha256", serialize = FALSE)
      return(paste("zotero", "local", instance_hash, "user", key, sep = "_"))
    }
  }
  .zotero_mapping_abort("IDENTITY_UNRESOLVED", "N\u00e3o foi poss\u00edvel resolver uma identidade Zotero est\u00e1vel para o item.")
}

.zotero_mapping_optional_text <- function(value, field) {
  if (is.null(value)) return(NULL)
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    .zotero_mapping_abort("INVALID_RESPONSE", sprintf("O campo `%s` do item Zotero deve ser texto ou NULL.", field))
  }
  if (!nzchar(trimws(value))) NULL else value
}

.zotero_mapping_year <- function(date) {
  if (is.null(date) || !nzchar(trimws(date))) return(NULL)
  matches <- regmatches(date, gregexpr("(?<![0-9])[0-9]{4}(?![0-9])", date, perl = TRUE))[[1L]]
  if (identical(matches, character(0)) || length(matches) != 1L) return(NULL)
  matches[[1L]]
}

.zotero_mapping_tags <- function(tags) {
  if (is.null(tags)) return(list())
  if (is.character(tags)) tags <- as.list(tags)
  if (!is.list(tags)) {
    .zotero_mapping_abort("INVALID_RESPONSE", "A lista de tags do item Zotero \u00e9 inv\u00e1lida.")
  }
  lapply(tags, function(tag) {
    if (is.character(tag) && length(tag) == 1L && !is.na(tag)) return(tag)
    if (is.list(tag) && !is.null(tag$tag) && is.character(tag$tag) &&
        length(tag$tag) == 1L && !is.na(tag$tag)) return(tag$tag)
    .zotero_mapping_abort("INVALID_RESPONSE", "Uma tag do item Zotero \u00e9 inv\u00e1lida.")
  })
}

.zotero_mapping_collections <- function(collections) {
  if (is.null(collections)) return(list())
  if (is.character(collections)) collections <- as.list(collections)
  if (!is.list(collections)) {
    .zotero_mapping_abort("INVALID_RESPONSE", "A lista de cole\u00e7\u00f5es do item Zotero \u00e9 inv\u00e1lida.")
  }
  lapply(collections, function(key) {
    if (!is.character(key) || length(key) != 1L || is.na(key) ||
        !grepl("^[A-Z0-9]{8}$", key)) {
      .zotero_mapping_abort("INVALID_RESPONSE", "Uma chave de cole\u00e7\u00e3o do item Zotero \u00e9 inv\u00e1lida.")
    }
    key
  })
}
