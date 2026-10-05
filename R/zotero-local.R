# Zotero local library operations. All requests use the shared GET-only client.

.zotero_library_id_text <- function(value) {
  if (is.character(value) && length(value) == 1L && !is.na(value) && grepl("^[0-9]+$", value)) {
    out <- sub("^0+", "", value)
    return(if (nzchar(out)) out else "0")
  }
  if (is.numeric(value) && !is.logical(value) && length(value) == 1L &&
      !is.na(value) && is.finite(value) && value >= 0 && value == floor(value) &&
      value <= 9007199254740991) {
    return(format(value, scientific = FALSE, trim = TRUE, digits = 22L))
  }
  NULL
}

#' Resolve library and Zotero instance context
#'
#' @param config Object returned by `zotero_config()`.
#' @param item Optional original Zotero wrapper whose `library` member can
#'   resolve the local user ID alias `"0"`.
#' @return Context with backend, library type and ID, server ID and instance ID.
.zotero_context <- function(config, item = NULL) {
  .zotero_validate_config(config)
  if (!is.null(item) && !is.list(item)) {
    .zotero_abort("INVALID_RESPONSE", "Wrapper Zotero invalido ao resolver contexto.")
  }

  library_id <- config$library_id
  is_local_user_alias <- identical(config$backend, "local") &&
    identical(config$library_type, "user") && identical(config$library_id, "0")
  if (is_local_user_alias) library_id <- config$state$library_id

  wrapper_library <- if (is.list(item)) item$library else NULL
  if (!is.null(wrapper_library)) {
    if (!is.list(wrapper_library)) {
      .zotero_abort("INVALID_RESPONSE", "Identidade da biblioteca no wrapper Zotero invalida.")
    }
    wrapper_type <- wrapper_library$type
    wrapper_id <- .zotero_library_id_text(wrapper_library$id)
    if (!is.null(wrapper_type) &&
        (!.zotero_scalar_text(wrapper_type) || !identical(wrapper_type, config$library_type))) {
      .zotero_abort("VERSION_CONFLICT", "O item pertence a outra biblioteca Zotero.")
    }
    if (!is.null(wrapper_library$id) && is.null(wrapper_id)) {
      .zotero_abort("INVALID_RESPONSE", "ID da biblioteca no wrapper Zotero invalido.")
    }
    if (is_local_user_alias) {
      if (identical(wrapper_type, "user") && !is.null(wrapper_id) && wrapper_id != "0") {
        prior_id <- config$state$library_id
        if (!is.null(prior_id) && !identical(prior_id, wrapper_id)) {
          .zotero_abort("VERSION_CONFLICT", "O ID de biblioteca Zotero mudou durante a sessao.")
        }
        library_id <- wrapper_id
        config$state$library_id <- wrapper_id
      }
    } else if (!is.null(wrapper_id) && !identical(wrapper_id, config$library_id)) {
      .zotero_abort("VERSION_CONFLICT", "O item pertence a outro ID de biblioteca Zotero.")
    }
  }

  server_id <- if (identical(config$backend, "local")) config$state$server_id else NULL
  if (is.null(library_id) && is.null(server_id) && is.null(config$instance_id)) {
    .zotero_abort("IDENTITY_UNRESOLVED", "Informe instance_id ou use um Zotero que exponha server_id.")
  }
  list(
    backend = config$backend,
    library_type = config$library_type,
    library_id = library_id,
    server_id = server_id,
    instance_id = config$instance_id
  )
}

.zotero_validate_wrappers <- function(items, object_type = "item") {
  if (!is.list(items)) .zotero_abort("INVALID_RESPONSE", "Lista de objetos Zotero invalida.")
  if (!object_type %in% c("item", "collection")) {
    .zotero_abort("INVALID_ARGUMENT", "Tipo de objeto Zotero invalido.")
  }
  for (item in items) {
    if (!is.list(item) || !.zotero_scalar_text(item$key) ||
        !grepl("^[A-Z0-9]{8}$", item$key) || !is.list(item$data)) {
      .zotero_abort("INVALID_RESPONSE", sprintf("Wrapper Zotero de %s invalido.", object_type))
    }
    if (identical(object_type, "item")) {
      item_type <- item$data$itemType %||% item$itemType
      if (!.zotero_scalar_text(item_type)) {
        .zotero_abort("INVALID_RESPONSE", "Wrapper de item Zotero sem itemType valido.")
      }
    }
  }
  invisible(items)
}

#' Validate Zotero connectivity and report API support
#'
#' @param config Object returned by `zotero_config()`.
#' @return Availability, API version, server identity, context and capabilities.
#' @export
zotero_status <- function(config) {
  .zotero_validate_config(config)
  if (identical(config$backend, "local")) {
    response <- .zotero_request(config, "", expect_json = TRUE)
  } else {
    response <- .zotero_request(
      config, paste0(.zotero_library_path(config), "/items/top"),
      query = list(limit = 1L, start = 0), expect_json = TRUE
    )
  }
  api_version <- response$headers[["zotero-api-version"]]
  if (is.null(api_version) && is.list(response$body)) {
    api_version <- response$body$version %||% response$body$apiVersion
  }
  if (is.numeric(api_version) && length(api_version) == 1L && !is.na(api_version)) {
    api_version <- as.character(api_version)
  }
  if (!.zotero_scalar_text(api_version)) {
    .zotero_abort("INVALID_RESPONSE", "Versao da API Zotero ausente na resposta.")
  }
  major_version <- sub("^([0-9]+)([.].*)?$", "\\1", api_version)
  if (!grepl("^[0-9]+$", major_version) || !identical(major_version, "3")) {
    .zotero_abort("INVALID_RESPONSE", "Apenas a API Zotero v3 e suportada.")
  }
  config$state$api_version <- "3"
  server_id <- if (identical(config$backend, "local")) config$state$server_id else NULL
  list(
    available = TRUE,
    backend = config$backend,
    api_version = "3",
    server_id = server_id,
    context = .zotero_context(config),
    capabilities = list(read = TRUE, local_files = identical(config$backend, "local"))
  )
}

#' List Zotero collections
#'
#' @param config Object returned by `zotero_config()`.
#' @param cursor Optional decimal offset returned by a previous page.
#' @param limit Number of results, from 1 to 100.
#' @return A page containing collection wrappers, cursor, total and context.
#' @export
zotero_list_collections <- function(config, cursor = NULL, limit = 50L) {
  .zotero_validate_config(config)
  page <- .zotero_response_page(config, paste0(.zotero_library_path(config), "/collections"), cursor, limit)
  .zotero_validate_wrappers(page$items, "collection")
  first_item <- if (length(page$items)) page$items[[1L]] else NULL
  list(
    items = page$items,
    next_cursor = page$next_cursor,
    total = page$total,
    context = .zotero_context(config, item = first_item),
    library_version = page$library_version
  )
}

#' Search Zotero top-level items
#'
#' @param config Object returned by `zotero_config()`.
#' @param query Optional Zotero quicksearch string.
#' @param collection_key Optional collection key to restrict the search.
#' @param cursor Optional decimal offset returned by a previous page.
#' @param limit Number of results, from 1 to 100.
#' @return A page containing top-level item wrappers, cursor, total and context.
#' @export
zotero_search_items <- function(config, query = NULL, collection_key = NULL,
                                cursor = NULL, limit = 50L) {
  .zotero_validate_config(config)
  if (!is.null(query) && (!is.character(query) || length(query) != 1L || is.na(query) ||
                          grepl("[[:cntrl:]]", query))) {
    .zotero_abort("INVALID_ARGUMENT", "query deve ser uma string simples.")
  }
  if (!is.null(collection_key)) .zotero_validate_key(collection_key, "collection_key")
  if (is.null(collection_key)) {
    path <- paste0(.zotero_library_path(config), "/items/top")
  } else {
    path <- paste0(.zotero_library_path(config), "/collections/", collection_key, "/items/top")
  }
  filters <- list()
  if (!is.null(query) && nzchar(query)) filters$q <- query
  page <- .zotero_response_page(config, path, cursor, limit, query = filters)
  .zotero_validate_wrappers(page$items, "item")
  context_item <- if (length(page$items)) page$items[[1L]] else NULL
  page$items <- Filter(function(item) {
    item_type <- item$data$itemType %||% item$itemType
    !item_type %in% c("note", "attachment")
  }, page$items)
  list(
    items = page$items,
    next_cursor = page$next_cursor,
    total = page$total,
    context = .zotero_context(config, item = context_item),
    library_version = page$library_version
  )
}

#' Get a Zotero item and its children
#'
#' @param config Object returned by `zotero_config()`.
#' @param item_key Eight-character Zotero item key.
#' @return The original item wrapper, child wrappers and resolved context.
#' @export
zotero_get_item <- function(config, item_key) {
  .zotero_validate_config(config)
  item_key <- .zotero_validate_key(item_key, "item_key")
  path <- paste0(.zotero_library_path(config), "/items/", item_key)
  response <- .zotero_request(config, path, expect_json = TRUE)
  item <- response$body
  .zotero_validate_wrappers(list(item), "item")
  if (!identical(item$key, item_key)) {
    .zotero_abort("INVALID_RESPONSE", "A API Zotero retornou outra chave de item.")
  }
  item_type <- item$data$itemType %||% item$itemType
  children <- list()
  if (!item_type %in% c("note", "attachment")) {
    children <- .zotero_fetch_all_pages(config, paste0(path, "/children"))
    .zotero_validate_wrappers(children, "item")
  }
  list(item = item, children = children, context = .zotero_context(config, item = item))
}
