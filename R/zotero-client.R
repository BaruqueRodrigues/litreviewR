# Zotero client: common configuration, HTTP transport and pagination helpers.

.zotero_error_codes <- c(
  "INVALID_CONFIG", "INVALID_ARGUMENT", "CONNECTION_UNAVAILABLE",
  "ACCESS_DENIED", "NOT_FOUND", "RATE_LIMITED", "INVALID_RESPONSE",
  "ATTACHMENT_UNAVAILABLE", "VERSION_CONFLICT", "WRITE_FAILED",
  "CORPUS_LOCKED", "LIMIT_EXCEEDED", "IDENTITY_UNRESOLVED",
  "BACKEND_UNAVAILABLE"
)

#' Configure a Zotero library
#'
#' Creates a read-only Zotero client configuration. Credentials are read by
#' backend-specific code from the environment variable named by `api_key_env`;
#' its value is never copied into the configuration.
#'
#' @param backend Zotero backend, `"local"` or `"web"`.
#' @param library_type Zotero library type, `"user"` or `"group"`.
#' @param library_id Decimal library ID as text. `"0"` is allowed only for a
#'   local user library.
#' @param base_url Optional exact supported API base URL.
#' @param api_key_env Environment variable name for a backend credential.
#' @param instance_id Optional persistent identifier for a local instance when
#'   Zotero does not report a server ID.
#' @param timeout Positive request timeout in seconds.
#' @param max_response_bytes Maximum response body size in bytes.
#' @param max_file_bytes Maximum attachment size in bytes.
#' @param max_pages Maximum pages fetched while resolving item children.
#' @param .transport Optional internal transport function for controlled tests.
#' @return An object of class `zotero_config`.
#' @export
zotero_config <- function(backend = "local", library_type = "user", library_id = "0",
                           base_url = NULL, api_key_env = "ZOTERO_API_KEY",
                           instance_id = NULL, timeout = 30,
                           max_response_bytes = 10485760,
                           max_file_bytes = 104857600, max_pages = 1000L,
                           .transport = NULL) {
  if (!is.character(library_id) || length(library_id) != 1L || is.na(library_id) ||
      !grepl("^[0-9]+$", library_id)) {
    .zotero_abort("INVALID_CONFIG", "library_id deve ser uma string decimal.")
  }
  canonical_id <- sub("^0+", "", library_id)
  if (!nzchar(canonical_id)) canonical_id <- "0"

  config <- structure(list(
    backend = backend,
    library_type = library_type,
    library_id = canonical_id,
    base_url = base_url,
    api_key_env = api_key_env,
    instance_id = instance_id,
    timeout = timeout,
    max_response_bytes = max_response_bytes,
    max_file_bytes = max_file_bytes,
    max_pages = max_pages,
    .transport = .transport,
    state = new.env(parent = emptyenv())
  ), class = "zotero_config")
  config$state$server_id <- NULL
  config$state$api_version <- NULL
  config$state$library_id <- NULL
  .zotero_validate_config(config)
  config
}

.zotero_scalar_text <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x) &&
    !grepl("[[:cntrl:]]", x)
}

.zotero_positive_number <- function(x) {
  is.numeric(x) && !is.logical(x) && length(x) == 1L && !is.na(x) &&
    is.finite(x) && x > 0
}

.zotero_positive_integer <- function(x) {
  .zotero_positive_number(x) && x == floor(x)
}

.zotero_validate_config <- function(config) {
  invalid <- function() .zotero_abort("INVALID_CONFIG", "Configuracao Zotero invalida.")
  if (!inherits(config, "zotero_config") || !is.list(config)) invalid()
  if (!.zotero_scalar_text(config$backend) || !config$backend %in% c("local", "web")) invalid()
  if (!.zotero_scalar_text(config$library_type) || !config$library_type %in% c("user", "group")) invalid()
  if (!.zotero_scalar_text(config$library_id) || !grepl("^(0|[1-9][0-9]*)$", config$library_id)) invalid()
  if (identical(config$library_id, "0") &&
      !(identical(config$backend, "local") && identical(config$library_type, "user"))) invalid()
  if (!.zotero_scalar_text(config$api_key_env) ||
      !grepl("^[A-Za-z_][A-Za-z0-9_]*$", config$api_key_env)) invalid()
  if (!is.null(config$instance_id) && !.zotero_scalar_text(config$instance_id)) invalid()
  if (!.zotero_positive_number(config$timeout)) invalid()
  if (!.zotero_positive_integer(config$max_response_bytes) ||
      !.zotero_positive_integer(config$max_file_bytes) ||
      !.zotero_positive_integer(config$max_pages)) invalid()
  if (!is.null(config$.transport) && !is.function(config$.transport)) invalid()
  if (!is.environment(config$state)) invalid()

  expected_base <- if (identical(config$backend, "local")) {
    c("http://localhost:23119/api/", "http://127.0.0.1:23119/api/",
      "http://[::1]:23119/api/")
  } else {
    "https://api.zotero.org/"
  }
  if (is.null(config$base_url)) return(invisible(config))
  if (!.zotero_scalar_text(config$base_url) || !config$base_url %in% expected_base) invalid()
  invisible(config)
}

#' Create a structured Zotero error
#'
#' @param code Stable contract error code.
#' @param message Safe user-facing message.
#' @param retryable Whether the operation may succeed after a transient retry.
#' @param details Optional non-sensitive structured details.
#' @return Does not return; raises a `zotero_error` condition.
.zotero_abort <- function(code, message, retryable = FALSE, details = NULL) {
  if (!is.character(code) || length(code) != 1L || is.na(code) ||
      !code %in% .zotero_error_codes) code <- "INVALID_RESPONSE"
  if (!.zotero_scalar_text(message)) message <- "Falha na operacao Zotero."
  condition <- structure(
    list(message = message, call = NULL, code = code,
         retryable = isTRUE(retryable), details = details),
    class = c("zotero_error", "error", "condition")
  )
  stop(condition)
}

.zotero_validate_key <- function(key, field = "item_key") {
  if (!.zotero_scalar_text(field) || !field %in% c("item_key", "collection_key", "attachment_key")) {
    .zotero_abort("INVALID_ARGUMENT", "Campo de chave Zotero invalido.")
  }
  if (!.zotero_scalar_text(key) || !grepl("^[A-Z0-9]{8}$", key)) {
    .zotero_abort("INVALID_ARGUMENT", sprintf("%s deve ter oito caracteres maiusculos alfanumericos.", field))
  }
  key
}

.zotero_library_path <- function(config) {
  .zotero_validate_config(config)
  prefix <- if (identical(config$library_type, "user")) "users" else "groups"
  paste(prefix, config$library_id, sep = "/")
}

.zotero_validate_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    .zotero_abort("INVALID_ARGUMENT", "Caminho Zotero invalido.")
  }
  if (!nzchar(path)) return(path)
  segments <- strsplit(path, "/", fixed = TRUE)[[1L]]
  if (startsWith(path, "/") || grepl("[?#]", path) || grepl("\\\\", path) ||
      grepl("%", path, fixed = TRUE) || grepl("[[:cntrl:]]", path) ||
      grepl("://", path, fixed = TRUE) || any(!nzchar(segments)) ||
      any(segments %in% c(".", "..")) || !grepl("^[A-Za-z0-9/_-]+$", path)) {
    .zotero_abort("INVALID_ARGUMENT", "Caminho Zotero invalido.")
  }
  path
}

.zotero_validate_query <- function(query) {
  if (!is.list(query) || (length(query) &&
      (is.null(names(query)) || anyNA(names(query)) || any(!nzchar(names(query))) ||
       anyDuplicated(names(query))))) {
    .zotero_abort("INVALID_ARGUMENT", "Parametros de consulta invalidos.")
  }
  if (!length(query)) return(query)
  if (any(!grepl("^[A-Za-z][A-Za-z0-9_-]*$", names(query)))) {
    .zotero_abort("INVALID_ARGUMENT", "Parametros de consulta invalidos.")
  }
  allowed <- c("limit", "start", "q", "itemType", "tag", "sort", "direction", "since")
  if (any(!names(query) %in% allowed)) {
    .zotero_abort("INVALID_ARGUMENT", "Parametro de consulta nao permitido.")
  }
  for (value in query) {
    valid <- (is.character(value) && length(value) == 1L && !is.na(value) &&
                !grepl("[[:cntrl:]]", value)) ||
      (is.numeric(value) && !is.logical(value) && length(value) == 1L && is.finite(value)) ||
      (is.logical(value) && length(value) == 1L && !is.na(value))
    if (!valid) .zotero_abort("INVALID_ARGUMENT", "Valor de consulta invalido.")
  }
  query
}

.zotero_validate_headers <- function(headers) {
  if (!is.list(headers) || (length(headers) &&
      (is.null(names(headers)) || anyNA(names(headers)) || any(!nzchar(names(headers)))))) {
    .zotero_abort("INVALID_ARGUMENT", "Headers HTTP invalidos.")
  }
  if (!length(headers)) return(headers)
  header_names <- tolower(names(headers))
  allowed <- c("accept", "user-agent", "zotero-api-version", "zotero-api-key",
               "authorization", "zotero-server-id")
  if (anyDuplicated(header_names) || any(!header_names %in% allowed)) {
    .zotero_abort("INVALID_ARGUMENT", "Header HTTP nao permitido.")
  }
  for (value in headers) {
    if (!.zotero_scalar_text(as.character(value)) || length(value) != 1L ||
        grepl("[[:cntrl:]]", as.character(value))) {
      .zotero_abort("INVALID_ARGUMENT", "Valor de header HTTP invalido.")
    }
  }
  names(headers) <- header_names
  headers
}

.zotero_normalize_headers <- function(headers) {
  if (is.null(headers)) return(list())
  if (is.character(headers)) headers <- as.list(headers)
  if (!is.list(headers) || (length(headers) &&
      (is.null(names(headers)) || anyNA(names(headers)) || any(!nzchar(names(headers)))))) {
    .zotero_abort("INVALID_RESPONSE", "Resposta HTTP invalida.")
  }
  if (!length(headers)) return(list())
  names(headers) <- tolower(names(headers))
  if (any(!grepl("^[A-Za-z0-9-]+$", names(headers))) || anyDuplicated(names(headers))) {
    .zotero_abort("INVALID_RESPONSE", "Resposta HTTP invalida.")
  }
  for (value in headers) {
    if (length(value) != 1L || is.na(value) || grepl("[[:cntrl:]]", as.character(value))) {
      .zotero_abort("INVALID_RESPONSE", "Resposta HTTP invalida.")
    }
  }
  lapply(headers, as.character)
}

.zotero_stream_collector <- function(max_response_bytes) {
  state <- new.env(parent = emptyenv())
  state$chunks <- list()
  state$bytes <- 0
  state$exceeded <- FALSE
  write_chunk <- function(data) {
    if (!is.raw(data)) {
      state$exceeded <- TRUE
      stop("invalid stream chunk", call. = FALSE)
    }
    next_bytes <- state$bytes + length(data)
    if (next_bytes > max_response_bytes) {
      state$exceeded <- TRUE
      stop("response byte limit exceeded", call. = FALSE)
    }
    if (length(data)) state$chunks[[length(state$chunks) + 1L]] <- data
    state$bytes <- next_bytes
    length(data)
  }
  list(
    write = write_chunk,
    state = state,
    body = function() if (length(state$chunks)) do.call(c, state$chunks) else raw()
  )
}

.zotero_httr_get <- function(url, query, headers, timeout, followlocation, output) {
  additional_headers <- do.call(httr::add_headers, list(.headers = headers))
  httr::GET(
    url,
    query = query,
    additional_headers,
    httr::timeout(timeout),
    httr::config(followlocation = followlocation),
    output
  )
}

.zotero_default_transport <- function(request, .get = .zotero_httr_get) {
  collector <- .zotero_stream_collector(request$max_response_bytes)
  header_names <- names(request$headers)
  header_values <- as.character(unlist(request$headers, use.names = FALSE))
  names(header_values) <- vapply(header_names, function(name) {
    paste0(toupper(substr(name, 1L, 1L)), substr(name, 2L, nchar(name)))
  }, character(1))
  response <- tryCatch(
    .get(
      url = request$url,
      query = request$query,
      headers = header_values,
      timeout = request$timeout,
      followlocation = 0L,
      output = httr::write_stream(collector$write)
    ),
    error = function(error) {
      if (isTRUE(collector$state$exceeded)) {
        .zotero_abort("LIMIT_EXCEEDED", "Resposta Zotero excede o limite configurado.")
      }
      .zotero_abort("CONNECTION_UNAVAILABLE", "Nao foi possivel conectar a Zotero.", retryable = TRUE)
    }
  )
  list(
    status = as.integer(httr::status_code(response)),
    headers = as.list(httr::headers(response)),
    body = collector$body()
  )
}

.zotero_observe_server_id <- function(config, headers) {
  if (!identical(config$backend, "local")) return(invisible(NULL))
  observed <- headers[["zotero-server-id"]]
  if (is.null(observed)) return(invisible(NULL))
  if (!.zotero_scalar_text(observed)) {
    .zotero_abort("INVALID_RESPONSE", "Identificador do servidor Zotero invalido.")
  }
  prior <- config$state$server_id
  if (!is.null(prior) && !identical(prior, observed)) {
    .zotero_abort("VERSION_CONFLICT", "A instancia Zotero mudou durante a sessao.")
  }
  config$state$server_id <- observed
  invisible(observed)
}

.zotero_retry_details <- function(status, headers) {
  details <- list(http_status = status)
  value <- headers[["retry-after"]]
  if (is.null(value)) value <- headers[["backoff"]]
  if (!is.null(value) && grepl("^[0-9]+$", value)) {
    seconds <- suppressWarnings(as.numeric(value))
    if (is.finite(seconds) && seconds <= .Machine$integer.max) {
      details$retry_after_seconds <- seconds
    }
  }
  details
}

.zotero_http_request <- function(config, path, query = list(), expect_json = TRUE,
                                 headers = list()) {
  .zotero_validate_config(config)
  path <- .zotero_validate_path(path)
  query <- .zotero_validate_query(query)
  headers <- .zotero_validate_headers(headers)
  if (!is.logical(expect_json) || length(expect_json) != 1L || is.na(expect_json)) {
    .zotero_abort("INVALID_ARGUMENT", "expect_json deve ser TRUE ou FALSE.")
  }
  if (identical(config$backend, "local") &&
      any(names(headers) %in% c("zotero-api-key", "authorization"))) {
    .zotero_abort("INVALID_ARGUMENT", "Autenticacao nao e aceita no backend local.")
  }

  base <- config$base_url
  if (is.null(base)) base <- if (identical(config$backend, "local")) {
    "http://localhost:23119/api/"
  } else {
    "https://api.zotero.org/"
  }
  request_headers <- list(
    accept = "application/json, text/plain;q=0.9",
    `user-agent` = "litreviewR/0.1.0",
    `zotero-api-version` = "3"
  )
  if (identical(config$backend, "local") && !is.null(config$state$server_id)) {
    request_headers[["zotero-server-id"]] <- config$state$server_id
  }
  for (header_name in names(headers)) request_headers[[header_name]] <- headers[[header_name]]
  request_headers <- .zotero_validate_headers(request_headers)
  request <- list(
    method = "GET",
    url = paste0(base, path),
    query = query,
    headers = request_headers,
    timeout = config$timeout,
    max_response_bytes = config$max_response_bytes
  )

  response <- tryCatch({
    if (is.function(config$.transport)) config$.transport(request) else .zotero_default_transport(request)
  },
  zotero_error = function(error) stop(error),
  error = function(error) .zotero_abort(
    "CONNECTION_UNAVAILABLE", "Nao foi possivel conectar a Zotero.", retryable = TRUE
  ))

  required <- c("status", "headers", "body")
  if (!is.list(response) || !all(required %in% names(response)) ||
      !is.integer(response$status) || length(response$status) != 1L ||
      is.na(response$status) || response$status < 100L || response$status > 599L ||
      !(is.raw(response$body) || (is.character(response$body) &&
        length(response$body) == 1L && !is.na(response$body)))) {
    .zotero_abort("INVALID_RESPONSE", "O transporte Zotero retornou uma resposta invalida.")
  }
  response_headers <- .zotero_normalize_headers(response$headers)
  .zotero_observe_server_id(config, response_headers)

  if (response$status != 200L) {
    status <- response$status
    if (status %in% c(401L, 403L)) {
      .zotero_abort("ACCESS_DENIED", "Acesso a biblioteca Zotero negado.", details = list(http_status = status))
    }
    if (status == 404L) {
      .zotero_abort("NOT_FOUND", "Recurso Zotero nao encontrado.", details = list(http_status = status))
    }
    if (status %in% c(412L, 428L)) {
      .zotero_abort("VERSION_CONFLICT", "A versao ou instancia Zotero mudou.", details = list(http_status = status))
    }
    if (status == 429L) {
      .zotero_abort("RATE_LIMITED", "Limite de requisicoes Zotero atingido.", retryable = TRUE,
                    details = .zotero_retry_details(status, response_headers))
    }
    if (status >= 500L) {
      .zotero_abort("CONNECTION_UNAVAILABLE", "O servico Zotero esta indisponivel.", retryable = TRUE,
                    details = .zotero_retry_details(status, response_headers))
    }
    if (status >= 400L) {
      .zotero_abort("INVALID_RESPONSE", "A requisicao Zotero foi rejeitada.", details = list(http_status = status))
    }
    .zotero_abort("INVALID_RESPONSE", "Resposta de redirecionamento ou status inesperado do Zotero.",
                  details = list(http_status = status))
  }

  body_bytes <- if (is.raw(response$body)) length(response$body) else nchar(response$body, type = "bytes")
  if (body_bytes > config$max_response_bytes) {
    .zotero_abort("LIMIT_EXCEEDED", "Resposta Zotero excede o limite configurado.")
  }
  body_text <- if (is.raw(response$body)) {
    tryCatch(rawToChar(response$body), error = function(error) {
      .zotero_abort("INVALID_RESPONSE", "Resposta Zotero nao e texto valido.")
    })
  } else {
    response$body
  }
  body_text <- enc2utf8(body_text)
  if (isTRUE(expect_json)) {
    decoded <- tryCatch(
      jsonlite::fromJSON(body_text, simplifyVector = FALSE),
      error = function(error) .zotero_abort("INVALID_RESPONSE", "Resposta Zotero nao contem JSON valido.")
    )
    return(list(status = response$status, headers = response_headers, body = decoded))
  }
  list(status = response$status, headers = response_headers, body = body_text)
}

.zotero_request <- function(config, path, query = list(), expect_json = TRUE) {
  .zotero_validate_config(config)
  path <- .zotero_validate_path(path)
  query <- .zotero_validate_query(query)
  if (identical(config$backend, "web")) {
    request_env <- environment(.zotero_request)
    if (!exists(".zotero_web_request", envir = request_env,
                mode = "function", inherits = TRUE)) {
      .zotero_abort("BACKEND_UNAVAILABLE", "O backend Zotero Web ainda nao esta disponivel.")
    }
    web_request <- get(".zotero_web_request", envir = request_env, inherits = TRUE)
    return(web_request(config, path, query = query, expect_json = expect_json))
  }
  .zotero_http_request(config, path, query = query, expect_json = expect_json)
}

.zotero_parse_offset <- function(cursor) {
  if (is.null(cursor)) return("0")
  if (!.zotero_scalar_text(cursor) || !grepl("^[0-9]+$", cursor)) {
    .zotero_abort("INVALID_ARGUMENT", "cursor deve ser um offset decimal sem sinal.")
  }
  number <- suppressWarnings(as.numeric(cursor))
  if (!is.finite(number) || number > 9007199254740991) {
    .zotero_abort("INVALID_ARGUMENT", "cursor excede o limite numerico suportado.")
  }
  canonical <- sub("^0+", "", cursor)
  if (nzchar(canonical)) canonical else "0"
}

.zotero_validate_limit <- function(limit) {
  if (!.zotero_positive_integer(limit) || limit > 100) {
    .zotero_abort("INVALID_ARGUMENT", "limit deve ser um inteiro entre 1 e 100.")
  }
  as.integer(limit)
}

.zotero_parse_integer_header <- function(value, field) {
  if (is.null(value)) return(NULL)
  if (!.zotero_scalar_text(value) || !grepl("^[0-9]+$", value)) {
    .zotero_abort("INVALID_RESPONSE", sprintf("Header %s invalido.", field))
  }
  number <- suppressWarnings(as.numeric(value))
  if (!is.finite(number) || number > .Machine$integer.max) {
    .zotero_abort("INVALID_RESPONSE", sprintf("Header %s invalido.", field))
  }
  as.integer(number)
}

.zotero_response_page <- function(config, path, cursor, limit, query = list()) {
  offset_text <- .zotero_parse_offset(cursor)
  limit <- .zotero_validate_limit(limit)
  offset <- as.numeric(offset_text)
  page_query <- c(list(limit = limit, start = offset), query)
  response <- .zotero_request(config, path, query = page_query, expect_json = TRUE)
  items <- response$body
  if (!is.list(items)) .zotero_abort("INVALID_RESPONSE", "A API Zotero nao retornou uma lista de itens.")
  if (length(items) > limit) .zotero_abort("LIMIT_EXCEEDED", "Zotero retornou mais itens que o limite solicitado.")
  total <- .zotero_parse_integer_header(response$headers[["total-results"]], "Total-Results")
  library_version <- .zotero_parse_integer_header(
    response$headers[["last-modified-version"]] %||% response$headers[["zotero-library-version"]],
    "Last-Modified-Version"
  )
  if (!is.null(total) && (offset > total || offset + length(items) > total)) {
    .zotero_abort("INVALID_RESPONSE", "Total-Results nao corresponde a pagina recebida.")
  }
  next_offset <- offset + length(items)
  has_more <- if (!is.null(total)) next_offset < total else length(items) == limit
  if (has_more && length(items) == 0L) {
    .zotero_abort("INVALID_RESPONSE", "A paginacao Zotero nao avancou.")
  }
  list(
    items = items,
    next_cursor = if (has_more) format(next_offset, scientific = FALSE, trim = TRUE) else NULL,
    total = total,
    library_version = library_version
  )
}

.zotero_fetch_all_pages <- function(config, path, page_size = 100L) {
  cursor <- NULL
  items <- list()
  pages <- 0L
  while (TRUE) {
    if (pages >= config$max_pages) {
      .zotero_abort("LIMIT_EXCEEDED", "Numero maximo de paginas Zotero excedido.")
    }
    page <- .zotero_response_page(config, path, cursor, page_size)
    pages <- pages + 1L
    if (length(page$items)) items <- c(items, page$items)
    if (is.null(page$next_cursor)) break
    cursor <- page$next_cursor
  }
  items
}
