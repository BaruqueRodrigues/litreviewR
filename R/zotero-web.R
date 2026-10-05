# Read-only Zotero Web API adapter on the shared F1 HTTP transport.

.zotero_web_wait <- function(seconds) Sys.sleep(seconds)

.zotero_web_wait_for_backoff <- function(config) {
  until <- config$state$web_backoff_until
  config$state$web_backoff_until <- NULL
  if (is.null(until)) return(invisible(NULL))
  remaining <- max(0, as.numeric(until) - as.numeric(Sys.time()))
  if (remaining > 0) .zotero_web_wait(min(remaining, 10))
  invisible(NULL)
}

.zotero_web_record_backoff <- function(config, response) {
  value <- response$headers[["backoff"]]
  if (is.null(value)) value <- response$headers[["retry-after"]]
  if (is.character(value) && length(value) == 1L && grepl("^[0-9]+$", value)) {
    seconds <- suppressWarnings(as.numeric(value))
    if (is.finite(seconds) && seconds > 0) {
      config$state$web_backoff_until <- as.numeric(Sys.time()) + min(seconds, 10)
    }
  }
  invisible(NULL)
}

.zotero_web_request <- function(config, path, query = list(), expect_json = TRUE) {
  if (!inherits(config, "zotero_config") || !identical(config$backend, "web")) {
    .zotero_abort("INVALID_CONFIG", "O adaptador Web requer uma configuracao Zotero Web.")
  }
  api_key <- Sys.getenv(config$api_key_env, unset = "")
  if (!nzchar(trimws(api_key))) {
    .zotero_abort(
      "INVALID_CONFIG",
      sprintf("Defina a variavel de ambiente %s para usar o Zotero Web.", config$api_key_env)
    )
  }

  headers <- list(`zotero-api-key` = api_key)
  max_attempts <- 3L
  .zotero_web_wait_for_backoff(config)
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(
      list(ok = TRUE, value = .zotero_http_request(
        config, path, query = query, expect_json = expect_json, headers = headers
      )),
      zotero_error = function(error) list(ok = FALSE, error = error)
    )
    if (isTRUE(result$ok)) {
      .zotero_web_record_backoff(config, result$value)
      return(result$value)
    }

    error <- result$error
    retryable_code <- error$code %in% c("RATE_LIMITED", "CONNECTION_UNAVAILABLE")
    if (!isTRUE(error$retryable) || !retryable_code || attempt == max_attempts) {
      stop(error)
    }

    delay <- error$details$retry_after_seconds
    if (is.null(delay) || !is.numeric(delay) || length(delay) != 1L ||
        is.na(delay) || !is.finite(delay) || delay < 0) {
      delay <- min(0.25 * 2^(attempt - 1L), 5)
    }
    .zotero_web_wait(min(delay, 10))
  }
  .zotero_abort("CONNECTION_UNAVAILABLE", "O servico Zotero esta indisponivel.", retryable = TRUE)
}
