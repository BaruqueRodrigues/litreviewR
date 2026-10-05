.zotero_test_fixture <- function(name) {
  jsonlite::fromJSON(
    testthat::test_path("fixtures", "zotero", "local", name),
    simplifyVector = FALSE
  )
}

.zotero_test_response <- function(body, headers = list(), status = 200L) {
  body_raw <- if (is.raw(body)) body else if (is.character(body) && length(body) == 1L) {
    charToRaw(body)
  } else {
    charToRaw(as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")))
  }
  list(status = as.integer(status), headers = headers, body = body_raw)
}

.zotero_test_error <- function(code, expr) {
  result <- tryCatch({
    force(expr)
    NULL
  }, zotero_error = function(error) error)
  testthat::expect_s3_class(result, "zotero_error")
  testthat::expect_identical(result$code, code)
  result
}

test_that("Zotero config validates and canonicalizes library IDs", {
  config <- zotero_config(library_id = "00123", .transport = function(request) NULL)
  expect_identical(config$library_id, "123")
  expect_identical(config$base_url, NULL)
  expect_false("api_key" %in% names(config))
  expect_identical(config$state$server_id, NULL)
  expect_identical(zotero_config(library_id = "00")$library_id, "0")

  expect_error(zotero_config(library_id = 123), class = "zotero_error")
  expect_error(zotero_config(backend = "web", library_id = "0"), class = "zotero_error")
  expect_error(zotero_config(library_type = "group", library_id = "00"), class = "zotero_error")
  expect_error(zotero_config(base_url = "http://localhost:23120/api/"), class = "zotero_error")
  expect_error(zotero_config(backend = "web", library_id = "12", base_url = "https://example.test/"),
               class = "zotero_error")
  expect_identical(.zotero_validate_key("ATTACH12", "attachment_key"), "ATTACH12")
})

test_that("structured errors preserve stable fields without remote text", {
  error <- .zotero_test_error("ACCESS_DENIED", .zotero_abort(
    "ACCESS_DENIED", "Safe message", details = list(http_status = 403L)
  ))
  expect_identical(class(error), c("zotero_error", "error", "condition"))
  expect_identical(error$message, "Safe message")
  expect_false(error$retryable)
  expect_identical(error$details, list(http_status = 403L))

  config <- zotero_config(.transport = function(request) stop("TRANSPORT_SECRET"))
  transport_error <- .zotero_test_error("CONNECTION_UNAVAILABLE", .zotero_http_request(config, ""))
  expect_false(grepl("TRANSPORT_SECRET", conditionMessage(transport_error), fixed = TRUE))
})

test_that("default transport passes safe headers, timeout and disabled redirects to httr", {
  captured <- new.env(parent = emptyenv())
  request <- list(
    method = "GET",
    url = "https://api.zotero.org/users/123/items",
    query = list(limit = 1L, start = 0),
    headers = list(accept = "application/json", `user-agent` = "litreviewR/test",
                   `zotero-api-version` = "3", `zotero-api-key` = "SYNTHETIC_KEY"),
    timeout = 13,
    max_response_bytes = 16
  )
  response <- .zotero_default_transport(
    request,
    .get = function(url, query, headers, timeout, followlocation, output) {
      captured$url <- url
      captured$query <- query
      captured$headers <- headers
      captured$timeout <- timeout
      captured$followlocation <- followlocation
      captured$method <- "GET"
      output$output$f(charToRaw("ok"))
      structure(list(
        url = url, status_code = 200L,
        headers = list(`Zotero-API-Version` = "3"), content = raw(),
        times = NULL, cookies = data.frame(), config = list(), request = list()
      ), class = "response")
    }
  )
  expect_identical(captured$url, request$url)
  expect_identical(captured$query, request$query)
  expect_identical(tolower(names(captured$headers)), names(request$headers))
  expect_identical(unname(captured$headers[["Zotero-api-key"]]), "SYNTHETIC_KEY")
  expect_identical(captured$timeout, 13)
  expect_identical(captured$followlocation, 0L)
  expect_identical(captured$method, "GET")
  expect_identical(response$body, charToRaw("ok"))
})

test_that("common HTTP interface validates requests and decodes only raw or text bodies", {
  captured <- new.env(parent = emptyenv())
  config <- zotero_config(
    library_id = "123",
    base_url = "http://127.0.0.1:23119/api/",
    timeout = 7,
    max_response_bytes = 128,
    .transport = function(request) {
      captured$request <- request
      .zotero_test_response('{"ok":true}', headers = list(`Zotero-Server-ID` = "server-a"))
    }
  )
  response <- .zotero_http_request(config, "users/123/items/top", query = list(limit = 2L, start = 0))
  expect_identical(response$body, list(ok = TRUE))
  expect_identical(names(response$headers), "zotero-server-id")
  expect_identical(captured$request$method, "GET")
  expect_identical(captured$request$url, "http://127.0.0.1:23119/api/users/123/items/top")
  expect_identical(captured$request$query, list(limit = 2L, start = 0))
  expect_identical(captured$request$timeout, 7)
  expect_identical(captured$request$max_response_bytes, 128)
  expect_identical(captured$request$headers[["zotero-api-version"]], "3")
  expect_false(grepl("?", captured$request$url, fixed = TRUE))
  expect_identical(captured$request$headers[["zotero-server-id"]], NULL)

  text_config <- zotero_config(.transport = function(request) {
    list(status = 200L, headers = list(), body = "file:///tmp/synthetic.pdf")
  })
  expect_identical(.zotero_request(text_config, "users/0/items/ATTACH12/file/view/url",
                                   expect_json = FALSE)$body, "file:///tmp/synthetic.pdf")

  .zotero_test_error("INVALID_ARGUMENT", .zotero_request(config, "../items"))
  .zotero_test_error("INVALID_ARGUMENT", .zotero_request(config, "https://example.test/"))
  .zotero_test_error("INVALID_ARGUMENT", .zotero_request(config, "users/123/items?key=secret"))
  .zotero_test_error("INVALID_ARGUMENT", .zotero_http_request(config, "users/123/items", headers = list(Injected = "x")))
  .zotero_test_error("INVALID_ARGUMENT", .zotero_http_request(config, "users/123/items", headers = list(`user-agent` = "x\nInjected: y")))
})

test_that("response limit is checked for fakes and while streaming chunks", {
  config <- zotero_config(max_response_bytes = 4, .transport = function(request) {
    list(status = 200L, headers = list(), body = charToRaw("12345"))
  })
  .zotero_test_error("LIMIT_EXCEEDED", .zotero_http_request(config, ""))

  collector <- .zotero_stream_collector(4)
  expect_identical(collector$write(as.raw(c(1, 2, 3))), 3L)
  try(collector$write(as.raw(c(4, 5))), silent = TRUE)
  expect_true(collector$state$exceeded)
  expect_length(collector$body(), 3L)
})

test_that("HTTP failures map safely and reject decoded objects from transports", {
  config <- zotero_config(.transport = function(request) {
    list(status = 403L, headers = list(), body = charToRaw("REMOTE_BODY_SECRET"))
  })
  error <- .zotero_test_error("ACCESS_DENIED", .zotero_http_request(config, "users/0"))
  expect_false(grepl("REMOTE_BODY_SECRET", conditionMessage(error), fixed = TRUE))
  expect_false(grepl("REMOTE_BODY_SECRET", paste(capture.output(str(error)), collapse = " "), fixed = TRUE))

  mappings <- list(
    `404` = "NOT_FOUND", `412` = "VERSION_CONFLICT", `428` = "VERSION_CONFLICT",
    `429` = "RATE_LIMITED", `503` = "CONNECTION_UNAVAILABLE", `400` = "INVALID_RESPONSE"
  )
  for (status in names(mappings)) {
    config <- zotero_config(.transport = local({
      code <- as.integer(status)
      function(request) list(status = code, headers = list(), body = raw())
    }))
    error <- .zotero_test_error(mappings[[status]], .zotero_http_request(config, ""))
    expect_identical(error$retryable, status %in% c("429", "503"))
  }

  decoded_config <- zotero_config(.transport = function(request) {
    list(status = 200L, headers = list(), body = list(secret = "not-raw"))
  })
  .zotero_test_error("INVALID_RESPONSE", .zotero_http_request(decoded_config, ""))
})

test_that("Web requests dispatch through the shared HTTP transport", {
  withr::local_envvar(c(LITREVIEW_TEST_ZOTERO_KEY = "synthetic-secret"))
  requests <- list()
  config <- zotero_config(
    backend = "web", library_id = "123", api_key_env = "LITREVIEW_TEST_ZOTERO_KEY",
    .transport = function(request) {
      requests[[length(requests) + 1L]] <<- request
      .zotero_test_response('{"version":"3"}', headers = list(`Zotero-API-Version` = "3"))
    }
  )

  response <- .zotero_request(config, "users/123/items/top")

  expect_identical(response$body$version, "3")
  expect_length(requests, 1L)
  expect_identical(requests[[1L]]$method, "GET")
  expect_identical(requests[[1L]]$headers[["zotero-api-key"]], "synthetic-secret")
  expect_false("synthetic-secret" %in% unlist(config, use.names = FALSE))
})

test_that("local status uses API v3 and server ID as an identity fallback", {
  requests <- list()
  config <- zotero_config(.transport = function(request) {
    requests[[length(requests) + 1L]] <<- request
    .zotero_test_response(.zotero_test_fixture("api-root.json"),
                          headers = list(`Zotero-API-Version` = "3", `Zotero-Server-ID` = "instance-a"))
  })
  status <- zotero_status(config)
  expect_true(status$available)
  expect_identical(status$api_version, "3")
  expect_identical(status$server_id, "instance-a")
  expect_null(status$context$library_id)
  expect_identical(status$context$server_id, "instance-a")
  expect_identical(status$capabilities, list(read = TRUE, local_files = TRUE))
  expect_identical(requests[[1L]]$url, "http://localhost:23119/api/")
  expect_length(requests, 1L)

  old_config <- zotero_config(.transport = function(request) {
    .zotero_test_response(.zotero_test_fixture("api-root.json"),
                          headers = list(`Zotero-API-Version` = "2"))
  })
  .zotero_test_error("INVALID_RESPONSE", zotero_status(old_config))

  unresolved <- zotero_config(.transport = function(request) {
    .zotero_test_response(.zotero_test_fixture("api-root.json"),
                          headers = list(`Zotero-API-Version` = "3"))
  })
  .zotero_test_error("IDENTITY_UNRESOLVED", zotero_status(unresolved))
})

test_that("context resolves alias 0 from a compatible wrapper and requires a fallback", {
  config <- zotero_config()
  item <- .zotero_test_fixture("article.json")
  expect_identical(.zotero_context(config, item)$library_id, "123")
  expect_identical(.zotero_context(config, item)$server_id, NULL)

  server_config <- zotero_config(instance_id = "installation-a")
  context <- .zotero_context(server_config)
  expect_null(context$library_id)
  expect_identical(context$instance_id, "installation-a")

  wrong_type <- item
  wrong_type$library$type <- "group"
  .zotero_test_error("VERSION_CONFLICT", .zotero_context(config, wrong_type))
  no_identity <- .zotero_test_error("IDENTITY_UNRESOLVED", .zotero_context(zotero_config()))
  expect_identical(no_identity$retryable, FALSE)
})

test_that("server ID changes within one config fail with VERSION_CONFLICT", {
  ids <- c("server-a", "server-b")
  requests <- list()
  config <- zotero_config(.transport = function(request) {
    requests[[length(requests) + 1L]] <<- request
    current <- ids[[1L]]
    ids <<- ids[-1L]
    .zotero_test_response("{}", headers = list(`Zotero-Server-ID` = current))
  })
  .zotero_http_request(config, "")
  .zotero_test_error("VERSION_CONFLICT", .zotero_http_request(config, ""))
  expect_identical(requests[[2L]]$headers[["zotero-server-id"]], "server-a")
})

test_that("collection pagination calculates cursors from total and explicit offsets", {
  collection <- .zotero_test_fixture("collection.json")
  calls <- list()
  config <- zotero_config(.transport = function(request) {
    calls[[length(calls) + 1L]] <<- request
    result <- if (request$query$start == 0) list(collection) else list(collection)
    .zotero_test_response(result, headers = list(`Total-Results` = "2", `Last-Modified-Version` = "9",
                                                  `Zotero-Server-ID` = "server-a"))
  })
  first <- zotero_list_collections(config, limit = 1L)
  expect_identical(first$next_cursor, "1")
  expect_identical(first$total, 2L)
  expect_identical(first$library_version, 9L)
  expect_identical(calls[[1L]]$query$start, 0)
  second <- zotero_list_collections(config, cursor = first$next_cursor, limit = 1L)
  expect_null(second$next_cursor)
  expect_identical(calls[[2L]]$query$start, 1)
  expect_identical(second$context$server_id, "server-a")

  empty <- zotero_config(library_id = "123", .transport = function(request) .zotero_test_response(list(), headers = list()))
  expect_identical(zotero_list_collections(empty, limit = 5L)$items, list())
  expect_null(zotero_list_collections(empty, limit = 5L)$next_cursor)
  .zotero_test_error("INVALID_ARGUMENT", zotero_list_collections(config, cursor = "https://example.test"))
  .zotero_test_error("INVALID_ARGUMENT", zotero_list_collections(config, cursor = "-1"))
  .zotero_test_error("INVALID_ARGUMENT", zotero_list_collections(config, cursor = "1.5"))
  .zotero_test_error("INVALID_ARGUMENT", zotero_list_collections(config, limit = 101L))
})

test_that("search uses quicksearch and collection top-level endpoint, filtering child types", {
  article <- .zotero_test_fixture("article.json")
  note <- .zotero_test_fixture("note.json")
  attachment <- .zotero_test_fixture("attachment.json")
  captured <- new.env(parent = emptyenv())
  config <- zotero_config(library_id = "123", .transport = function(request) {
    captured$request <- request
    .zotero_test_response(list(article, note, attachment),
                          headers = list(`Total-Results` = "3", `Zotero-Server-ID` = "server-a"))
  })
  page <- zotero_search_items(config, query = "climate change", collection_key = "COLL1234", limit = 3L)
  expect_length(page$items, 1L)
  expect_identical(page$items[[1L]]$key, "ABCD1234")
  expect_identical(captured$request$url,
                   "http://localhost:23119/api/users/123/collections/COLL1234/items/top")
  expect_identical(captured$request$query$q, "climate change")
  expect_identical(captured$request$query$limit, 3L)
})

test_that("search allows a final empty page when total is unknown", {
  article <- .zotero_test_fixture("article.json")
  starts <- numeric()
  config <- zotero_config(.transport = function(request) {
    starts <<- c(starts, request$query$start)
    if (request$query$start == 0) .zotero_test_response(list(article), headers = list()) else {
      .zotero_test_response(list(), headers = list())
    }
  })
  first <- zotero_search_items(config, limit = 1L)
  expect_identical(first$next_cursor, "1")
  expect_identical(first$context$library_id, "123")
  second <- zotero_search_items(config, cursor = first$next_cursor, limit = 1L)
  expect_null(second$next_cursor)
  expect_identical(second$context$library_id, "123")
  expect_identical(starts, c(0, 1))
})

test_that("an empty page preserves the library ID already resolved from a wrapper", {
  article <- .zotero_test_fixture("article.json")
  config <- zotero_config(.transport = function(request) {
    if (request$query$start == 0) .zotero_test_response(list(article), headers = list()) else {
      .zotero_test_response(list(), headers = list())
    }
  })
  first <- zotero_search_items(config, limit = 1L)
  expect_identical(first$context$library_id, "123")
  second <- zotero_search_items(config, cursor = first$next_cursor, limit = 1L)
  expect_length(second$items, 0L)
  expect_identical(second$context$library_id, "123")

  other <- article
  other$library$id <- 456
  .zotero_test_error("VERSION_CONFLICT", .zotero_context(config, other))
})

test_that("get item paginates children, skips note children and enforces max_pages", {
  article <- .zotero_test_fixture("article.json")
  child <- .zotero_test_fixture("child.json")
  attachment <- .zotero_test_fixture("attachment.json")
  starts <- numeric()
  config <- zotero_config(library_id = "123", max_pages = 2L, .transport = function(request) {
    if (grepl("/children$", request$url)) {
      starts <<- c(starts, request$query$start)
      child_page <- if (request$query$start == 0) list(child) else list(attachment)
      return(.zotero_test_response(child_page, headers = list(`Total-Results` = "2")))
    }
    .zotero_test_response(article)
  })
  result <- zotero_get_item(config, "ABCD1234")
  expect_identical(result$item$key, "ABCD1234")
  expect_identical(vapply(result$children, `[[`, character(1), "key"), c("CHILD234", "ATTACH12"))
  expect_identical(starts, c(0, 1))
  expect_identical(result$context$library_id, "123")

  skipped_calls <- 0L
  note_config <- zotero_config(.transport = function(request) {
    skipped_calls <<- skipped_calls + 1L
    .zotero_test_response(.zotero_test_fixture("note.json"))
  })
  note_result <- zotero_get_item(note_config, "NOTE1234")
  expect_identical(note_result$children, list())
  expect_identical(skipped_calls, 1L)

  bounded_config <- zotero_config(library_id = "123", max_pages = 1L, .transport = function(request) {
    if (grepl("/children$", request$url)) {
      return(.zotero_test_response(list(child), headers = list(`Total-Results` = "2")))
    }
    .zotero_test_response(article)
  })
  .zotero_test_error("LIMIT_EXCEEDED", zotero_get_item(bounded_config, "ABCD1234"))
})
