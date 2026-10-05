web_test_config <- function(transport) {
  zotero_config(
    backend = "web", library_type = "user", library_id = "123456",
    api_key_env = "LITREVIEW_TEST_ZOTERO_KEY", .transport = transport
  )
}

web_fixture <- function(name) {
  jsonlite::fromJSON(test_path("fixtures", "zotero", "web", name), simplifyVector = FALSE)
}

test_that("Web backend uses the shared HTTP transport and environment credential", {
  withr::local_envvar(c(LITREVIEW_TEST_ZOTERO_KEY = "synthetic-secret"))
  requests <- list()
  config <- web_test_config(function(request) {
    requests[[length(requests) + 1L]] <<- request
    list(status = 200L, headers = list(`Zotero-API-Version` = "3"),
         body = '{"version":"3"}')
  })

  status <- zotero_status(config)

  expect_true(status$available)
  expect_identical(status$backend, "web")
  expect_length(requests, 1L)
  expect_identical(requests[[1L]]$method, "GET")
  expect_identical(requests[[1L]]$url, "https://api.zotero.org/users/123456/items/top")
  expect_identical(requests[[1L]]$headers[["zotero-api-version"]], "3")
  expect_identical(requests[[1L]]$headers[["zotero-api-key"]], "synthetic-secret")
  expect_false("synthetic-secret" %in% unlist(config, use.names = FALSE))
})

test_that("Web backend reuses common pagination, wrappers, and quicksearch", {
  withr::local_envvar(c(LITREVIEW_TEST_ZOTERO_KEY = "synthetic-secret"))
  requests <- list()
  config <- web_test_config(function(request) {
    requests[[length(requests) + 1L]] <<- request
    if (grepl("/collections$", request$url)) {
      body <- web_fixture("collections.json")
    } else {
      body <- web_fixture("items.json")
    }
    list(status = 200L,
         headers = list(`Total-Results` = "3", `Zotero-API-Version` = "3"),
         body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"))
  })

  collections <- zotero_list_collections(config, limit = 2L)
  items <- zotero_search_items(config, query = "review", limit = 2L)

  expect_length(collections$items, 2L)
  expect_identical(collections$next_cursor, "2")
  expect_length(items$items, 2L)
  expect_identical(requests[[2L]]$query$q, "review")
  expect_identical(requests[[2L]]$query$start, 0)
})

test_that("Web errors are typed and never reveal credentials or response bodies", {
  withr::local_envvar(c(LITREVIEW_TEST_ZOTERO_KEY = "secret-do-not-leak"))
  config <- web_test_config(function(request) {
    list(status = 403L, headers = list(), body = "secret-do-not-leak remote payload")
  })

  error <- tryCatch(zotero_status(config), zotero_error = identity)

  expect_identical(error$code, "ACCESS_DENIED")
  expect_false(grepl("secret-do-not-leak", conditionMessage(error), fixed = TRUE))
  expect_false(grepl("remote payload", conditionMessage(error), fixed = TRUE))
})

test_that("Web rate limits honor Backoff and retry only a bounded number of times", {
  withr::local_envvar(c(LITREVIEW_TEST_ZOTERO_KEY = "synthetic-secret"))
  calls <- 0L
  config <- web_test_config(function(request) {
    calls <<- calls + 1L
    if (calls == 1L) {
      list(status = 429L, headers = list(Backoff = "0"), body = "ignored secret")
    } else {
      list(status = 200L, headers = list(`Zotero-API-Version` = "3"), body = '{"version":"3"}')
    }
  })

  expect_true(zotero_status(config)$available)
  expect_identical(calls, 2L)

  limited <- web_test_config(function(request) {
    list(status = 429L, headers = list(`Retry-After` = "0"), body = "ignored secret")
  })
  error <- tryCatch(zotero_status(limited), zotero_error = identity)
  expect_identical(error$code, "RATE_LIMITED")
  expect_true(error$retryable)
})

test_that("Web backend rejects missing credentials before making a request", {
  withr::local_envvar(c(LITREVIEW_TEST_ZOTERO_KEY = NA_character_))
  calls <- 0L
  config <- web_test_config(function(request) {
    calls <<- calls + 1L
    stop("transport must not be called")
  })

  error <- tryCatch(zotero_status(config), zotero_error = identity)
  expect_identical(error$code, "INVALID_CONFIG")
  expect_identical(calls, 0L)
})

test_that("successful Web responses schedule their Backoff before the next request", {
  withr::local_envvar(c(LITREVIEW_TEST_ZOTERO_KEY = "synthetic-secret"))
  waits <- numeric()
  testthat::local_mocked_bindings(
    .zotero_web_wait = function(seconds) waits <<- c(waits, seconds),
    .package = "litreviewR"
  )
  calls <- 0L
  config <- web_test_config(function(request) {
    calls <<- calls + 1L
    headers <- list(`Zotero-API-Version` = "3")
    if (calls == 1L) headers$Backoff <- "5"
    list(status = 200L, headers = headers, body = '{"version":"3"}')
  })

  zotero_status(config)
  zotero_status(config)

  expect_identical(calls, 2L)
  expect_length(waits, 1L)
  expect_gt(waits[[1L]], 0)
  expect_lte(waits[[1L]], 5)
})
