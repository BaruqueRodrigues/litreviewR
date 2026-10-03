test_that("JEV adapter uses the TypeSafe payload and returns structured values", {
  seen <- NULL
  transport <- function(payload, api_key, endpoint, timeout) {
    seen <<- list(payload = payload, api_key = api_key, endpoint = endpoint, timeout = timeout)
    list(
      model = "jev-1.13.0",
      answers = list(topic = list(
        type = "choice", choice = "institutions", confidence = 0.8,
        probabilities = list(institutions = 0.9, behavior = 0.1)
      )),
      usage = list(input_tokens = 120L, output_tokens = 18L)
    )
  }

  result <- jev_avalia(
    "Texto de exemplo", list(topic = list(type = "choice", instructions = "Classifique.",
                                          criteria = list(institutions = "Instituições", behavior = "Comportamento"))),
    api_key = "test-secret", .transport = transport
  )

  expect_equal(seen$payload$model, "jev-latest")
  expect_equal(seen$payload$state, "Texto de exemplo")
  expect_equal(seen$api_key, "test-secret")
  expect_equal(result$answers$topic$choice, "institutions")
  expect_equal(result$answers$topic$probabilities$institutions, 0.9)
  expect_equal(result$answers$topic$confidence, 0.8)
  expect_equal(result$usage$input_tokens, 120L)
})

test_that("documented provider response fixture has a valid Choice shape", {
  fixture <- jsonlite::fromJSON(testthat::test_path("fixtures", "jev", "choice-response.json"),
                                simplifyVector = FALSE)
  expect_silent(.jev_validate_api_response(fixture, "topic", list(topic = c("institutions", "behavior"))))
})

test_that("missing credentials fail before any transport call", {
  called <- FALSE
  transport <- function(...) { called <<- TRUE; stop("must not run") }
  expect_error(
    jev_avalia("texto", list(q = list()), api_key = "", .transport = transport),
    class = "litreview_jev_error"
  )
  expect_false(called)
})

test_that("JEV rejects answer categories and probability maps outside the request contract", {
  expect_error(
    .jev_validate_api_response(list(answers = list(q = list(
      type = "choice", choice = "invented", confidence = 0.9,
      probabilities = list(allowed = 1)
    ))), "q", list(q = c("allowed", "other"))),
    class = "litreview_jev_error"
  )
  expect_error(
    .jev_validate_api_response(list(answers = list(q = list(
      type = "choice", choice = "allowed", confidence = 1.2,
      probabilities = list(allowed = 1)
    ))), "q", list(q = "allowed")),
    class = "litreview_jev_error"
  )
})

test_that("invalid HTTP answers and network failures stay controlled errors", {
  invalid <- function(...) list(model = "jev", answers = list(), usage = list())
  expect_error(
    jev_avalia("texto", list(q = list()), api_key = "dummy", .transport = invalid),
    class = "litreview_jev_error"
  )
  timeout <- function(...) stop("simulated timeout")
  err <- tryCatch(jev_avalia("texto", list(q = list()), api_key = "dummy", .transport = timeout),
                  litreview_jev_error = identity)
  expect_equal(err$code, "transport_error")
  expect_false(grepl("dummy", conditionMessage(err), fixed = TRUE))
})
