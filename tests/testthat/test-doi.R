.mock_crossref_response <- function(items, status = 200L, retry_after = NULL) {
  json <- jsonlite::toJSON(list(message = list(items = items)), auto_unbox = TRUE)
  headers <- list(`content-type` = "application/json")
  if (!is.null(retry_after)) headers[["retry-after"]] <- retry_after
  structure(list(
    url = "https://api.crossref.org/works",
    status_code = as.integer(status),
    headers = headers,
    all_headers = list(), cookies = data.frame(),
    content = charToRaw(as.character(json)), times = NULL,
    config = list(), request = list()
  ), class = "response")
}

test_that("normaliza DOI de URL e prefixo", {
  expect_equal(normaliza_doi("https://doi.org/10.1234/ABC"), "10.1234/ABC")
  expect_equal(normaliza_doi("doi: 10.5678/x.y"), "10.5678/x.y")
  expect_equal(normaliza_doi(NULL), "")
})

test_that("gera_referencias preserva metadados e IDs distinguem chaves BibTeX", {
  bib <- tempfile(fileext = ".bib")
  on.exit(unlink(bib), add = TRUE)
  writeLines(c(
    "@article{paper_a, title={Same title}, author={Doe, Jane}, year={2020}, journal={Journal A}, doi={https://doi.org/10.1000/A}, abstract={A summary}, url={https://example.org/a}}",
    "@article{paper_b, title={Same title}, author={Roe, John}, year={2021}, journal={Journal B}, doi={10.1000/B}}"
  ), bib)
  refs <- gera_referencias(bib)
  expect_length(refs, 2L)
  expect_false(identical(refs[[1]]$id, refs[[2]]$id))
  expect_equal(refs[[1]]$bib_key, "paper_a")
  expect_equal(refs[[1]]$article_id, refs[[1]]$id)
  expect_true(contrato_validar(refs[[1]], "article")$valid)
  expect_equal(refs[[1]]$journal, "Journal A")
  expect_equal(refs[[1]]$abstract, "A summary")
  expect_equal(refs[[1]]$doi, "10.1000/A")
  expect_true(refs[[2]]$id %in% refs[[1]]$duplicate_candidates)
  expect_true(refs[[1]]$id %in% refs[[2]]$duplicate_candidates)
})

test_that("Crossref confirma uma correspondência forte e registra autores e ano", {
  item <- list(DOI = "10.1000/test", title = list("Institutions and democratic change"),
               author = list(list(given = "Jane", family = "Doe")),
               published = list(`date-parts` = list(c(2020, 1, 1))))
  captured <- NULL
  get <- function(url, ..., query) {
    captured <<- list(url = url, query = query)
    .mock_crossref_response(list(item))
  }
  result <- identifica_doi_referencia(
    "Institutions and democratic change", autores = "Jane Doe", ano = 2020,
    mailto = "research@example.org", .get = get, .sleep = function(...) NULL
  )
  expect_equal(result$status, "confirmada")
  expect_equal(result$doi, "10.1000/test")
  expect_equal(captured$query$mailto, "research@example.org")
  expect_gte(result$score, .85)
})

test_that("Crossref conserva ambiguidade e repete HTTP 429", {
  items <- list(
    list(DOI = "10.1000/a", title = list("Institutional change and democracy")),
    list(DOI = "10.1000/b", title = list("Institutional change and democracy"))
  )
  calls <- 0L
  sleeps <- numeric()
  get <- function(...) {
    calls <<- calls + 1L
    if (calls == 1L) .mock_crossref_response(list(), status = 429L, retry_after = "3") else .mock_crossref_response(items)
  }
  result <- identifica_doi_referencia("Institutional change and democracy", .get = get,
                                      .sleep = function(seconds) sleeps <<- c(sleeps, seconds))
  expect_equal(calls, 2L)
  expect_equal(sleeps, 3)
  expect_equal(result$status, "ambigua")
  expect_null(result$doi)
  expect_length(result$candidates, 2L)
})

test_that("falhas HTTP e respostas malformadas não escolhem DOI", {
  get <- function(...) .mock_crossref_response(list(), status = 503L)
  result <- identifica_doi_referencia("A title", .get = get, max_attempts = 1,
                                      .sleep = function(...) NULL)
  expect_equal(result$status, "erro")
  expect_null(descobre_doi_por_titulo("A title", .get = get, max_attempts = 1,
                                      .sleep = function(...) NULL))
})

test_that("buscas Crossref bem sucedidas usam cache de sessão", {
  calls <- 0L
  item <- list(DOI = "10.1000/cached", title = list("Cacheable reference"))
  get <- function(...) {
    calls <<- calls + 1L
    .mock_crossref_response(list(item))
  }
  first <- identifica_doi_referencia("Cacheable reference", .get = get)
  second <- identifica_doi_referencia("Cacheable reference", .get = get)
  expect_equal(first$doi, second$doi)
  expect_equal(calls, 1L)
})
