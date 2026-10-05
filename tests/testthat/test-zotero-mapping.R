# F1's shared error helper is not part of this isolated F2 snapshot. Compile
# the reserved mapping source in a private environment with a test-only helper
# until integration supplies `.zotero_abort`; production code always delegates
# errors to that common helper.
.zotero_mapping_namespace <- asNamespace("litreviewR")
if (exists(".zotero_abort", envir = .zotero_mapping_namespace, inherits = FALSE)) {
  .zotero_normalize_for_test <- get("zotero_normalize_item", envir = .zotero_mapping_namespace)
} else {
  .zotero_mapping_test_env <- new.env(parent = .zotero_mapping_namespace)
  .zotero_mapping_test_env$.zotero_abort <- function(code, message, retryable = FALSE, details = NULL) {
    condition <- structure(
      list(message = message, call = NULL, code = code,
           retryable = retryable, details = details),
      class = c("zotero_error", "error", "condition")
    )
    stop(condition)
  }
  sys.source(testthat::test_path("..", "..", "R", "zotero-mapping.R"),
             envir = .zotero_mapping_test_env)
  .zotero_normalize_for_test <- .zotero_mapping_test_env$zotero_normalize_item
}

.zotero_mapping_fixture <- function(name) {
  jsonlite::fromJSON(
    testthat::test_path("fixtures", "zotero", "mapping", name),
    simplifyVector = FALSE
  )
}

.zotero_mapping_context <- function(backend = "local", library_type = "user",
                                    library_id = "123", server_id = "server-1",
                                    instance_id = NULL) {
  list(backend = backend, library_type = library_type, library_id = library_id,
       server_id = server_id, instance_id = instance_id)
}

.zotero_mapping_error <- function(expr) {
  tryCatch(force(expr), error = identity)
}

test_that("normaliza item Zotero e preserva origem e autores institucionais", {
  item <- .zotero_mapping_fixture("journal-article.json")
  context <- .zotero_mapping_context(library_id = "0")

  article <- .zotero_normalize_for_test(item, context)

  expect_identical(article$article_id, "zotero_user_123_ABCD1234")
  expect_identical(article$id, article$article_id)
  expect_identical(article$id_source, "zotero")
  expect_identical(article$source_key, "zotero:ABCD1234")
  expect_identical(article$title, "Pesquisa sintética")
  expect_identical(article$author, item$data$creators)
  expect_identical(article$author[[1]], list(creatorType = "author", name = "Instituto Exemplo"))
  expect_null(article$author[[1]]$firstName)
  expect_null(article$author[[1]]$lastName)
  expect_identical(article$date_original, "2024-05")
  expect_identical(article$year, "2024")
  expect_identical(article$doi, "10.1000/example")
  expect_identical(article$journal, "Revista Sintética")
  expect_identical(article$abstract, "Resumo de teste.")
  expect_identical(article$url, "https://example.org/article")
  expect_identical(article$entry_type, "journalArticle")
  expect_identical(article$tags, list("piloto", "revisao"))
  expect_identical(article$collections, list("COLL1234"))
  expect_identical(article$duplicate_candidates, character())
  expect_identical(article$provenance$backend, "local")
  expect_identical(article$provenance$library, list(type = "user", id = "123"))
  expect_identical(article$provenance$key, "ABCD1234")
  expect_identical(article$provenance$version, item$version)
  expect_identical(article$provenance$server_id, "server-1")
  expect_identical(article$raw, item)
  expect_true(contrato_validar(article, "article")$valid)
})

test_that("usa identidade local SHA-256 sem persistir o alias 0", {
  item <- .zotero_mapping_fixture("incomplete-date.json")
  context <- .zotero_mapping_context(library_id = "0", server_id = "stable-server",
                                     instance_id = "user-instance")
  expected_hash <- digest::digest("stable-server", algo = "sha256", serialize = FALSE)

  article <- .zotero_normalize_for_test(item, context)

  expect_identical(article$article_id,
                   paste("zotero", "local", expected_hash, "user", "EFGH5678", sep = "_"))
  expect_identical(article$provenance$library, list(type = "user", id = NULL))
  expect_null(article$year)
  expect_null(article$doi)
  expect_identical(article$author[[1]]$name, "Centro de Pesquisa")
})

test_that("usa instance_id quando server_id não foi observado", {
  item <- .zotero_mapping_fixture("incomplete-date.json")
  context <- .zotero_mapping_context(library_id = NULL, server_id = NULL,
                                     instance_id = "persisted-instance")
  expected_hash <- digest::digest("persisted-instance", algo = "sha256", serialize = FALSE)

  article <- .zotero_normalize_for_test(item, context)

  expect_identical(article$article_id,
                   paste("zotero", "local", expected_hash, "user", "EFGH5678", sep = "_"))
})

test_that("usa ID real de grupo e não aceita biblioteca divergente", {
  item <- .zotero_mapping_fixture("journal-article.json")
  item$library <- list(type = "group", id = 456)
  context <- .zotero_mapping_context(library_type = "group", library_id = "456")
  article <- .zotero_normalize_for_test(item, context)
  expect_identical(article$article_id, "zotero_group_456_ABCD1234")

  error <- .zotero_mapping_error(
    .zotero_normalize_for_test(item, .zotero_mapping_context(library_type = "group", library_id = "999"))
  )
  expect_s3_class(error, "zotero_error")
  expect_identical(error$code, "VERSION_CONFLICT")
})

test_that("canonicaliza zeros à esquerda nos IDs de contexto", {
  item <- .zotero_mapping_fixture("journal-article.json")
  with_leading_zeros <- .zotero_normalize_for_test(
    item, .zotero_mapping_context(library_id = "00123")
  )
  expect_identical(with_leading_zeros$article_id, "zotero_user_123_ABCD1234")
  expect_identical(with_leading_zeros$provenance$library$id, "123")

  fallback_item <- .zotero_mapping_fixture("incomplete-date.json")
  zero_alias <- .zotero_normalize_for_test(
    fallback_item,
    .zotero_mapping_context(library_id = "00", server_id = "stable-server")
  )
  expected_hash <- digest::digest("stable-server", algo = "sha256", serialize = FALSE)
  expect_identical(zero_alias$article_id,
                   paste("zotero", "local", expected_hash, "user", "EFGH5678", sep = "_"))
})

test_that("não infere ano em intervalo ambíguo e omite nota e anexo", {
  item <- .zotero_mapping_fixture("incomplete-date.json")
  article <- .zotero_normalize_for_test(item, .zotero_mapping_context(library_id = NULL))
  expect_null(article$year)
  expect_null(article$doi)

  item$data$itemType <- "attachment"
  expect_null(.zotero_normalize_for_test(item, .zotero_mapping_context(library_id = NULL)))
  item$data$itemType <- "note"
  expect_null(.zotero_normalize_for_test(item, .zotero_mapping_context(library_id = NULL)))
})

test_that("um título ausente é normalizado como NULL", {
  item <- .zotero_mapping_fixture("journal-article.json")
  item$data$title <- NULL
  article <- .zotero_normalize_for_test(item, .zotero_mapping_context(library_id = "123"))
  expect_null(article$title)
})

test_that("falhas de identidade e wrappers inválidos usam erros Zotero", {
  item <- .zotero_mapping_fixture("incomplete-date.json")
  context <- .zotero_mapping_context(library_id = NULL, server_id = NULL, instance_id = NULL)
  error <- .zotero_mapping_error(.zotero_normalize_for_test(item, context))
  expect_s3_class(error, "zotero_error")
  expect_identical(error$code, "IDENTITY_UNRESOLVED")

  item$key <- "bad-key"
  error <- .zotero_mapping_error(
    .zotero_normalize_for_test(item, .zotero_mapping_context(library_id = "123"))
  )
  expect_s3_class(error, "zotero_error")
  expect_identical(error$code, "INVALID_RESPONSE")
})

test_that("data$version malformada produz erro estruturado Zotero", {
  item <- .zotero_mapping_fixture("journal-article.json")
  for (bad_version in list(list(7), NA_real_, -1)) {
    item$data$version <- bad_version
    error <- .zotero_mapping_error(
      .zotero_normalize_for_test(item, .zotero_mapping_context(library_id = "123"))
    )
    expect_s3_class(error, "zotero_error")
    expect_identical(error$code, "INVALID_RESPONSE")
  }
})
