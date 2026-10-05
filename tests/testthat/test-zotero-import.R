.zotero_import_fixture <- function(name) {
  jsonlite::fromJSON(
    test_path("fixtures", "zotero", "import", name),
    simplifyVector = FALSE
  )
}

.zotero_import_transport <- function(items, children = list(), file_urls = list(),
                                    versions = NULL, library_version = "7",
                                    server_id = "synthetic-instance") {
  state <- new.env(parent = emptyenv())
  state$items <- items
  state$children <- children
  state$file_urls <- file_urls
  state$versions <- versions
  state$library_version <- library_version
  state$server_id <- server_id
  state$calls <- list()

  transport <- function(request) {
    state$calls[[length(state$calls) + 1L]] <- request
    path <- sub("^http://localhost:23119/api/", "", request$url)
    headers <- list(
      "zotero-api-version" = "3",
      "zotero-server-id" = state$server_id
    )
    start <- if (is.null(request$query$start)) 0 else as.numeric(request$query$start)
    version <- state$library_version
    if (!is.null(state$versions) && !is.null(names(state$versions)) &&
        as.character(start) %in% names(state$versions)) {
      version <- state$versions[[as.character(start)]]
    }
    if (!is.null(version)) headers[["last-modified-version"]] <- as.character(version)

    encode <- function(value) as.character(jsonlite::toJSON(
      value, auto_unbox = TRUE, null = "null", digits = NA, force = TRUE
    ))
    if (grepl("/collections/[A-Z0-9]{8}/items/top$", path)) {
      headers[["total-results"]] <- as.character(length(state$items))
      limit <- as.integer(request$query$limit)
      if (start >= length(state$items)) {
        payload <- list()
      } else {
        last <- min(length(state$items), start + limit)
        payload <- state$items[seq.int(start + 1, last)]
      }
      return(list(status = 200L, headers = headers, body = encode(payload)))
    }

    child_match <- regexec("^users/[0-9]+/items/([A-Z0-9]{8})/children$", path)
    child_parts <- regmatches(path, child_match)[[1L]]
    if (length(child_parts)) {
      key <- child_parts[[2L]]
      payload <- state$children[[key]]
      if (is.null(payload)) payload <- list()
      headers[["total-results"]] <- as.character(length(payload))
      limit <- as.integer(request$query$limit)
      if (start >= length(payload)) payload <- list() else {
        last <- min(length(payload), start + limit)
        payload <- payload[seq.int(start + 1, last)]
      }
      return(list(status = 200L, headers = headers, body = encode(payload)))
    }

    file_match <- regexec("^users/[0-9]+/items/([A-Z0-9]{8})/file/view/url$", path)
    file_parts <- regmatches(path, file_match)[[1L]]
    if (length(file_parts)) {
      url <- state$file_urls[[file_parts[[2L]]]]
      if (is.null(url)) return(list(status = 404L, headers = headers, body = "{}"))
      return(list(status = 200L, headers = headers, body = url))
    }

    item_match <- regexec("^users/[0-9]+/items/([A-Z0-9]{8})$", path)
    item_parts <- regmatches(path, item_match)[[1L]]
    if (length(item_parts)) {
      key <- item_parts[[2L]]
      parent <- Filter(function(item) identical(item$key, key), state$items)
      if (!length(parent)) return(list(status = 404L, headers = headers, body = "{}"))
      return(list(status = 200L, headers = headers, body = encode(parent[[1L]])))
    }
    list(status = 404L, headers = headers, body = "{}")
  }
  list(transport = transport, state = state)
}

.zotero_import_pdf_file <- function(text = "Synthetic Zotero PDF") {
  path <- tempfile("zotero-source-", fileext = ".pdf")
  grDevices::pdf(path, width = 3, height = 3)
  graphics::plot.new()
  graphics::text(0.5, 0.5, text)
  grDevices::dev.off()
  path
}

.zotero_import_file_url <- function(path) {
  paste0("file://", utils::URLencode(normalizePath(path, winslash = "/"), reserved = FALSE))
}

.zotero_import_error_code <- function(expr) {
  tryCatch({
    force(expr)
    NULL
  }, zotero_error = function(error) error$code)
}

.zotero_import_config <- function(items, children = list(), file_urls = list(),
                                  versions = NULL, library_version = "7",
                                  library_id = "0", max_pages = 1000L,
                                  max_file_bytes = 104857600) {
  fixture <- .zotero_import_transport(items, children, file_urls, versions,
                                      library_version)
  config <- zotero_config(
    library_id = library_id,
    max_pages = max_pages,
    max_file_bytes = max_file_bytes,
    .transport = fixture$transport
  )
  list(config = config, state = fixture$state)
}

test_that("dry-run stays read-only and metadata-only skips all child requests", {
  item <- .zotero_import_fixture("article.json")
  note <- .zotero_import_fixture("note.json")
  fixture <- .zotero_import_config(list(item, note))
  root <- tempfile("zotero-dry-run-")
  before <- list.files(dirname(root), all.files = TRUE, no.. = TRUE)

  planned <- zotero_import_collection(fixture$config, "COLL1234", root)

  expect_true(planned$dry_run)
  expect_false(dir.exists(root))
  expect_false(file.exists(file.path(root, "manifest.json")))
  expect_identical(planned$context$library_id, "123")
  expect_identical(planned$articles[[1]]$article_id, "zotero_user_123_ABCD1234")
  expect_equal(planned$counts$imported, 1L)
  expect_false(any(vapply(fixture$state$calls, function(request) {
    grepl("/children$|/file/view/url$", request$url)
  }, logical(1))))
  expect_setequal(list.files(dirname(root), all.files = TRUE, no.. = TRUE), before)
})

test_that("valid PDFs are copied idempotently, versioned on change, and retained by metadata-only imports", {
  testthat::skip_if_not_installed("pdftools")
  item <- .zotero_import_fixture("article.json")
  note <- .zotero_import_fixture("note.json")
  attachment <- .zotero_import_fixture("attachment-pdf.json")
  source_one <- .zotero_import_pdf_file("First source version")
  source_two <- .zotero_import_pdf_file("Second source version")
  on.exit(unlink(c(source_one, source_two), force = TRUE), add = TRUE)
  fixture <- .zotero_import_config(
    list(item, note),
    children = list(ABCD1234 = list(attachment)),
    file_urls = list(PDF12345 = .zotero_import_file_url(source_one))
  )
  root <- tempfile("zotero-pdf-corpus-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  first_hash <- digest::digest(source_one, algo = "sha256", file = TRUE, serialize = FALSE)

  first <- zotero_import_collection(fixture$config, "COLL1234", root,
                                    include_pdfs = TRUE, dry_run = FALSE)
  document <- first$documents[[1L]]
  expect_identical(document$acquisition_status, "acquired")
  expect_true(file.exists(document$path))
  expect_identical(document$sha256, first_hash)
  expect_false(identical(normalizePath(document$path), normalizePath(source_one)))
  expect_true(file.exists(file.path(root, document$relative_path)))
  expect_false(dir.exists(file.path(root, ".zotero-import.lock")))

  second <- zotero_import_collection(fixture$config, "COLL1234", root,
                                     include_pdfs = TRUE, dry_run = FALSE)
  expect_identical(second$counts$unchanged, 1L)
  expect_identical(second$documents[[1L]]$path, document$path)
  expect_identical(digest::digest(source_one, algo = "sha256", file = TRUE, serialize = FALSE), first_hash)

  calls_before_metadata <- length(fixture$state$calls)
  metadata <- zotero_import_collection(fixture$config, "COLL1234", root, dry_run = FALSE)
  expect_identical(metadata$counts$unchanged, 1L)
  expect_identical(metadata$documents[[1L]]$path, document$path)
  expect_true(file.exists(metadata$documents[[1L]]$path))
  expect_false(any(vapply(fixture$state$calls[(calls_before_metadata + 1L):length(fixture$state$calls)],
                          function(request) grepl("/children$|/file/view/url$", request$url),
                          logical(1))))

  fixture$state$file_urls[["PDF12345"]] <- .zotero_import_file_url(source_two)
  fixture$state$items[[1L]]$version <- fixture$state$items[[1L]]$version + 1L
  fixture$state$items[[1L]]$data$version <- fixture$state$items[[1L]]$version
  source_two_hash <- digest::digest(source_two, algo = "sha256", file = TRUE, serialize = FALSE)
  updated <- zotero_import_collection(fixture$config, "COLL1234", root,
                                      include_pdfs = TRUE, dry_run = FALSE)
  expect_identical(updated$counts$unchanged, 1L)
  expect_identical(updated$documents[[1L]]$sha256, source_two_hash)
  expect_false(identical(updated$documents[[1L]]$path, document$path))
  expect_match(updated$documents[[1L]]$relative_path, paste0("-", source_two_hash, "\\.pdf$"))
  expect_true(file.exists(document$path))
  expect_identical(digest::digest(document$path, algo = "sha256", file = TRUE, serialize = FALSE), first_hash)
  expect_true(file.exists(updated$documents[[1L]]$path))
  expect_false(dir.exists(file.path(root, ".zotero-import.lock")))
})

test_that("attachment failures are recorded without blocking other PDFs", {
  testthat::skip_if_not_installed("pdftools")
  item <- .zotero_import_fixture("article.json")
  good <- .zotero_import_fixture("attachment-pdf.json")
  missing <- good
  missing$key <- "MISS1234"
  missing$data$key <- "MISS1234"
  corrupt <- good
  corrupt$key <- "BADPDF12"
  corrupt$data$key <- "BADPDF12"
  source <- .zotero_import_pdf_file()
  corrupt_source <- tempfile("not-a-pdf-")
  writeLines("not a PDF", corrupt_source)
  on.exit(unlink(c(source, corrupt_source), force = TRUE), add = TRUE)
  fixture <- .zotero_import_config(
    list(item),
    children = list(ABCD1234 = list(good, missing, corrupt)),
    file_urls = list(PDF12345 = .zotero_import_file_url(source),
                     BADPDF12 = .zotero_import_file_url(corrupt_source))
  )
  root <- tempfile("zotero-partial-corpus-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  result <- zotero_import_collection(fixture$config, "COLL1234", root,
                                    include_pdfs = TRUE, dry_run = FALSE)

  expect_equal(result$counts$failed, 1L)
  expect_equal(result$counts$not_available, 1L)
  expect_equal(result$counts$imported, 1L)
  statuses <- vapply(result$documents, `[[`, character(1), "acquisition_status")
  expect_true("acquired" %in% statuses)
  expect_true("not_available" %in% statuses)
  expect_true("failed" %in% statuses)
  expect_true(file.exists(result$documents[[which(statuses == "acquired")[[1L]]]]$path))
  expect_false(dir.exists(file.path(root, ".zotero-import.lock")))
})

test_that("local user alias zero resolves from wrappers and never enters persisted identity", {
  item <- .zotero_import_fixture("article.json")
  fixture <- .zotero_import_config(list(item))
  root <- tempfile("zotero-alias-zero-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  result <- zotero_import_collection(fixture$config, "COLL1234", root, dry_run = FALSE)
  persisted <- paste(readLines(file.path(root, "manifest.json"), warn = FALSE), collapse = "\n")

  expect_identical(result$context$library_id, "123")
  expect_identical(result$articles[[1L]]$article_id, "zotero_user_123_ABCD1234")
  expect_false(grepl("zotero_user_0_", persisted, fixed = TRUE))
  expect_false(grepl('"library_id":"0"', persisted, fixed = TRUE))
})

test_that("duplicate DOI or title candidates are recorded without merging articles", {
  first <- .zotero_import_fixture("article.json")
  second <- first
  second$key <- "EFGH5678"
  second$data$key <- "EFGH5678"
  second$data$title <- "Outra redacao"
  fixture <- .zotero_import_config(list(first, second))
  root <- tempfile("zotero-duplicates-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  result <- zotero_import_collection(fixture$config, "COLL1234", root)

  expect_length(result$articles, 2L)
  expect_true(result$articles[[2L]]$article_id %in% result$articles[[1L]]$duplicate_candidates)
  expect_true(result$articles[[1L]]$article_id %in% result$articles[[2L]]$duplicate_candidates)
})

test_that("filtered pages advance by validated raw cursors and version changes abort before commit", {
  notes <- lapply(seq_len(100L), function(i) {
    note <- .zotero_import_fixture("note.json")
    note$key <- sprintf("N%07d", i)
    note$data$key <- note$key
    note
  })
  article <- .zotero_import_fixture("article.json")
  items <- c(notes, list(article))
  changed <- .zotero_import_config(items, versions = c("0" = "7", "100" = "8"))
  changed_root <- tempfile("zotero-version-conflict-")
  on.exit(unlink(changed_root, recursive = TRUE), add = TRUE)
  expect_identical(
    .zotero_import_error_code(zotero_import_collection(changed$config, "COLL1234", changed_root,
                                                       dry_run = FALSE)),
    "VERSION_CONFLICT"
  )
  expect_false(dir.exists(changed_root))

  limited <- .zotero_import_config(items, max_pages = 1L)
  limited_root <- tempfile("zotero-page-limit-")
  on.exit(unlink(limited_root, recursive = TRUE), add = TRUE)
  expect_identical(
    .zotero_import_error_code(zotero_import_collection(limited$config, "COLL1234", limited_root,
                                                       dry_run = FALSE)),
    "LIMIT_EXCEEDED"
  )
  expect_false(dir.exists(limited_root))
})

test_that("corrupt and foreign manifests are preserved and refused", {
  item <- .zotero_import_fixture("article.json")
  fixture <- .zotero_import_config(list(item))
  root <- tempfile("zotero-corrupt-manifest-")
  dir.create(root)
  manifest_path <- file.path(root, "manifest.json")
  writeBin(charToRaw("{broken"), manifest_path)
  original <- readBin(manifest_path, "raw", n = file.info(manifest_path)$size)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  expect_identical(.zotero_import_error_code(
    zotero_import_collection(fixture$config, "COLL1234", root, dry_run = FALSE)
  ), "VERSION_CONFLICT")
  expect_identical(readBin(manifest_path, "raw", n = file.info(manifest_path)$size), original)
  expect_false(dir.exists(file.path(root, ".zotero-import.lock")))

  clean_root <- tempfile("zotero-foreign-manifest-")
  on.exit(unlink(clean_root, recursive = TRUE), add = TRUE)
  zotero_import_collection(fixture$config, "COLL1234", clean_root, dry_run = FALSE)
  clean_manifest <- readBin(file.path(clean_root, "manifest.json"), "raw",
                            n = file.info(file.path(clean_root, "manifest.json"))$size)
  foreign <- .zotero_import_fixture("article.json")
  foreign$library$id <- 456L
  foreign_fixture <- .zotero_import_config(list(foreign), library_id = "456")
  expect_identical(.zotero_import_error_code(
    zotero_import_collection(foreign_fixture$config, "COLL1234", clean_root, dry_run = FALSE)
  ), "VERSION_CONFLICT")
  expect_identical(readBin(file.path(clean_root, "manifest.json"), "raw",
                           n = file.info(file.path(clean_root, "manifest.json"))$size), clean_manifest)
  expect_false(dir.exists(file.path(clean_root, ".zotero-import.lock")))
})

test_that("symlink roots, descendants and locks are rejected without following or deleting them", {
  testthat::skip_on_os("windows")
  item <- .zotero_import_fixture("article.json")
  fixture <- .zotero_import_config(list(item))

  outside <- tempfile("zotero-outside-")
  dir.create(outside)
  root_link <- tempfile("zotero-root-link-")
  expect_true(file.symlink(outside, root_link))
  expect_identical(.zotero_import_error_code(
    zotero_import_collection(fixture$config, "COLL1234", root_link, dry_run = FALSE)
  ), "INVALID_ARGUMENT")
  expect_length(list.files(outside, all.files = TRUE, no.. = TRUE), 0L)
  unlink(root_link)

  corpus <- tempfile("zotero-descendant-link-")
  dir.create(corpus)
  pdf_link <- file.path(corpus, "pdfs")
  expect_true(file.symlink(outside, pdf_link))
  expect_identical(.zotero_import_error_code(
    zotero_import_collection(fixture$config, "COLL1234", corpus, dry_run = FALSE)
  ), "VERSION_CONFLICT")
  expect_length(list.files(outside, all.files = TRUE, no.. = TRUE), 0L)
  unlink(corpus, recursive = TRUE)

  lock_corpus <- tempfile("zotero-lock-attack-")
  dir.create(lock_corpus)
  lock_path <- file.path(lock_corpus, ".zotero-import.lock")
  dir.create(lock_path)
  sentinel <- file.path(lock_path, "keep.txt")
  writeLines("owned by another process", sentinel)
  expect_identical(.zotero_import_error_code(
    zotero_import_collection(fixture$config, "COLL1234", lock_corpus, dry_run = FALSE)
  ), "CORPUS_LOCKED")
  expect_true(file.exists(sentinel))
  expect_false(file.exists(file.path(lock_corpus, "manifest.json")))
  unlink(c(outside, lock_corpus), recursive = TRUE)
})

test_that("an oversized prior corpus PDF is preserved without hashing or replacing it", {
  testthat::skip_if_not_installed("pdftools")
  item <- .zotero_import_fixture("article.json")
  attachment <- .zotero_import_fixture("attachment-pdf.json")
  source <- .zotero_import_pdf_file()
  on.exit(unlink(source, force = TRUE), add = TRUE)
  fixture <- .zotero_import_config(
    list(item), children = list(ABCD1234 = list(attachment)),
    file_urls = list(PDF12345 = .zotero_import_file_url(source))
  )
  root <- tempfile("zotero-size-limit-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  original <- zotero_import_collection(fixture$config, "COLL1234", root,
                                      include_pdfs = TRUE, dry_run = FALSE)
  old_document <- original$documents[[1L]]
  old_hash <- old_document$sha256
  fixture$config$max_file_bytes <- 1
  digest_calls <- 0L
  testthat::local_mocked_bindings(
    .zotero_import_digest = function(path) {
      digest_calls <<- digest_calls + 1L
      stop("oversized files must not be hashed")
    },
    .package = "litreviewR"
  )

  result <- zotero_import_collection(fixture$config, "COLL1234", root,
                                     include_pdfs = TRUE, dry_run = FALSE)

  expect_identical(digest_calls, 0L)
  expect_identical(result$counts$failed, 1L)
  expect_identical(result$documents[[1L]]$acquisition_status, "acquired")
  expect_identical(result$documents[[1L]]$path, old_document$path)
  expect_identical(result$documents[[1L]]$sha256, old_hash)
  expect_identical(.zotero_import_existing_pdf_hash(old_document$path, 1)$status, "oversized")
  expect_false(dir.exists(file.path(root, ".zotero-import.lock")))
})
