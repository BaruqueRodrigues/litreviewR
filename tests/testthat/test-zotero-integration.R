# Integration exercises the real F1/F2/F4 pipeline; only HTTP is substituted.
.zotero_pipeline_fixture <- function(pdf_path = NULL, only_notes = FALSE) {
  calls <- list()
  item <- jsonlite::fromJSON(test_path('fixtures','zotero','contracts','item.json'), simplifyVector = FALSE)
  note <- list(key='NOTE1234',version=7L,library=list(type='user',id=123L),
               data=list(key='NOTE1234',version=7L,itemType='note',note='<p>Synthetic note</p>'))
  attachment <- list(key = 'PDFD1234', version = 7L,
    library = list(type = 'user', id = 123L),
    data = list(key = 'PDFD1234', version = 7L, itemType = 'attachment',
      parentItem = 'ABCD1234', contentType = 'application/pdf',
      linkMode = 'imported_file', filename = 'synthetic.pdf'))
  transport <- function(request) {
    calls[[length(calls) + 1L]] <<- request
    path <- sub('^http://localhost:23119/api/', '', request$url)
    headers <- list('zotero-api-version' = '3', 'zotero-server-id' = 'synthetic-instance',
                    'last-modified-version' = '7')
    payload <- if (!nzchar(path)) list() else switch(path,
      'users/0/collections/COLL1234/items/top' = if (only_notes) list(note) else list(item,note),
      'users/0/items/ABCD1234' = item,
      'users/0/items/ABCD1234/children' = list(attachment),
      'users/0/items/PDFD1234/file/view/url' = if (is.null(pdf_path)) NULL else paste0('file://', pdf_path),
      NULL)
    if (is.null(payload)) return(list(status = 404L, headers = headers, body = '{}'))
    if (grepl('/items/top$|/children$', path)) headers[['total-results']] <- as.character(length(payload))
    body <- if (grepl('/file/view/url$',path)) payload else as.character(jsonlite::toJSON(payload, auto_unbox=TRUE, null='null'))
    list(status = 200L, headers = headers, body = body)
  }
  list(config = zotero_config(.transport = transport), calls = function() calls)
}

test_that('real Zotero pipeline imports metadata idempotently without touching dry-run destinations', {
  fixture <- .zotero_pipeline_fixture()
  root <- tempfile('zotero-integration-')
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  dry <- zotero_import_collection(fixture$config,'COLL1234',root)
  expect_false(dir.exists(root))
  expect_true(dry$dry_run)
  expect_identical(dry$articles[[1]]$article_id,'zotero_user_123_ABCD1234')
  expect_true(contrato_validar(dry$articles[[1]],'article')$valid)
  expect_false(any(vapply(fixture$calls(),function(x) grepl('/children$',x$url),logical(1))))
  first <- zotero_import_collection(fixture$config,'COLL1234',root,dry_run=FALSE)
  second <- zotero_import_collection(fixture$config,'COLL1234',root,dry_run=FALSE)
  expect_equal(first$counts$imported,1)
  expect_equal(second$counts$unchanged,1)
  expect_length(second$articles,1L)
  expect_false(dir.exists(file.path(root,'.zotero-import.lock')))
  disk <- jsonlite::fromJSON(file.path(root,'manifest.json'),simplifyVector=FALSE)
  expect_identical(disk$format_version,'1.0.0')
  expect_identical(disk$articles[[1]]$article_id,first$articles[[1]]$article_id)
})

test_that('real Zotero pipeline copies valid PDFs, preserves originals and reuses document identity', {
  original <- tempfile('zotero-original-',fileext='.pdf')
  root <- tempfile('zotero-pdf-corpus-')
  on.exit(unlink(c(original,root),recursive=TRUE),add=TRUE)
  grDevices::pdf(original); graphics::plot.new(); graphics::text(.5,.5,'Synthetic Zotero PDF'); grDevices::dev.off()
  original_hash <- digest::digest(original,algo='sha256',file=TRUE,serialize=FALSE)
  fixture <- .zotero_pipeline_fixture(original)
  first <- zotero_import_collection(fixture$config,'COLL1234',root,include_pdfs=TRUE,dry_run=FALSE)
  expect_length(first$documents,1L)
  document <- first$documents[[1]]
  expect_identical(document$acquisition_status,'acquired')
  expect_true(contrato_validar(document,'document')$valid)
  expect_true(file.exists(document$path))
  expect_false(identical(normalizePath(document$path),normalizePath(original)))
  expect_identical(document$sha256,original_hash)
  second <- zotero_import_collection(fixture$config,'COLL1234',root,include_pdfs=TRUE,dry_run=FALSE)
  expect_identical(second$documents[[1]]$document_id,document$document_id)
  expect_identical(second$documents[[1]]$path,document$path)
  expect_identical(digest::digest(original,algo='sha256',file=TRUE,serialize=FALSE),original_hash)
  metadata <- zotero_import_collection(fixture$config,'COLL1234',root,dry_run=FALSE)
  expect_identical(metadata$documents[[1]]$sha256,original_hash)
})


test_that('note-only collections keep raw pagination identity without becoming articles', {
  fixture <- .zotero_pipeline_fixture(only_notes=TRUE)
  root <- tempfile('zotero-notes-only-')
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  result <- zotero_import_collection(fixture$config,'COLL1234',root,dry_run=FALSE)
  expect_length(result$articles,0L)
  expect_equal(result$counts$imported,0)
  expect_identical(result$context$library_id,'123')
  expect_true(file.exists(file.path(root,'manifest.json')))
})


test_that('shared HTTP errors expose only safe numeric retry hints for the external Web adapter', {
  cfg <- zotero_config(library_id='123', .transport=function(request) {
    list(status=429L,headers=list('Retry-After'='3'),body='REMOTE_BODY_SECRET')
  })
  error <- tryCatch(.zotero_request(cfg,'users/123/items/top'),zotero_error=identity)
  expect_identical(error$code,'RATE_LIMITED')
  expect_equal(error$details$retry_after_seconds,3)
  expect_false(grepl('REMOTE_BODY_SECRET',conditionMessage(error),fixed=TRUE))
  expect_null(.zotero_retry_details(429L,list('retry-after'='UNTRUSTED_SECRET'))$retry_after_seconds)
})

test_that('partial manifest writes preserve the prior commit and release the owned lock', {
  fixture <- .zotero_pipeline_fixture()
  root <- tempfile('zotero-atomic-failure-')
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  zotero_import_collection(fixture$config,'COLL1234',root,dry_run=FALSE)
  manifest_path <- file.path(root,'manifest.json')
  before <- readBin(manifest_path,'raw',n=file.info(manifest_path)$size)
  writer <- .zotero_import_write_manifest
  writer_env <- new.env(parent=environment(writer))
  writer_env$writeLines <- function(text,con,...) {
    base::writeLines(substr(text,1,10),con,...)
    stop('SYNTHETIC_DISK_FAILURE_SECRET')
  }
  environment(writer) <- writer_env
  testthat::local_mocked_bindings(.zotero_import_write_manifest=writer,.package='litreviewR')
  error <- tryCatch(zotero_import_collection(fixture$config,'COLL1234',root,dry_run=FALSE),
                    zotero_error=identity)
  expect_identical(error$code,'WRITE_FAILED')
  expect_false(grepl('SYNTHETIC_DISK_FAILURE_SECRET',conditionMessage(error),fixed=TRUE))
  expect_identical(readBin(manifest_path,'raw',n=file.info(manifest_path)$size),before)
  expect_false(dir.exists(file.path(root,'.zotero-import.lock')))
  expect_length(list.files(root,all.files=TRUE,pattern='^[.]zotero-manifest-'),0L)
})
