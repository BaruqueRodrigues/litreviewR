.test_pdf <- function(path) {
  grDevices::pdf(path)
  graphics::plot.new()
  graphics::text(.5, .5, "PDF de teste")
  grDevices::dev.off()
  path
}

test_that("download automático trata lista vazia e referência individual", {
  expect_identical(baixa_pdf_auto(list(), delay = 0), list())
  pdf <- .test_pdf(tempfile(fileext = ".pdf"))
  on.exit(unlink(pdf), add = TRUE)
  out <- tempfile(); dir.create(out); on.exit(unlink(out, recursive = TRUE), add = TRUE)
  result <- baixa_pdf_auto(list(id = "one", title = "An article", path = pdf),
                           diretorio = out, delay = 0, fontes = "local")
  expect_length(result, 1L)
  expect_equal(result[[1]]$status, "sucesso")
  expect_true(.is_valid_pdf(result[[1]]$path))
})

test_that("falha de fonte e PDF inválido nunca são reportados como sucesso", {
  out <- tempfile(); dir.create(out); on.exit(unlink(out, recursive = TRUE), add = TRUE)
  html <- tempfile(fileext = ".pdf")
  writeLines("<html>paywall</html>", html)
  on.exit(unlink(html), add = TRUE)
  result <- baixa_pdf_auto(list(id = "bad", title = "Unavailable", path = html),
                           diretorio = out, delay = 0, fontes = "local")
  expect_equal(result[[1]]$status, "erro")
  expect_false(file.exists(file.path(out, "bad.pdf")))
})

test_that("uma referência sem DOI pode usar URL direta fornecida pelo usuário", {
  out <- tempfile(); dir.create(out); on.exit(unlink(out, recursive = TRUE), add = TRUE)
  result <- baixa_pdf_auto(list(id = "no-doi", title = "No DOI"), diretorio = out,
                           delay = 0, fontes = "url")
  expect_equal(result[[1]]$status, "indisponivel")
  expect_equal(result[[1]]$source, "url")
})

test_that("baixa_pdf_auto preserva o resultado de cada fonte tentada", {
  out <- tempfile(); dir.create(out); on.exit(unlink(out, recursive = TRUE), add = TRUE)
  result <- baixa_pdf_auto(list(id = "attempts", title = "No DOI"), diretorio = out,
                           delay = 0, fontes = c("url", "aberto"))[[1]]
  expect_equal(vapply(result$attempts, `[[`, character(1), "source"), c("url", "aberto"))
  expect_equal(vapply(result$attempts, `[[`, character(1), "status"), c("indisponivel", "indisponivel"))
})

test_that("retomada reutiliza PDF válido e não o sobrescreve", {
  out <- tempfile(); dir.create(out); on.exit(unlink(out, recursive = TRUE), add = TRUE)
  good <- .test_pdf(file.path(out, "existing.pdf"))
  before <- unname(tools::md5sum(good))
  result <- baixa_pdf_auto(list(id = "existing", title = "Unavailable"),
                           diretorio = out, delay = 0, fontes = "local")
  expect_equal(result[[1]]$status, "ja_existente")
  expect_equal(unname(tools::md5sum(good)), before)
})

test_that("Sci-Hub tenta domínios configurados sem depender de um domínio fixo", {
  seen <- character()
  failing_get <- function(url, ...) {
    seen <<- c(seen, url)
    stop("simulated offline")
  }
  result <- baixa_pdf_scihub(list(id = "x", title = "Example", doi = "10.1234/x"), delay = 0,
                            urls = c("https://mirror-a.example", "https://mirror-b.example"),
                            .get = failing_get)
  expect_equal(result$status, "erro")
  expect_length(seen, 2L)
  expect_true(all(grepl("mirror", seen)))
})

test_that("Sci-Hub usa iframe quando embed falta e resolve URLs relativas", {
  iframe <- xml2::read_html('<html><body><iframe src="/download/article.pdf"></iframe></body></html>')
  expect_equal(.extrair_url_pdf_scihub(iframe, "https://mirror.example/item/123"),
               "https://mirror.example/download/article.pdf")
  both <- xml2::read_html(paste0(
    '<embed src="/viewer"><iframe src="//cdn.example/paper.pdf?download=1"></iframe>'
  ))
  expect_equal(.extrair_url_pdf_scihub(both, "https://mirror.example/item"),
               "https://cdn.example/paper.pdf?download=1")
})

test_that("PDF local só é válido com assinatura e leitura possível", {
  valid <- .test_pdf(tempfile(fileext = ".pdf"))
  on.exit(unlink(valid), add = TRUE)
  invalid <- tempfile(fileext = ".pdf")
  writeLines("<html>not a PDF</html>", invalid)
  on.exit(unlink(invalid), add = TRUE)
  expect_true(.is_valid_pdf(valid))
  expect_false(.is_valid_pdf(invalid))
})

test_that("HTTP 200 com HTML não é tratado como PDF e não deixa arquivo parcial", {
  out <- tempfile(); dir.create(out); on.exit(unlink(out, recursive = TRUE), add = TRUE)
  mock_get <- function(url, ...) {
    configs <- list(...)
    write_config <- Filter(function(x) is.list(x$output) && !is.null(x$output$path), configs)[[1]]
    writeLines("<html>Access denied</html>", write_config$output$path)
    structure(list(status_code = 200L), class = "response")
  }
  dest <- file.path(out, "article.pdf")
  result <- .baixar_pdf_validado("https://example.test/article.pdf", dest,
                                 "article", "test", .get = mock_get)
  expect_equal(result$status, "erro")
  expect_false(file.exists(dest))
  expect_match(result$reason, "não é um PDF")
})
