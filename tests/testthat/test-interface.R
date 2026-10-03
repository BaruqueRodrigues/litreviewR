test_that("prévia exibe instruções ativas e omite dimensões inativas", {
  schema <- schema_padrao()
  schema$dimensions[[1]]$instruction <- "Registrar literalmente"
  schema$dimensions[[2]]$active <- FALSE
  preview <- build_schema_preview(schema)
  expect_match(preview, "Registrar literalmente", fixed = TRUE)
  expect_false(grepl("Objetivo declarado", preview, fixed = TRUE))
})

test_that("fixture de interface é válida e preserva categorias no JSON", {
  path <- testthat::test_path("fixtures", "interface", "schema-minimo.json")
  schema <- schema_ler(path)
  expect_true(schema_validar(schema)$valid)
  roundtrip <- schema_de_json(schema_para_json(schema))
  expect_identical(roundtrip$dimensions[[1]]$categories[[1]]$id, "institutions")
  expect_identical(roundtrip$dimensions[[1]]$operations, list("classify"))
})

test_that("configura_revisao devolve app sem iniciar servidor", {
  skip_if_not_installed("shiny")
  app <- configura_revisao(launch = FALSE)
  expect_s3_class(app, "shiny.appobj")
})
