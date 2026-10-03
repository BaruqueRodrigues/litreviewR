test_that("o esquema geral contém 14 dimensões válidas", {
  schema <- schema_padrao()
  expect_true(schema_validar(schema)$valid)
  expect_length(schema$dimensions, 14L)
  ids <- vapply(schema$dimensions, `[[`, character(1), "id")
  expect_length(unique(ids), 14L)
})

test_that("JSON round-trip preserva IDs, listas e tipos dos campos", {
  schema <- schema_padrao()
  schema$dimensions[[1]]$search_terms <- list("pergunta", "questão")
  schema$dimensions[[1]]$evidence_required <- FALSE
  schema$dimensions[[2]]$active <- FALSE
  decoded <- schema_de_json(schema_para_json(schema))
  expect_identical(decoded$schema_id, schema$schema_id)
  expect_identical(decoded$dimensions[[1]]$id, schema$dimensions[[1]]$id)
  expect_identical(decoded$dimensions[[1]]$search_terms, schema$dimensions[[1]]$search_terms)
  expect_type(decoded$dimensions[[1]]$evidence_required, "logical")
  expect_type(decoded$dimensions[[2]]$active, "logical")
  expect_true(schema_validar(decoded)$valid)
})

test_that("esquema de arquivo permite salvar e reler a mesma configuração", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  schema <- schema_padrao()
  schema$area <- "Sociologia"
  schema_escrever(schema, path)
  reopened <- schema_ler(path)
  expect_identical(reopened$area, "Sociologia")
  expect_identical(vapply(reopened$dimensions, `[[`, character(1), "id"),
                   vapply(schema$dimensions, `[[`, character(1), "id"))
  bundled_path <- system.file("schemas", "ciencias-sociais.v1.json", package = "litreviewR")
  if (!nzchar(bundled_path)) bundled_path <- testthat::test_path("..", "..", "inst", "schemas", "ciencias-sociais.v1.json")
  bundled <- schema_ler(bundled_path)
  expect_true(schema_validar(bundled)$valid)
})

test_that("duplicatas, referências inválidas e operadores desconhecidos são rejeitados", {
  schema <- schema_padrao()
  schema$dimensions[[2]]$id <- schema$dimensions[[1]]$id
  expect_false(schema_validar(schema)$valid)

  schema <- schema_padrao()
  schema$dimensions[[2]]$applicability$all <- list(list(
    dimension_id = "missing-dimension", operator = "equals", value = "x"
  ))
  expect_false(schema_validar(schema)$valid)
  schema$dimensions[[2]]$applicability$all[[1]]$operator <- "run_code"
  expect_false(schema_validar(schema)$valid)

  schema <- schema_padrao()
  schema$dimensions[[2]]$applicability$all <- list(list(
    dimension_id = "research_question", operator = "equals"
  ))
  expect_false(schema_validar(schema)$valid)

  schema <- schema_padrao()
  schema$dimensions[[1]]$type <- "single_category"
  schema$dimensions[[1]]$categories <- list(
    list(id = "same", label = "Um", description = "Primeira"),
    list(id = "same", label = "Dois", description = "Segunda")
  )
  expect_false(schema_validar(schema)$valid)
})

test_that("validador retorna erros para entradas parciais ou malformadas", {
  schema <- schema_padrao()
  schema$dimensions[[1]]$type <- NULL
  schema$dimensions[[2]]$scope <- NA_character_
  schema$dimensions[[3]]$inference_policy <- 1
  schema$dimensions[[4]]$operations <- list(NA_character_)
  schema$dimensions[[5]]$applicability$all <- list("malformed")
  expect_no_error(result <- schema_validar(schema))
  expect_false(result$valid)
  expect_gt(length(result$errors), 0L)

  expect_no_error(partial <- schema_validar(list(schema_id = "partial", dimensions = list("bad"))))
  expect_false(partial$valid)

  schema <- schema_padrao()
  schema$dimensions[[1]]$type <- "single_category"
  schema$dimensions[[1]]$categories <- list("malformed")
  expect_no_error(categories <- schema_validar(schema))
  expect_false(categories$valid)
})

test_that("contratos de fixture aceitam estados distintos e evidências", {
  fixture_dir <- testthat::test_path("fixtures", "contratos")
  types <- c("article", "document", "study", "passage", "evidence", "response",
             "response_ambiguous", "response_conflicting", "response_error", "run",
             "acquisition_attempt", "api_call")
  for (name in types) {
    type <- switch(name, response_ambiguous = "response", response_conflicting = "response",
                   response_error = "response", name)
    obj <- jsonlite::fromJSON(file.path(fixture_dir, paste0(name, ".json")), simplifyVector = FALSE)
    expect_true(contrato_validar(obj, type)$valid, info = name)
  }
  invalid_types <- c("article", "document", "study", "passage", "evidence", "response",
                     "run", "acquisition_attempt", "api_call")
  for (type in invalid_types) {
    obj <- jsonlite::fromJSON(file.path(fixture_dir, paste0(type, "-invalid.json")), simplifyVector = FALSE)
    expect_false(contrato_validar(obj, type)$valid, info = paste(type, "invalid"))
  }
  passage <- jsonlite::fromJSON(file.path(fixture_dir, "passage.json"), simplifyVector = FALSE)
  evidence <- jsonlite::fromJSON(file.path(fixture_dir, "evidence.json"), simplifyVector = FALSE)
  expect_true(contrato_validar(evidence, "evidence", corpus = list(passage))$valid)
  evidence$quote <- "citação que não existe"
  expect_false(contrato_validar(evidence, "evidence", corpus = list(passage))$valid)
  api_call <- jsonlite::fromJSON(file.path(fixture_dir, "api_call.json"), simplifyVector = FALSE)
  api_call$parameters$access_token <- "never-store-this"
  expect_false(contrato_validar(api_call, "api_call")$valid)
})

test_that("resposta consulta tipo, categoria e política da dimensão", {
  schema <- schema_padrao()
  schema$dimensions[[1]]$inference_policy <- "allow_inference"
  schema$dimensions[[1]]$type <- "multiple_categories"
  schema$dimensions[[1]]$categories <- list(
    list(id = "theme_a", label = "Tema A", description = "Descrição A"),
    list(id = "theme_b", label = "Tema B", description = "Descrição B")
  )
  schema$dimensions[[1]]$operations <- list("classify")
  response <- list(
    run_id = "run-001", schema_id = schema$schema_id, revision = 1L,
    dimension_id = "research_question", unit_type = "article", unit_id = "art-001",
    value = list("theme_a", "theme_b"), status = "extraido", value_origin = "inferred",
    evidence_ids = list("ev1", "ev2"), human_review = list(status = "pending")
  )
  expect_true(contrato_validar(response, "response", schema = schema)$valid)
  response$value <- list("unknown")
  expect_false(contrato_validar(response, "response", schema = schema)$valid)
})
