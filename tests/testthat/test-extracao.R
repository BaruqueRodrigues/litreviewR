test_that("extrai_dimensoes valida argumentos de entrada", {
  schema <- schema_padrao()
  expect_error(extrai_dimensoes("", schema, list(scope = "article", unit_id = "art-1")),
               class = "litreview_llm_error")
  expect_error(extrai_dimensoes("Texto", list(), list(scope = "article", unit_id = "art-1")),
               class = "litreview_llm_error")
  expect_error(extrai_dimensoes("Texto", schema, list(scope = "outro", unit_id = "art-1")),
               class = "litreview_llm_error")
})

test_that("extrai_dimensoes expõe falhas de API sem registrar a credencial", {
  schema <- schema_padrao()
  schema$dimensions <- list(schema$dimensions[[1]])
  transport <- function(api_key, ...) stop(paste("request failed for", api_key))
  result <- extrai_dimensoes(
    "Texto para teste.", schema, list(scope = "article", unit_id = "art-error"),
    api_key = "secret-test-token", .transport = transport
  )
  expect_equal(result$responses[[1]]$status, "erro_extracao")
  expect_equal(result$errors[["research_question"]]$message,
               "request failed for [REDACTED]")
  expect_false(grepl("secret-test-token", paste(result$errors, collapse = " "), fixed = TRUE))
})

test_that("extrai_dimensoes retorna vazio se nenhuma dimensao de extracao bater com o escopo", {
  schema <- schema_padrao()
  # Por padrão o schema tem escopo article. Testando com escopo passage:
  res <- extrai_dimensoes("Texto curto", schema, list(scope = "passage", unit_id = "pass-1"))
  expect_equal(length(res$responses), 0L)
  expect_equal(length(res$evidences), 0L)
  expect_equal(res$usage$input_tokens, 0L)
})

test_that("extrai_dimensoes realiza extracao e gera contrato response valido", {
  schema <- schema_padrao()
  # Mantém apenas 1 dimensão para teste rápido e limpo
  schema$dimensions <- list(schema$dimensions[[1]]) # research_question
  dim_id <- schema$dimensions[[1]]$id

  texto_artigo <- "Introdução: O presente artigo investiga os determinantes da corrupção local."
  mock_json <- jsonlite::toJSON(list(
    status = "extraido",
    value = "Quais são os determinantes da corrupção local?",
    value_origin = "explicit",
    quote = "O presente artigo investiga os determinantes da corrupção local."
  ), auto_unbox = TRUE)

  passagem <- list(
    passage_id = "pass-art-1",
    document_id = "doc-1",
    page_pdf = 1L,
    text = texto_artigo
  )
  corpus <- list(passages = list(passagem))

  unidade <- list(
    scope = "article",
    unit_id = "art-1",
    passage_id = "pass-art-1"
  )

  res <- extrai_dimensoes(
    conteudo = texto_artigo,
    schema = schema,
    unidade = unidade,
    corpus = corpus,
    mock_response = mock_json,
    run_id = "run-test-1"
  )

  expect_equal(length(res$responses), 1L)
  resp <- res$responses[[dim_id]]

  expect_equal(resp$dimension_id, "research_question")
  expect_equal(resp$value, "Quais são os determinantes da corrupção local?")
  expect_equal(resp$status, "extraido")
  expect_equal(resp$value_origin, "explicit")
  expect_equal(length(resp$evidence_ids), 1L)

  # Validação formal com o contrato de F0
  chk <- contrato_validar(resp, tipo = "response", schema = schema)
  expect_true(chk$valid)

  # Verifica que a evidência foi gerada e validada
  expect_equal(length(res$evidences), 1L)
  ev <- res$evidences[[1]]
  expect_equal(ev$quote, "O presente artigo investiga os determinantes da corrupção local.")
  chk_ev <- contrato_validar(ev, tipo = "evidence", corpus = corpus)
  expect_true(chk_ev$valid)
})

test_that("extrai_dimensoes trata status nao_encontrado com value = NULL", {
  schema <- schema_padrao()
  schema$dimensions <- list(schema$dimensions[[1]])
  dim_id <- schema$dimensions[[1]]$id

  mock_not_found <- jsonlite::toJSON(list(
    status = "nao_encontrado",
    value = NULL,
    value_origin = "explicit",
    quote = NULL
  ), auto_unbox = TRUE)

  unidade <- list(scope = "article", unit_id = "art-2")
  res <- extrai_dimensoes(
    conteudo = "Texto sem pergunta formulada.",
    schema = schema,
    unidade = unidade,
    mock_response = mock_not_found
  )

  resp <- res$responses[[dim_id]]
  expect_equal(resp$status, "nao_encontrado")
  expect_null(resp$value)
  expect_equal(length(resp$evidence_ids), 0L)

  chk <- contrato_validar(resp, tipo = "response", schema = schema)
  expect_true(chk$valid)
})

test_that("extrai_dimensoes respeita explicit_only contra inferencias indevidas", {
  schema <- schema_padrao()
  schema$dimensions <- list(schema$dimensions[[1]])
  schema$dimensions[[1]]$inference_policy <- "explicit_only"
  dim_id <- schema$dimensions[[1]]$id

  # Modelo tenta inferir, mas a política é explicit_only
  mock_inferred <- jsonlite::toJSON(list(
    status = "extraido",
    value = "Pergunta deduzida",
    value_origin = "inferred",
    quote = "Texto implícito"
  ), auto_unbox = TRUE)

  unidade <- list(scope = "article", unit_id = "art-3")
  res <- extrai_dimensoes(
    conteudo = "Texto implícito",
    schema = schema,
    unidade = unidade,
    mock_response = mock_inferred
  )

  resp <- res$responses[[dim_id]]
  expect_equal(resp$status, "nao_encontrado")
  expect_null(resp$value)
})

test_that("extrai_dimensoes registra resposta truncada como erro recuperável", {
  schema <- schema_padrao()
  schema$dimensions <- list(schema$dimensions[[1]])
  transport <- function(prompt, model, ...) {
    list(
      content = '{"status":"extraido","value":"texto parcial',
      usage = list(input_tokens = 100L, output_tokens = 1000L, cost = 0.001),
      finish_reason = "length",
      model = model
    )
  }

  result <- extrai_dimensoes(
    "Texto do artigo.", schema, list(scope = "article", unit_id = "art-truncated"),
    .transport = transport
  )

  expect_equal(result$errors$research_question$code, "output_truncated")
  expect_equal(result$responses$research_question$status, "erro_extracao")
  expect_null(result$responses$research_question$value)
})
