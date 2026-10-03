test_that("classifica_dimensoes valida entradas basicas", {
  schema <- schema_padrao()
  expect_error(classifica_dimensoes("", schema, list(scope = "article", unit_id = "art-1")),
               class = "litreview_jev_error")
  expect_error(classifica_dimensoes("Texto", list(), list(scope = "article", unit_id = "art-1")),
               class = "litreview_jev_error")
  expect_error(classifica_dimensoes("Texto", schema, list(scope = "invalid_scope", unit_id = "art-1")),
               class = "litreview_jev_error")
  expect_error(classifica_dimensoes("Texto", schema, list(scope = "article", unit_id = "")),
               class = "litreview_jev_error")
  expect_error(classifica_dimensoes("Texto", schema, list(scope = "article", unit_id = "art-1"),
                                    confidence_threshold = 1.5),
               class = "litreview_jev_error")
})

test_that("classifica_dimensoes devolve data frame vazio quando nao ha dimensoes no escopo", {
  schema <- schema_padrao()
  # schema padrão possui apenas dimensões com operation extract por padrão
  res <- classifica_dimensoes("Texto", schema, list(scope = "article", unit_id = "art-1"))
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0L)
  expect_true("schema_id" %in% names(res))
  expect_true("probabilities" %in% names(res))
})

test_that("JEV exige autorização explícita para valores de origem inferida", {
  schema <- schema_padrao()
  schema$dimensions[[1]] <- list(
    id = "topic", label = "Tema", definition = "Tema do texto.", active = TRUE,
    type = "single_category", scope = "article", operations = list("classify"),
    instruction = "Classifique o tema.", search_terms = list(), include_examples = list(),
    exclude_examples = list(), evidence_required = TRUE,
    categories = list(
      list(id = "a", label = "A", description = "Categoria A."),
      list(id = "b", label = "B", description = "Categoria B.")
    ), scoring_rubric = list(), applicability = list(all = list(), any = list()),
    inference_policy = "explicit_only",
    review_rule = list(type = "if_ambiguous", threshold = NULL, experimental = FALSE)
  )
  called <- FALSE
  mock <- function(...) { called <<- TRUE; stop("must not run") }
  expect_error(classifica_dimensoes("Texto", schema, list(scope = "article", unit_id = "a1"),
                                    api_key = "dummy", .transport = mock),
               class = "litreview_jev_error")
  expect_false(called)
})

test_that("classifica_dimensoes classifica dimensoes single_category com mock transport", {
  schema <- schema_padrao()
  schema$dimensions[[1]] <- list(
    id = "theory", label = "Teoria", definition = "Corrente teórica.", active = TRUE,
    type = "single_category", scope = "article", operations = list("classify"),
    instruction = "Escolha a corrente.", search_terms = list(), include_examples = list(),
    exclude_examples = list(), evidence_required = TRUE,
    categories = list(
      list(id = "institutionalism", label = "Institucionalismo", description = "Foco em regras e normas."),
      list(id = "behavioralism", label = "Comportamentalismo", description = "Foco no comportamento individual.")
    ),
    scoring_rubric = list(), applicability = list(all = list(), any = list()),
    inference_policy = "allow_inference",
    review_rule = list(type = "if_ambiguous", threshold = NULL, experimental = FALSE)
  )

  mock_transport <- function(payload, api_key, endpoint, timeout) {
    list(
      model = "jev-1.13.0",
      answers = list(
        q0001 = list(
          type = "choice",
          choice = "institutionalism",
          confidence = 0.89,
          probabilities = list(institutionalism = 0.89, behavioralism = 0.10, "__indeterminate__" = 0.01)
        )
      ),
      usage = list(input_tokens = 150L, output_tokens = 20L)
    )
  }

  unidade <- list(scope = "article", unit_id = "art-100", article_id = "art-100", evidence_ids = list("ev-1"))
  res <- classifica_dimensoes("Texto sobre instituições", schema, unidade, api_key = "dummy",
                               .transport = mock_transport)

  expect_equal(nrow(res), 1L)
  expect_equal(res$dimension_id, "theory")
  expect_equal(res$value, "institutionalism")
  expect_equal(res$status, "extraido")
  expect_equal(res$confidence, 0.89)
  expect_equal(res$unit_id, "art-100")
  expect_equal(res$content_examined, "Texto sobre instituições")
})

test_that("classifica_dimensoes trata multiple_categories com perguntas binarias independentes", {
  schema <- schema_padrao()
  schema$dimensions[[1]] <- list(
    id = "themes", label = "Temas", definition = "Temas abordados.", active = TRUE,
    type = "multiple_categories", scope = "article", operations = list("classify"),
    instruction = "Identifique os temas presentes.", search_terms = list(), include_examples = list(),
    exclude_examples = list(), evidence_required = TRUE,
    categories = list(
      list(id = "democracy", label = "Democracia", description = "Estudos sobre regime democrático."),
      list(id = "elections", label = "Eleições", description = "Processos eleitorais e voto.")
    ),
    scoring_rubric = list(), applicability = list(all = list(), any = list()),
    inference_policy = "allow_inference",
    review_rule = list(type = "if_ambiguous", threshold = NULL, experimental = FALSE)
  )

  mock_transport <- function(payload, api_key, endpoint, timeout) {
    list(
      model = "jev-1.13.0",
      answers = list(
        q0001 = list(type = "choice", choice = "yes", confidence = 0.95,
                     probabilities = list(yes = 0.95, no = 0.05)),
        q0002 = list(type = "choice", choice = "no", confidence = 0.80,
                     probabilities = list(yes = 0.20, no = 0.80))
      ),
      usage = list(input_tokens = 180L, output_tokens = 30L)
    )
  }

  unidade <- list(scope = "article", unit_id = "art-200")
  res <- classifica_dimensoes("Texto sobre qualidade democrática.", schema, unidade,
                               api_key = "dummy", .transport = mock_transport)

  expect_equal(nrow(res), 2L)
  expect_equal(res$category_id, c("democracy", "elections"))
  expect_equal(res$membership, c(TRUE, FALSE))
  expect_equal(res$value, c("democracy", NA_character_))
  expect_equal(res$status, c("extraido", "extraido"))
})

test_that("classifica_dimensoes marca status ambiguo para escolha indeterminada ou confianca baixa", {
  schema <- schema_padrao()
  schema$dimensions[[1]] <- list(
    id = "method", label = "Metodo", definition = "Abordagem metodológica.", active = TRUE,
    type = "single_category", scope = "article", operations = list("classify"),
    instruction = "Classifique o método.", search_terms = list(), include_examples = list(),
    exclude_examples = list(), evidence_required = TRUE,
    categories = list(
      list(id = "quanti", label = "Quantitativo", description = "Dados numéricos e estatística."),
      list(id = "quali", label = "Qualitativo", description = "Estudos de caso e entrevistas.")
    ),
    scoring_rubric = list(), applicability = list(all = list(), any = list()),
    inference_policy = "allow_inference",
    review_rule = list(type = "if_ambiguous", threshold = NULL, experimental = FALSE)
  )

  # Caso 1: modelo escolheu explicitamente __indeterminate__
  mock_indeterminate <- function(...) {
    list(
      model = "jev",
      answers = list(q0001 = list(
        type = "choice", choice = "__indeterminate__", confidence = 0.60,
        probabilities = list(quanti = 0.2, quali = 0.2, "__indeterminate__" = 0.6)
      )),
      usage = list()
    )
  }
  res1 <- classifica_dimensoes("Texto inconclusivo", schema, list(scope = "article", unit_id = "art-1"),
                                api_key = "test", .transport = mock_indeterminate)
  expect_equal(res1$status, "ambiguo")
  expect_true(is.na(res1$value))

  # Caso 2: confiança abaixo do limiar
  mock_low_conf <- function(...) {
    list(
      model = "jev",
      answers = list(q0001 = list(
        type = "choice", choice = "quanti", confidence = 0.45,
        probabilities = list(quanti = 0.45, quali = 0.40, "__indeterminate__" = 0.15)
      )),
      usage = list()
    )
  }
  res2 <- classifica_dimensoes("Texto misto", schema, list(scope = "article", unit_id = "art-1"),
                                confidence_threshold = 0.70, api_key = "test",
                                .transport = mock_low_conf)
  expect_equal(res2$status, "ambiguo")
  expect_true(is.na(res2$value))
})
