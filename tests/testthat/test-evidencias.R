test_that("valida_evidencia_trecho encontra posicoes exatas de citacao valida", {
  texto <- "Este artigo analisa a transição política brasileira após 1985."
  trecho <- "transição política brasileira"

  res <- valida_evidencia_trecho(trecho, texto)
  expect_true(res$valid)
  expect_equal(res$start, 23L)
  expect_equal(res$end, 51L)
  expect_equal(substr(texto, res$start, res$end), trecho)
})

test_that("valida_evidencia_trecho rejeita citacao inventada ou com diferencas", {
  texto <- "Este artigo analisa a transição política brasileira após 1985."
  trecho_inventado <- "transição democrática nacional"

  res <- valida_evidencia_trecho(trecho_inventado, texto)
  expect_false(res$valid)
  expect_true(grepl("não ocorre literalmente", res$error, fixed = TRUE))
})

test_that("criar_evidencia produz objeto conforme contrato evidence de F0", {
  ev <- criar_evidencia("ev-001", "pass-001", "trecho citado", start = 1L, end = 13L)
  expect_equal(ev$evidence_id, "ev-001")
  expect_equal(ev$passage_id, "pass-001")
  expect_equal(ev$quote, "trecho citado")
  expect_equal(ev$start, 1L)
  expect_equal(ev$end, 13L)

  chk <- contrato_validar(ev, tipo = "evidence")
  expect_true(chk$valid)
})

test_that("criar_evidencia calcula start e end automaticamente a partir do corpus", {
  passagem <- list(
    passage_id = "pass-001",
    document_id = "doc-001",
    page_pdf = 2L,
    text = "A metodologia empregada combina análise documental e regressão linear."
  )
  corpus <- list(passages = list(passagem))

  ev <- criar_evidencia(
    evidence_id = "ev-002",
    passage_id = "pass-001",
    quote = "análise documental e regressão linear",
    corpus = corpus
  )

  expect_true(!is.null(ev$start) && !is.null(ev$end))
  expect_equal(substr(passagem$text, ev$start, ev$end), ev$quote)

  chk <- contrato_validar(ev, tipo = "evidence", corpus = corpus)
  expect_true(chk$valid)
})

test_that("criar_evidencia rejeita passagem inexistente no corpus", {
  passagem <- list(passage_id = "pass-001", document_id = "doc-001", page_pdf = 1L, text = "Texto.")
  corpus <- list(passages = list(passagem))

  expect_error(
    criar_evidencia("ev-003", "pass-999", "Texto.", corpus = corpus, error = TRUE),
    class = "error"
  )
})

test_that("valida_evidencias_resposta valida conjunto de evidencias vinculadas", {
  passagem <- list(passage_id = "p-1", document_id = "d-1", page_pdf = 1L, text = "O método é quantitativo.")
  corpus <- list(passages = list(passagem))
  ev <- criar_evidencia("ev-1", "p-1", "O método é quantitativo.", corpus = corpus)

  resp <- list(
    run_id = "run-1", schema_id = "s-1", revision = 1L, dimension_id = "method",
    unit_type = "article", unit_id = "art-1", value = "quanti", status = "extraido",
    value_origin = "explicit", evidence_ids = list("ev-1"),
    human_review = list(status = "pending")
  )

  chk <- valida_evidencias_resposta(resp, list(ev), corpus = corpus)
  expect_true(chk$valid)

  # Resposta apontando para evidência não fornecida
  resp_fantasma <- resp
  resp_fantasma$evidence_ids <- list("ev-inexistente")
  chk_fantasma <- valida_evidencias_resposta(resp_fantasma, list(ev), corpus = corpus)
  expect_false(chk_fantasma$valid)
})
