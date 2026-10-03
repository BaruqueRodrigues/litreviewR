test_that("editor de dimensões atualiza e exclui dimensões", {
  skip_if_not_installed("shiny")
  dims <- list(
    list(id = "one", label = "Uma", definition = "", active = TRUE, type = "text", scope = "article", operations = list("extract"), instruction = "", search_terms = list(), include_examples = list(), exclude_examples = list(), evidence_required = TRUE, categories = list(), scoring_rubric = list(), applicability = list(all = list(), any = list()), inference_policy = "explicit_only", review_rule = list(type = "if_ambiguous", threshold = NULL, experimental = FALSE)),
    list(id = "two", label = "Duas", definition = "", active = TRUE, type = "text", scope = "article", operations = list("extract"), instruction = "", search_terms = list(), include_examples = list(), exclude_examples = list(), evidence_required = TRUE, categories = list(), scoring_rubric = list(), applicability = list(all = list(), any = list()), inference_policy = "explicit_only", review_rule = list(type = "if_ambiguous", threshold = NULL, experimental = FALSE))
  )
  store <- shiny::reactiveVal(dims)
  shiny::testServer(mod_dimensoes_server, args = list(dimensoes = shiny::reactive(store()), on_change = store), {
    session$setInputs(dimension_id = "one")
    session$setInputs(label = "Questão nova")
    session$flushReact()
    expect_equal(store()[[1]]$label, "Questão nova")
    expect_equal(session$returned$selecionada(), "one")
    session$setInputs(delete = 1)
    session$flushReact()
    expect_length(store(), 1)
  })
})

test_that("lógica de ativação e ordenação preserva dimensões", {
  dims <- list(list(id = "one", active = TRUE), list(id = "two", active = TRUE))
  expect_false(set_dimension_active(dims, "one")[[1]]$active)
  expect_equal(vapply(reorder_dimensions(dims, "one", 1L), `[[`, character(1), "id"), c("two", "one"))
  expect_identical(set_dimension_active(dims, "missing"), dims)
})
