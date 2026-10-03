test_that("extração de resumo respeita delimitadores e não usa cabeçalho como fallback", {
  text <- paste(
    "Título do artigo", "Autoria Jane Doe", "Resumo: Este artigo analisa instituições e eleições.",
    "A análise compara quatro casos ao longo do tempo.", "Palavras-chave: eleições; instituições.",
    "Introdução", "Texto introdutório que não deve integrar o resumo.", sep = "\n"
  )
  result <- .extrair_resumo(text)
  expect_match(result$resumo, "compara quatro casos", fixed = TRUE)
  expect_false(grepl("Palavras-chave", result$resumo, fixed = TRUE))
  expect_false(grepl("Texto introdutório", result$resumo, fixed = TRUE))
  expect_false(result$incerto)
  inline_heading <- .extrair_resumo(paste(
    "Abstract", "This article analyzes spending. Introduction begins in the next column.",
    sep = "\n"
  ))
  expect_false(grepl("Introduction begins", inline_heading$resumo, fixed = TRUE))
  expect_true(inline_heading$incerto)
  missing <- .extrair_resumo("Título\nAutoria\nO texto começa sem marcador de resumo.")
  expect_equal(missing$resumo, "")
  expect_true(missing$incerto)
})

test_that("marcador sem cabeçalho final fica explicitamente incerto", {
  result <- .extrair_resumo("Abstract\nWe study political institutions and voting patterns.")
  expect_true(nzchar(result$resumo))
  expect_true(result$incerto)
})

test_that("extração aceita marcador de resumo no final de linha do PDF", {
  text <- paste(
    "Título do artigo", "Autoria Jane Doe", "    Abstract", "This study analyzes campaign spending.",
    "We compare election outcomes across states.", "Introduction", "The study begins here.", sep = "\n"
  )
  result <- .extrair_resumo(text)
  expect_match(result$resumo, "campaign spending", fixed = TRUE)
  expect_false(grepl("Introduction", result$resumo, fixed = TRUE))
  expect_false(result$incerto)
})
