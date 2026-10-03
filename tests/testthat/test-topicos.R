test_that("modela_topicos com LDA controla seed e produz resultados reproduziveis", {
  textos <- c(
    doc_1 = "Congresso nacional aprova reforma tributária após debates entre partidos.",
    doc_2 = "Câmara dos deputados vota projeto de lei orçamentária para próximo ano.",
    doc_3 = "Banco central eleva taxa de juros para conter inflação e estabilizar câmbio.",
    doc_4 = "Mercado financeiro reage a indicadores econômicos e balança comercial."
  )
  dtm <- cria_dtm(textos = textos, idioma = "portuguese", min_freq = 1)

  mod1 <- modela_topicos(dtm, k = 2, method = "lda", seed = 42)
  mod2 <- modela_topicos(dtm, k = 2, method = "lda", seed = 42)

  expect_s4_class(mod1, "LDA_VEM")
  expect_equal(mod1@k, 2L)
  # Com a mesma semente, os parâmetros devem ser idênticos
  expect_equal(mod1@beta, mod2@beta)
})

test_that("modela_topicos com STM funciona sem stm::convert e alinha metadados por doc_id", {
  textos <- c(
    doc_1 = "Estudo sobre eleições e financiamento partidário no Brasil.",
    doc_2 = "Análise sobre coalizões governamentais e poder executivo federal.",
    doc_3 = "Economia política da tributação e distribuição de renda.",
    doc_4 = "Política monetária e controle inflacionário no plano real."
  )
  dtm <- cria_dtm(textos = textos, idioma = "portuguese", min_freq = 1)

  metadados <- data.frame(
    doc_id = c("doc_1", "doc_2", "doc_3", "doc_4"),
    ano = c(2010, 2014, 2018, 2022),
    stringsAsFactors = FALSE
  )

  mod_stm <- suppressWarnings(
    modela_topicos(dtm, k = 2, method = "stm", metadados = metadados,
                   formula_prevalence = ~ ano, seed = 1234, verbose = FALSE)
  )

  expect_s3_class(mod_stm, "STM")
  expect_equal(mod_stm$settings$dim$K, 2L)
})

test_that("modela_topicos com STM alinha metadados mesmo se houver documento excluido", {
  textos <- c(
    doc_1 = "Estudo sobre eleições e partidos políticos.",
    doc_excluido = "e o de para", # somente stopwords, será excluído pela DTM
    doc_2 = "Análise sobre economia e reforma fiscal."
  )
  dtm <- cria_dtm(textos = textos, idioma = "portuguese", min_freq = 1)

  # Metadados contêm os 3 documentos originais
  metadados <- data.frame(
    doc_id = c("doc_1", "doc_excluido", "doc_2"),
    ano = c(2010, 2015, 2020),
    stringsAsFactors = FALSE
  )

  # modela_topicos deve filtrar automaticamente metadados para corresponder aos 2 documentos da DTM
  mod_stm <- suppressWarnings(
    modela_topicos(dtm, k = 2, method = "stm", metadados = metadados,
                   formula_prevalence = ~ ano, seed = 1234, verbose = FALSE)
  )

  expect_s3_class(mod_stm, "STM")
  expect_equal(mod_stm$settings$dim$N, 2L)
})

test_that("NMF fica suspenso ate a validacao de um backend mantido", {
  textos <- c("Texto um sobre legislativo.", "Texto dois sobre judiciario.")
  dtm <- cria_dtm(textos = textos, idioma = "portuguese", min_freq = 1)

  expect_error(modela_topicos(dtm, k = 2, method = "nmf"), "temporariamente suspenso")
})

test_that("extrai_topicos unifica pre-processamento e suporta modos agrupado e detalhado", {
  textos <- c(
    doc_1 = "Pesquisa sobre partidos e eleições legislativas.",
    doc_2 = "Votação no plenário e disciplina partidária.",
    doc_3 = "Comércio exterior e tarifas alfandegárias.",
    doc_4 = "Exportações agrícolas e câmbio flutuante."
  )

  # Modo agrupado
  res_agrupado <- extrai_topicos(textos = textos, k = 2, max_termos = 3, modo = "agrupado", seed = 10)
  expect_s3_class(res_agrupado, "tbl_df")
  expect_true(all(c("topico", "texto") %in% names(res_agrupado)))
  expect_equal(nrow(res_agrupado), 2L)

  # Modo detalhado
  res_detalhado <- extrai_topicos(textos = textos, k = 2, max_termos = 3, modo = "detalhado", seed = 10)
  expect_s3_class(res_detalhado, "tbl_df")
  expect_true(all(c("topico", "term", "beta") %in% names(res_detalhado)))
})

test_that("roda_analise_topicos executa pipeline integrado e devolve exclusoes", {
  textos <- c(
    doc_1 = "Instituições e regras eleitorais.",
    doc_vazio = "   ",
    doc_2 = "Política econômica e taxa de juros."
  )

  res_pipeline <- roda_analise_topicos(textos = textos, k = 2, method = "lda", min_freq = 1, seed = 123)
  expect_type(res_pipeline, "list")
  expect_true("topicos" %in% names(res_pipeline))
  expect_true("modelo" %in% names(res_pipeline))
  expect_true("dtm" %in% names(res_pipeline))
  expect_true("exclusoes" %in% names(res_pipeline))
  expect_true("associacao_documento_topico" %in% names(res_pipeline))
  expect_identical(rownames(res_pipeline$associacao_documento_topico), c("doc_1", "doc_2"))

  expect_true("doc_vazio" %in% res_pipeline$exclusoes$doc_id)
})

test_that("agrupa_topicos manual integra com arquivo CSV", {
  topicos <- tibble::tibble(
    topico = c(1, 2),
    texto = c("partido eleicao voto", "inflacao juros cambio")
  )
  csv_path <- if (file.exists("tests/testthat/fixtures/topicos/anotacao_topicos.csv")) {
    "tests/testthat/fixtures/topicos/anotacao_topicos.csv"
  } else if (file.exists("fixtures/topicos/anotacao_topicos.csv")) {
    "fixtures/topicos/anotacao_topicos.csv"
  } else {
    testthat::test_path("fixtures/topicos/anotacao_topicos.csv")
  }

  res_manual <- agrupa_topicos(topicos, metodo = "manual", arquivo_csv = csv_path)
  expect_true("area" %in% names(res_manual))
  expect_equal(res_manual$area, c("Politica Institucional", "Economia Politica"))
})

test_that("agrupa_topicos manual rejeita duplicatas, grupos ausentes e IDs sem correspondência", {
  topicos <- tibble::tibble(topico = c(1, 2), texto = c("termos um", "termos dois"))
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  readr::write_csv(tibble::tibble(topico = c(1, 1), area = c("A", "B")), path)
  expect_error(agrupa_topicos(topicos, metodo = "manual", arquivo_csv = path), "duplicados")
  readr::write_csv(tibble::tibble(topico = c(1, 2), area = c("A", NA_character_)), path)
  expect_error(agrupa_topicos(topicos, metodo = "manual", arquivo_csv = path), "sem grupo")
  readr::write_csv(tibble::tibble(topico = c(1, 3), area = c("A", "C")), path)
  expect_error(agrupa_topicos(topicos, metodo = "manual", arquivo_csv = path), "corresponder exatamente")
})

test_that("agrupa_topicos cluster via k-means agrupa com controle de seed", {
  topicos <- tibble::tibble(
    topico = c(1, 2, 3, 4),
    texto = c(
      "eleicoes partidos congresso voto parlamento",
      "deputados senado legislativo votacao regras",
      "inflacao juros cambio banco central moeda",
      "mercado balanca comercial divida exportacao fiscal"
    )
  )

  res_cluster <- agrupa_topicos(topicos, metodo = "cluster", n_clusters = 2, seed = 42)
  expect_true("grupo" %in% names(res_cluster))
  expect_equal(nrow(res_cluster), 4L)
  expect_true(all(grepl("^Grupo_", res_cluster$grupo)))
})
