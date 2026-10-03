test_that("cria_dtm valida argumentos de entrada", {
  expect_error(cria_dtm(textos = character()), "vazio")
  expect_error(cria_dtm(pasta_pdfs = "pasta_que_nao_existe_12345"), "Nenhum arquivo")
  expect_error(cria_dtm(textos = c("", "  ", NA)), "Nenhum texto utilizável")
})

test_that("cria_dtm preserva identificadores de documentos nos dimnames", {
  textos <- c(
    art_1 = "O presidencialismo de coalizão no Brasil estrutura as relações entre poderes.",
    art_2 = "A reforma política e o financiamento eleitoral afetam partidos e eleições.",
    art_3 = "A economia política e a inflação influenciam o comportamento eleitoral."
  )

  dtm <- cria_dtm(textos = textos, idioma = "portuguese", min_freq = 1)
  expect_s3_class(dtm, "DocumentTermMatrix")
  expect_equal(dtm$dimnames$Docs, c("art_1", "art_2", "art_3"))
  expect_true(ncol(dtm) > 0)
  expect_equal(nrow(dtm), 3L)
})

test_that("cria_dtm registra e retorna mapa de exclusoes para documentos vazios", {
  textos <- c(
    doc_valido1 = "Democracia representativa e instituições partidárias no parlamento.",
    doc_vazio = "   ",
    doc_valido2 = "Federalismo e relações intergovernamentais em estados federativos.",
    doc_apenas_stopwords = "e o a de para com em"
  )

  dtm <- cria_dtm(textos = textos, idioma = "portuguese", min_freq = 1)
  exclusoes <- dtm_obter_exclusoes(dtm)

  expect_s3_class(exclusoes, "tbl_df")
  expect_true(nrow(exclusoes) >= 2L)
  expect_true("doc_vazio" %in% exclusoes$doc_id)
  expect_true("doc_apenas_stopwords" %in% exclusoes$doc_id)

  # DTM final deve conter apenas os 2 documentos válidos
  expect_equal(nrow(dtm), 2L)
  expect_equal(dtm$dimnames$Docs, c("doc_valido1", "doc_valido2"))
})

test_that("cria_dtm gera doc_ids unicos quando textos nao sao nomeados", {
  textos <- c(
    "Estudo sobre políticas públicas e avaliação de impacto governamental.",
    "Pesquisa sobre judiciário e ativismo judicial no supremo tribunal."
  )

  dtm <- cria_dtm(textos = textos, idioma = "portuguese", min_freq = 1)
  expect_equal(dtm$dimnames$Docs, c("doc_1", "doc_2"))
})

test_that("cria_dtm rejeita colisões de ID em vez de renomeá-las silenciosamente", {
  expect_error(cria_dtm(textos = c("Texto do artigo um.", "Texto do artigo dois."),
                        doc_ids = c("artigo", "artigo"), min_freq = 1),
               "duplicados não são permitidos")
  expect_error(cria_dtm(textos = c("um texto", "outro texto"),
                        doc_ids = "somente_um", min_freq = 1),
               "um ID textual não vazio para cada texto")
})

test_that("corpus vazio preserva o mapa de exclusões na condição de erro", {
  error <- tryCatch(
    cria_dtm(textos = c(doc_a = " ", doc_b = NA_character_), min_freq = 1),
    litreview_empty_corpus = identity
  )
  expect_s3_class(error, "litreview_empty_corpus")
  expect_equal(error$exclusoes$doc_id, c("doc_a", "doc_b"))
  expect_equal(error$exclusoes$motivo, c("texto_vazio_ou_ilegivel", "texto_vazio_ou_ilegivel"))
})
