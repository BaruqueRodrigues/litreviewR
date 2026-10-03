test_that("llm_constroi_prompt_dimensao gera instrucoes com base no contrato F0", {
  dim <- list(
    id = "research_question",
    label = "Pergunta de pesquisa",
    definition = "Problema ou pergunta que o trabalho procura responder.",
    type = "text",
    scope = "article",
    operations = list("extract"),
    instruction = "Registre a pergunta como formulada pelos autores.",
    inference_policy = "explicit_only"
  )

  prompt <- llm_constroi_prompt_dimensao(dim, "article")
  expect_true(grepl("Pergunta de pesquisa", prompt, fixed = TRUE))
  expect_true(grepl("explicit_only", prompt, fixed = TRUE))
  expect_true(grepl("quote", prompt, fixed = TRUE))
  expect_true(grepl("no máximo 150 palavras", prompt, fixed = TRUE))
  expect_true(grepl("no máximo 35 palavras", prompt, fixed = TRUE))
})

test_that("llm_chat valida entradas e exige chave na ausencia de mock", {
  expect_error(llm_chat(""), class = "litreview_llm_error")
  expect_error(llm_chat("Pergunta", api_key = "", mock_response = NULL),
               class = "litreview_llm_error")
})

test_that("llm_chat funciona com mock_response sem tocar na rede", {
  mock_json <- '{"status": "extraido", "value": "Pergunta teste", "value_origin": "explicit", "quote": "Trecho teste"}'
  res <- llm_chat("Analise este texto", mock_response = mock_json, model = "gpt-4o-mini")

  expect_equal(res$content, mock_json)
  expect_equal(res$model, "gpt-4o-mini")
  expect_true(res$usage$input_tokens > 0)
})

test_that("llm_calcula_custo computa valores proporcionais a tokens", {
  custo_mini <- llm_calcula_custo(1000000, 1000000, "gpt-4o-mini")
  expect_equal(custo_mini, 0.15 + 0.60) # 0.75 USD

  custo_pequeno <- llm_calcula_custo(10000, 1000, "gpt-4o-mini")
  expect_true(custo_pequeno > 0 && custo_pequeno < 0.01)
})

test_that("llm_parse_resposta normaliza campos e respeita explicit_only", {
  dim_explicit <- list(id = "q", inference_policy = "explicit_only", type = "text")
  dim_inferred <- list(id = "q", inference_policy = "allow_inference", type = "text")

  json_inferred <- '{"status": "extraido", "value": "Valor inferido", "value_origin": "inferred", "quote": "Texto"}'

  # Com explicit_only, inferência não autorizada deve zerar o valor e virar nao_encontrado
  p1 <- llm_parse_resposta(json_inferred, dim_explicit)
  expect_equal(p1$status, "nao_encontrado")
  expect_null(p1$value)

  # Com allow_inference, valor inferido é mantido
  p2 <- llm_parse_resposta(json_inferred, dim_inferred)
  expect_equal(p2$status, "extraido")
  expect_equal(p2$value, "Valor inferido")
  expect_equal(p2$value_origin, "inferred")
})

test_that("llm_parse_resposta trata JSON invalido graciosamente", {
  dim <- list(id = "q", inference_policy = "explicit_only", type = "text")
  parsed <- llm_parse_resposta("isto nao e um json", dim)
  expect_equal(parsed$status, "erro_extracao")
  expect_equal(parsed$error_code, "invalid_json")
  expect_null(parsed$value)
})

test_that("llm_parse_resposta identifica status desconhecido", {
  dim <- list(id = "q", inference_policy = "explicit_only", type = "text")
  parsed <- llm_parse_resposta(
    '{"status":"found","value":"x","value_origin":"explicit"}', dim
  )
  expect_equal(parsed$status, "erro_extracao")
  expect_equal(parsed$error_code, "invalid_status")
})

test_that("llm_chat com .transport customizado funciona como injecao de dependencia", {
  meu_transport <- function(prompt, model, ...) {
    list(content = '{"status": "extraido", "value": "ok"}',
         usage = list(input_tokens = 50L, output_tokens = 10L, cost = 0.00005),
         model = model)
  }

  res <- llm_chat("Meu prompt", .transport = meu_transport)
  expect_equal(res$content, '{"status": "extraido", "value": "ok"}')
  expect_equal(res$usage$input_tokens, 50L)
})

test_that("llm_chat configura endpoint e variável de ambiente do DeepSeek", {
  seen <- NULL
  transport <- function(prompt, model, api_key, endpoint, max_tokens, timeout, ...) {
    seen <<- list(model = model, api_key = api_key, endpoint = endpoint,
                  max_tokens = max_tokens, timeout = timeout)
    list(content = "{}", usage = list(input_tokens = 1L, output_tokens = 1L, cost = 0), model = model)
  }
  res <- llm_chat("teste", provider = "deepseek", api_key = "dummy", .transport = transport)
  expect_equal(seen$model, "deepseek-flash")
  expect_equal(seen$endpoint, "https://api.deepseek.com/chat/completions")
  expect_equal(seen$api_key, "dummy")
  expect_equal(res$model, "deepseek-flash")
  expect_error(llm_chat("teste", provider = "deepseek", api_key = ""),
               "DEEPSEEK_API_KEY", class = "litreview_llm_error")
})

test_that("extrai_dimensoes repassa provedor DeepSeek ao adaptador", {
  schema <- schema_padrao()
  schema$dimensions <- list(schema$dimensions[[1]])
  seen <- NULL
  transport <- function(prompt, model, api_key, endpoint, max_tokens, timeout, ...) {
    seen <<- list(model = model, api_key = api_key, endpoint = endpoint,
                  max_tokens = max_tokens, timeout = timeout)
    list(content = '{"status":"nao_encontrado","value":null,"value_origin":"explicit","quote":null}',
         usage = list(input_tokens = 12L, output_tokens = 5L, cost = 0.00001), model = model)
  }
  result <- extrai_dimensoes(
    "Resumo aberto do artigo.", schema, list(scope = "article", unit_id = "art-1"),
    provider = "deepseek", api_key = "dummy", max_tokens = 2500, timeout = 90,
    .transport = transport
  )
  expect_equal(seen$model, "deepseek-flash")
  expect_equal(seen$endpoint, "https://api.deepseek.com/chat/completions")
  expect_equal(seen$api_key, "dummy")
  expect_equal(seen$max_tokens, 2500)
  expect_equal(seen$timeout, 90)
  expect_equal(result$responses[[1]]$backend, "deepseek")
})
