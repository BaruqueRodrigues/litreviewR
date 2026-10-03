#' Extração de dimensões usando LLM e instrumentos de revisão
#'
#' Extrai campos estruturados a partir do conteúdo examinado (artigo, estudo ou passagem)
#' com base nas dimensões ativas do esquema cujo escopo corresponda à unidade fornecida.
#' Respostas produzidas cumprem estritamente o contrato canônico `response` de F0.
#'
#' @param conteudo Texto a ser analisado pelo modelo.
#' @param schema Instrumento configurado (lista retornada por `schema_padrao()` ou JSON).
#' @param unidade Lista com `scope` ("article", "study", "passage"), `unit_id`, e IDs opcionais
#'   (`article_id`, `study_id`, `passage_id`).
#' @param corpus Lista opcional de passagens para validação e ancoragem de evidências.
#' @param run_id Identificador da execução (default: "run-001").
#' @param backend Nome do backend gravado no resultado. Por padrão, usa o provedor.
#' @param model Modelo a utilizar. O padrão é `gpt-4o-mini` para OpenAI e
#'   `deepseek-flash` para DeepSeek.
#' @param max_tokens Limite de tokens por resposta (default: 2000).
#' @param timeout Tempo máximo em segundos por chamada (default: 120).
#' @param api_key Chave opcional, lida do ambiente conforme o provedor.
#' @param mock_response Resposta(s) simulada(s) para testes sem rede. Pode ser uma string JSON
#'   ou uma lista nomeada por ID de dimensão.
#' @param .transport Função de transporte alternativo para testes.
#' @param provider Provedor compatível com Chat Completions: `openai` ou `deepseek`.
#' @param endpoint URL opcional do endpoint de chat completions.
#'
#' @return Lista contendo:
#'   - `responses`: lista de respostas conformes ao contrato `response`.
#'   - `evidences`: lista de objetos conformes ao contrato `evidence`.
#'   - `usage`: lista com tokens e custo total observado.
#' @export
extrai_dimensoes <- function(conteudo,
                             schema,
                             unidade,
                             corpus = NULL,
                             run_id = "run-001",
                             backend = NULL,
                             model = NULL,
                             max_tokens = 2000,
                             timeout = 120,
                             api_key = NULL,
                             mock_response = NULL,
                             .transport = NULL,
                             provider = "openai",
                             endpoint = NULL) {

  if (!is.character(provider) || length(provider) != 1L ||
      is.na(provider) || !provider %in% c("openai", "deepseek")) {
    .llm_abort("invalid_provider", "`provider` deve ser `openai` ou `deepseek`.")
  }
  if (is.null(model)) model <- if (provider == "deepseek") "deepseek-flash" else "gpt-4o-mini"
  if (is.null(backend)) backend <- provider

  if (!is.character(conteudo) || length(conteudo) != 1L || is.na(conteudo) || !nzchar(conteudo)) {
    .llm_abort("invalid_content", "`conteudo` deve ser uma string não vazia.")
  }

  if (exists("schema_validar", mode = "function")) {
    chk_schema <- schema_validar(schema, error = FALSE)
    if (!isTRUE(chk_schema$valid)) {
      .llm_abort("invalid_schema", paste("Esquema inválido:", paste(chk_schema$errors, collapse = "; ")))
    }
  }

  if (!is.list(unidade) || !unidade$scope %in% c("article", "study", "passage") ||
      is.null(unidade$unit_id) || !is.character(unidade$unit_id) || length(unidade$unit_id) != 1L || !nzchar(unidade$unit_id)) {
    .llm_abort("invalid_unit", "`unidade` exige `scope` ('article', 'study', 'passage') e `unit_id` válido.")
  }

  dimensions <- schema$dimensions
  if (!is.list(dimensions)) {
    .llm_abort("invalid_schema", "`dimensions` deve ser uma lista.")
  }

  # Filtra dimensões ativas, do escopo da unidade, que realizam extração
  dimensoes_alvo <- Filter(function(d) {
    isTRUE(d$active %||% TRUE) &&
      identical(d$scope, unidade$scope) &&
      "extract" %in% (d$operations %||% character())
  }, dimensions)

  if (length(dimensoes_alvo) == 0L) {
    return(list(
      responses = list(),
      evidences = list(),
      usage = list(input_tokens = 0L, output_tokens = 0L, cost = 0)
    ))
  }

  respostas <- list()
  evidencias <- list()
  erros <- list()
  total_in <- 0L
  total_out <- 0L
  total_cost <- 0

  for (i in seq_along(dimensoes_alvo)) {
    dim <- dimensoes_alvo[[i]]
    prompt_dim <- llm_constroi_prompt_dimensao(dim, unidade$scope)
    prompt_completo <- paste0(prompt_dim, "\n--- TEXTO A ANALISAR ---\n", conteudo)

    mock_dim <- if (is.list(mock_response) && !is.null(mock_response[[dim$id]])) {
      mock_response[[dim$id]]
    } else if (is.character(mock_response)) {
      mock_response
    } else {
      NULL
    }

    # Chamada ao modelo ou mock
    llm_out <- tryCatch(
      llm_chat(
        prompt = prompt_completo,
        model = model,
        max_tokens = max_tokens,
        timeout = timeout,
        api_key = api_key,
        mock_response = mock_dim,
        .transport = .transport,
        provider = provider,
        endpoint = endpoint
      ),
      error = function(e) {
        message <- conditionMessage(e)
        key <- api_key
        if (is.null(key) || !is.character(key) || length(key) != 1L || !nzchar(key)) {
          key_name <- if (provider == "deepseek") "DEEPSEEK_API_KEY" else "OPENAI_API_KEY"
          key <- Sys.getenv(key_name, unset = "")
        }
        if (is.character(key) && length(key) == 1L && nzchar(key)) {
          message <- gsub(key, "[REDACTED]", message, fixed = TRUE)
        }
        erros[[dim$id]] <<- list(code = e$code %||% "llm_error", message = message)
        list(
          content = jsonlite::toJSON(list(status = "erro_extracao", value = NULL, value_origin = "explicit")),
          usage = list(input_tokens = 0L, output_tokens = 0L, cost = 0),
          model = model,
          error = conditionMessage(e)
        )
      }
    )

    total_in <- total_in + (llm_out$usage$input_tokens %||% 0L)
    total_out <- total_out + (llm_out$usage$output_tokens %||% 0L)
    total_cost <- total_cost + (llm_out$usage$cost %||% 0)

    parsed <- if (identical(llm_out$finish_reason, "length")) {
      list(
        status = "erro_extracao", value = NULL, value_origin = "explicit",
        quote = NULL, error_code = "output_truncated",
        error_message = "A resposta atingiu o limite de tokens antes de terminar."
      )
    } else {
      llm_parse_resposta(llm_out$content, dim)
    }
    if (identical(parsed$status, "erro_extracao") && is.null(erros[[dim$id]])) {
      erros[[dim$id]] <- list(
        code = parsed$error_code %||% "invalid_model_output",
        message = parsed$error_message %||%
          "A resposta do modelo não pôde ser interpretada conforme o esquema."
      )
    }

    # Tratamento de evidência citada
    evidence_ids <- list()
    if (!is.null(parsed$quote) && is.character(parsed$quote) && nzchar(parsed$quote)) {
      ev_id <- paste0("ev-", dim$id, "-", unidade$unit_id)
      passage_id_alvo <- unidade$passage_id %||% paste0("pass-", unidade$unit_id)

      ev_obj <- tryCatch(
        criar_evidencia(
          evidence_id = ev_id,
          passage_id = passage_id_alvo,
          quote = parsed$quote,
          corpus = corpus,
          error = FALSE
        ),
        error = function(e) NULL
      )

      if (!is.null(ev_obj)) {
        # Se houver corpus, verifica se foi considerada válida
        if (is.null(corpus) || is.null(attr(ev_obj, "errors"))) {
          evidencias[[ev_id]] <- ev_obj
          evidence_ids <- list(ev_id)
        }
      }
    }

    # Constrói o objeto de resposta conforme contrato de F0
    resposta_obj <- list(
      run_id = run_id,
      schema_id = schema$schema_id,
      revision = as.integer(schema$revision),
      dimension_id = dim$id,
      unit_type = unidade$scope,
      unit_id = unidade$unit_id,
      value = parsed$value,
      status = parsed$status,
      value_origin = parsed$value_origin,
      backend = backend,
      model = model,
      instruction_version = as.character(schema$revision),
      evidence_ids = evidence_ids,
      coverage = list(status = "full_text"),
      human_review = list(status = "pending")
    )

    # Validação do contrato
    if (exists("contrato_validar", mode = "function")) {
      chk <- contrato_validar(resposta_obj, tipo = "response", schema = schema, error = FALSE)
      if (!chk$valid) {
        # Se o valor não corresponder ao tipo, ajusta para erro_extracao mantendo integridade
        resposta_obj$status <- "erro_extracao"
        resposta_obj$value <- NULL
      }
    }

    respostas[[dim$id]] <- resposta_obj
  }

  list(
    responses = respostas,
    evidences = unname(evidencias),
    errors = erros,
    usage = list(
      input_tokens = total_in,
      output_tokens = total_out,
      cost = total_cost
    )
  )
}
