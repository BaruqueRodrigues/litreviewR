#' Adaptador HTTP para modelos de linguagem (LLM)
#'
#' Envia requisições estruturadas para APIs de LLM compatíveis com Chat Completions.
#' A chave de API é lida do ambiente por padrão (ex: `OPENAI_API_KEY`).
#' Credenciais nunca são salvas nem expostas em mensagens de erro ou logs.
#'
#' @param prompt Instruções e contexto enviados ao modelo.
#' @param model Identificador do modelo. O padrão é `gpt-4o-mini` para OpenAI e
#'   `deepseek-flash` para DeepSeek.
#' @param system_prompt Instrução de sistema delimitando a tarefa.
#' @param api_key Chave opcional. Por padrão, lida de `OPENAI_API_KEY` ou
#'   `DEEPSEEK_API_KEY`, conforme o provedor.
#' @param endpoint URL opcional do endpoint de chat completions. Cada provedor
#'   tem um endpoint padrão.
#' @param temperature Temperatura de amostragem (default: 0).
#' @param max_tokens Limite de tokens na resposta (default: 2000).
#' @param timeout Limite em segundos para a requisição HTTP (default: 60).
#' @param mock_response Resposta simulada opcional para testes sem rede ou sem chave.
#' @param .transport Função alternativa de transporte HTTP para testes.
#' @param provider Provedor compatível com Chat Completions: `openai` ou `deepseek`.
#'
#' @return Lista contendo `content` (texto da resposta), `usage` (tokens in/out e custo) e `model`.
#' @export
llm_chat <- function(prompt,
                     model = NULL,
                     system_prompt = "Você é um assistente de pesquisa científica rigoroso e objetivo.",
                     api_key = NULL,
                     endpoint = NULL,
                     temperature = 0,
                     max_tokens = 2000,
                     timeout = 60,
                     mock_response = NULL,
                     .transport = NULL,
                     provider = c("openai", "deepseek")) {

  provider <- match.arg(provider)
  if (is.null(model)) model <- if (provider == "deepseek") "deepseek-flash" else "gpt-4o-mini"
  if (is.null(api_key)) {
    key_name <- if (provider == "deepseek") "DEEPSEEK_API_KEY" else "OPENAI_API_KEY"
    api_key <- Sys.getenv(key_name, unset = "")
  }
  if (is.null(endpoint)) {
    endpoint <- if (provider == "deepseek") {
      "https://api.deepseek.com/chat/completions"
    } else {
      "https://api.openai.com/v1/chat/completions"
    }
  }

  if (!is.character(prompt) || length(prompt) != 1L || is.na(prompt) || !nzchar(prompt)) {
    .llm_abort("invalid_prompt", "`prompt` deve ser uma string não vazia.")
  }

  # Se houver mock_response fornecido diretamente
  if (!is.null(mock_response)) {
    if (is.character(mock_response) && length(mock_response) == 1L) {
      return(list(
        content = mock_response,
        usage = list(input_tokens = 100L, output_tokens = 30L, cost = 0.0001),
        model = model
      ))
    } else if (is.list(mock_response) && !is.null(mock_response$content)) {
      return(mock_response)
    }
  }

  # Se houver transporte customizado
  if (!is.null(.transport)) {
    if (!is.function(.transport)) {
      .llm_abort("invalid_transport", "`.transport` deve ser uma função.")
    }
    return(.transport(prompt = prompt, model = model, system_prompt = system_prompt,
                      api_key = api_key, endpoint = endpoint, temperature = temperature,
                      max_tokens = max_tokens, timeout = timeout))
  }

  # Checagem de chave para chamadas reais
  if (!is.character(api_key) || length(api_key) != 1L || !nzchar(api_key)) {
    key_name <- if (provider == "deepseek") "DEEPSEEK_API_KEY" else "OPENAI_API_KEY"
    .llm_abort(
      "missing_credentials",
      paste0("Credencial de API LLM ausente. Defina `", key_name,
             "` no ambiente ou forneça `api_key` ou `mock_response`.")
    )
  }

  payload <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = prompt)
    ),
    temperature = temperature,
    max_tokens = max_tokens,
    response_format = list(type = "json_object")
  )

  resp <- tryCatch(
    httr::POST(
      endpoint,
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = jsonlite::toJSON(payload, auto_unbox = TRUE),
      encode = "raw",
      httr::timeout(timeout)
    ),
    error = function(e) {
      .llm_abort("transport_error", paste0("Falha na conexão HTTP com API LLM: ", conditionMessage(e)))
    }
  )

  status <- httr::status_code(resp)
  if (status < 200L || status >= 300L) {
    .llm_abort("http_error", paste0("API LLM retornou erro HTTP ", status), status)
  }

  body_text <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(
    jsonlite::fromJSON(body_text, simplifyVector = FALSE),
    error = function(e) .llm_abort("invalid_json", "Resposta da API LLM não é JSON válido.")
  )

  content_text <- parsed$choices[[1]]$message$content
  usage_info <- parsed$usage

  list(
    content = content_text,
    usage = list(
      input_tokens = as.integer(usage_info$prompt_tokens %||% 0L),
      output_tokens = as.integer(usage_info$completion_tokens %||% 0L),
      cost = llm_calcula_custo(usage_info$prompt_tokens %||% 0L,
                               usage_info$completion_tokens %||% 0L,
                               model)
    ),
    finish_reason = parsed$choices[[1]]$finish_reason %||% NA_character_,
    model = parsed$model %||% model
  )
}

.llm_abort <- function(code, message, status_code = NA_integer_) {
  condition <- structure(
    list(message = message, call = NULL, code = code, status_code = status_code),
    class = c("litreview_llm_error", "error", "condition")
  )
  stop(condition)
}

#' Calcula custo estimado de tokens para LLMs
#'
#' @param input_tokens Quantidade de tokens de entrada.
#' @param output_tokens Quantidade de tokens de saída.
#' @param model Identificador do modelo.
#' @return Valor numérico estimado em USD.
#' @export
llm_calcula_custo <- function(input_tokens, output_tokens, model = "gpt-4o-mini") {
  # Taxas de referência (por 1 milhão de tokens)
  taxas <- list(
    `gpt-4o-mini` = list(input = 0.15, output = 0.60),
    `gpt-4o` = list(input = 2.50, output = 10.00),
    # Tarifa DeepSeek-V4.1-Flash fora do horário de pico, publicada em 2026-10-03.
    # A tarifa de pico é o dobro; confirme o período/tarifa ao registrar o piloto.
    `deepseek-flash` = list(input = 0.15, output = 0.60),
    `gemini-1.5-flash` = list(input = 0.075, output = 0.30),
    `default` = list(input = 0.20, output = 0.80)
  )
  taxa <- taxas[[model]] %||% taxas[["default"]]
  (input_tokens / 1e6 * taxa$input) + (output_tokens / 1e6 * taxa$output)
}

#' Constrói prompt de extração estruturada para uma dimensão
#'
#' @param dimension Lista definindo a dimensão segundo o contrato F0.
#' @param unit_type Escopo da unidade analítica ("article", "study", "passage").
#' @return String com instruções claras para a extração em formato JSON estrito.
#' @export
llm_constroi_prompt_dimensao <- function(dimension, unit_type = "article") {
  instrucoes_tipo <- switch(
    dimension$type,
    text = "O valor deve ser uma string com a informação textual extraída.",
    number = "O valor deve ser um único número (int ou float).",
    date = "O valor deve ser uma data no formato AAAA-MM-DD.",
    single_category = paste0(
      "O valor deve ser exatamente um dos seguintes identificadores de categoria: ",
      paste(vapply(dimension$categories, function(c) c$id, character(1)), collapse = ", "), "."
    ),
    multiple_categories = paste0(
      "O valor deve ser uma lista com os identificadores aplicáveis dentre: ",
      paste(vapply(dimension$categories, function(c) c$id, character(1)), collapse = ", "), "."
    ),
    item_list = "O valor deve ser uma lista (array) de strings.",
    "O valor deve ser extraído conforme a definição."
  )

  instrucoes_politica <- if (identical(dimension$inference_policy, "explicit_only")) {
    "ATENÇÃO: Política 'explicit_only'. Extraia APENAS o que estiver EXPLICITAMENTE afirmado no texto. Não deduza nem extrapole. Se não estiver explicitamente presente, defina status como 'nao_encontrado'."
  } else {
    "Política 'allow_inference'. É permitida inferência bem fundamentada pelo texto. Se inferido, marque 'value_origin' como 'inferred'."
  }

  prompt <- paste0(
    "Tarefa de Extração de Dados Científicos\n",
    "Dimensão: ", dimension$label, " (ID: ", dimension$id, ")\n",
    "Definição: ", dimension$definition, "\n",
    "Instrução: ", dimension$instruction, "\n",
    "Tipo de dado esperado: ", dimension$type, " (", instrucoes_tipo, ")\n",
    "Unidade de análise: ", unit_type, "\n",
    instrucoes_politica, "\n\n",
    "Você deve retornar OBRIGATORIAMENTE um objeto JSON com o seguinte formato exato:\n",
    "{\n",
    '  "status": "extraido" | "nao_encontrado" | "nao_aplicavel" | "ambiguo" | "conflitante",\n',
    '  "value": <valor conforme o tipo, ou null se nao_encontrado/nao_aplicavel>,\n',
    '  "value_origin": "explicit" | "inferred",\n',
    '  "quote": "<trecho EXATO e contíguo do texto que serve de evidência direta, ou null se não houver>",\n',
    '  "reasoning": "<breve justificativa>"\n',
    "}\n\n",
    "Regras adicionais:\n",
    "- Se a informação não constar do texto fornecido, use status='nao_encontrado' e value=null.\n",
    "- Se houver ambiguidade explícita entre opções no texto, use status='ambiguo'.\n",
    "- Mantenha 'value' conciso: no máximo 150 palavras; listas devem ter no máximo 5 itens, agrupando itens próximos.\n",
    "- 'quote' deve ser uma única evidência curta, com no máximo 35 palavras, copiada literalmente e em sequência do texto.\n",
    "- 'reasoning' deve ter no máximo 30 palavras. Não repita o artigo nem inclua outras citações.\n",
    "- O campo 'quote' DEVE ser uma cópia literal, exata e contígua de um trecho contido no texto analisado. NUNCA invente ou altere palavras na citação.\n"
  )

  prompt
}

#' Realiza o parse seguro de resposta JSON do LLM para extração
#'
#' @param content String contendo a resposta JSON do LLM.
#' @param dimension Dimensão correspondente do esquema.
#' @return Lista estruturada com campos normalizados.
#' @export
llm_parse_resposta <- function(content, dimension) {
  parse_error <- NULL
  parsed <- tryCatch(
    jsonlite::fromJSON(content, simplifyVector = FALSE),
    error = function(e) {
      parse_error <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(parsed)) {
    return(list(
      status = "erro_extracao", value = NULL, value_origin = "explicit",
      quote = NULL, reasoning = "", error_code = "invalid_json",
      error_message = paste("Resposta JSON inválida ou truncada:", parse_error)
    ))
  }

  status <- parsed$status %||% "erro_extracao"
  if (!status %in% c("extraido", "nao_encontrado", "nao_aplicavel", "ambiguo", "conflitante", "erro_extracao")) {
    return(list(
      status = "erro_extracao", value = NULL, value_origin = "explicit",
      quote = NULL, reasoning = "", error_code = "invalid_status",
      error_message = paste("Status de resposta não reconhecido:", as.character(status))
    ))
  }

  value <- parsed$value
  if (status %in% c("nao_encontrado", "nao_aplicavel", "erro_extracao")) {
    value <- NULL
  }

  value_origin <- parsed$value_origin %||% "explicit"
  if (!value_origin %in% c("explicit", "inferred")) {
    value_origin <- "explicit"
  }

  if (identical(dimension$inference_policy, "explicit_only") && value_origin == "inferred") {
    # Política proíbe inferência
    if (!is.null(value)) {
      status <- "nao_encontrado"
      value <- NULL
      value_origin <- "explicit"
    }
  }

  quote <- parsed$quote
  if (is.null(quote) || !is.character(quote) || length(quote) != 1L || !nzchar(trimws(quote))) {
    quote <- NULL
  }

  list(
    status = status,
    value = value,
    value_origin = value_origin,
    quote = quote,
    reasoning = parsed$reasoning %||% "",
    error_code = NULL,
    error_message = NULL
  )
}
