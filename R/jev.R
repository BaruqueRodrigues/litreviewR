#' Avalia classificações com a API TypeSafe JEV
#'
#' Este adaptador usa diretamente o endpoint HTTP System One da TypeSafe.
#' Defina a variável de ambiente `TYPESAFE_API_KEY` antes da chamada. A função
#' não grava nem inclui a chave no resultado ou nas mensagens de erro.
#'
#' @param state Conteúdo a classificar, como texto de artigo, estudo ou passagem.
#' @param questions Mapa nomeado de perguntas Choice já no formato da API.
#' @param model Identificador do modelo TypeSafe. O padrão é `jev-latest`.
#' @param endpoint URL do endpoint System One.
#' @param api_key Chave opcional; por padrão, lida de `TYPESAFE_API_KEY`.
#' @param timeout Limite em segundos para a chamada HTTP.
#' @param .transport Adaptador substituível para testes sem chamadas reais.
#'
#' @return Lista com modelo, respostas tipadas, uso informado e conteúdo avaliado.
#' @export
jev_avalia <- function(state, questions, model = "jev-latest",
                       endpoint = "https://api.typesafe.ai/v1/systemone",
                       api_key = Sys.getenv("TYPESAFE_API_KEY", unset = ""),
                       timeout = 60, .transport = .jev_http_post) {
  valid_state <- (is.character(state) && length(state) == 1L && !is.na(state) && nzchar(state)) ||
    (is.list(state) && length(state) > 0L)
  if (!valid_state) {
    .jev_abort("invalid_state", "`state` deve ser texto não vazio ou um objeto estruturado.")
  }
  if (!is.list(questions) || !length(questions) || is.null(names(questions)) ||
      any(!nzchar(names(questions))) || anyDuplicated(names(questions))) {
    .jev_abort("invalid_questions", "`questions` deve ser uma lista não vazia com IDs únicos.")
  }
  if (!is.character(api_key) || length(api_key) != 1L || !nzchar(api_key)) {
    .jev_abort(
      "missing_credentials",
      "Credencial JEV ausente. Defina `TYPESAFE_API_KEY` no ambiente e tente novamente."
    )
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) || timeout <= 0) {
    .jev_abort("invalid_timeout", "`timeout` deve ser um número positivo.")
  }
  payload <- list(state = state, model = model, questions = questions)
  response <- tryCatch(
    .transport(payload, api_key, endpoint, timeout),
    litreview_jev_error = function(e) stop(e),
    error = function(e) .jev_abort("transport_error", "Falha durante a chamada à API JEV.")
  )
  expected_options <- lapply(questions, function(question) names(question$criteria))
  .jev_validate_api_response(response, names(questions), expected_options)
  list(
    model = response$model,
    answers = response$answers,
    usage = response$usage,
    state = state
  )
}

.jev_abort <- function(code, message, status_code = NA_integer_) {
  condition <- structure(
    list(message = message, call = NULL, code = code, status_code = status_code),
    class = c("litreview_jev_error", "error", "condition")
  )
  stop(condition)
}

.jev_http_post <- function(payload, api_key, endpoint, timeout) {
  response <- tryCatch(
    httr::POST(
      endpoint,
      httr::add_headers(Authorization = paste("Bearer", api_key)),
      httr::content_type_json(),
      body = payload,
      encode = "json",
      httr::timeout(timeout)
    ),
    error = function(e) {
      .jev_abort("transport_error", paste0("Falha na conexão com a API JEV: ", conditionMessage(e)))
    }
  )
  status <- httr::status_code(response)
  parsed <- tryCatch(
    httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8"),
    error = function(e) NULL
  )
  if (status < 200L || status >= 300L) {
    messages <- c(
      `401` = "Chave ausente ou inválida.",
      `422` = "A API rejeitou a estrutura da pergunta.",
      `429` = "Limite de chamadas excedido.",
      `529` = "A API está temporariamente sobrecarregada."
    )
    detail <- unname(messages[as.character(status)])
    if (!length(detail) || is.na(detail)) detail <- "A API retornou uma resposta de erro."
    .jev_abort("http_error", paste0("JEV HTTP ", status, ": ", detail), status)
  }
  if (!is.list(parsed)) .jev_abort("invalid_response", "A API JEV retornou JSON inválido.", status)
  parsed
}

.jev_validate_api_response <- function(response, question_ids, expected_options = NULL) {
  if (!is.list(response) || !is.list(response$answers) ||
      !is.character(response$model) || length(response$model) != 1L ||
      is.na(response$model) || !nzchar(response$model)) {
    .jev_abort("invalid_response", "A resposta JEV não contém modelo e `answers` válidas.")
  }
  missing <- setdiff(question_ids, names(response$answers))
  if (length(missing)) {
    .jev_abort("invalid_response", paste0("A resposta JEV não retornou: ", paste(missing, collapse = ", "), "."))
  }
  for (id in question_ids) {
    answer <- response$answers[[id]]
    if (!is.list(answer) || !identical(answer$type, "choice") ||
        !is.character(answer$choice) || length(answer$choice) != 1L ||
        is.na(answer$choice) || !nzchar(answer$choice) ||
        !is.list(answer$probabilities) || !is.numeric(answer$confidence) ||
        length(answer$confidence) != 1L || !is.finite(answer$confidence) ||
        answer$confidence < 0 || answer$confidence > 1) {
      .jev_abort("invalid_response", paste0("Resposta JEV inválida para a pergunta `", id, "`."))
    }
    probabilities <- unlist(answer$probabilities, use.names = TRUE)
    if (!length(probabilities) || is.null(names(probabilities)) || anyDuplicated(names(probabilities)) ||
        any(!is.finite(probabilities)) || any(probabilities < 0 | probabilities > 1) ||
        abs(sum(probabilities) - 1) > 1e-5 || !answer$choice %in% names(probabilities)) {
      .jev_abort("invalid_response", paste0("Probabilidades inválidas para a pergunta `", id, "`."))
    }
    if (!is.null(expected_options) &&
        (!answer$choice %in% expected_options[[id]] ||
         !setequal(names(probabilities), expected_options[[id]]))) {
      .jev_abort("invalid_response", paste0("Resposta JEV usou opções fora do contrato para `", id, "`."))
    }
  }
  invisible(TRUE)
}
