# Valores permitidos pelo contrato de respostas e estados auxiliares.
.estados_resposta <- c("extraido", "nao_encontrado", "nao_aplicavel", "ambiguo", "conflitante", "erro_extracao")
.estados_revisao <- c("pending", "accepted", "corrected", "rejected")
.tipos_contrato <- c("article", "document", "study", "passage", "response", "evidence", "run", "acquisition_attempt", "api_call")

.has_credential_field <- function(x) {
  if (!is.list(x)) return(FALSE)
  nm <- names(x)
  secret_name <- !is.null(nm) && any(grepl("^(api[_-]?key|access[_-]?token|auth(orization)?|password|passwd|secret|credential)$", nm, ignore.case = TRUE))
  secret_name || any(vapply(x, .has_credential_field, logical(1)))
}

#' Validar um objeto do corpus ou de uma execução
#'
#' Confere campos mínimos, valores de estado e, quando fornecidos, vínculos com
#' o corpus e com o instrumento. A validação de trechos é textual e não avalia
#' se semanticamente sustentam a resposta.
#'
#' @param x Objeto representado como lista nomeada.
#' @param tipo Tipo de contrato: `article`, `document`, `study`, `passage`,
#'   `response`, `evidence`, `run`, `acquisition_attempt` ou `api_call`.
#' @param corpus Opcionalmente, lista de passagens ou uma lista com elemento
#'   `passages`.
#' @param schema Instrumento opcional para validar dimensão e tipo do valor.
#' @param error Se `TRUE`, interrompe em caso de contrato inválido.
#' @return Lista com `valid` e vetor `errors`.
#' @export
contrato_validar <- function(x, tipo, corpus = NULL, schema = NULL, error = FALSE) {
  errors <- character()
  add_error <- function(message) errors <<- c(errors, message)
  string1 <- function(z) is.character(z) && length(z) == 1L && !is.na(z) && nzchar(z)
  enum1 <- function(z, values) string1(z) && z %in% values
  required_by_type <- list(
    article = c("article_id"),
    document = c("document_id", "article_id", "path", "acquisition_status", "extraction_status"),
    study = c("study_id", "article_id", "label", "passage_ids"),
    passage = c("passage_id", "document_id", "page_pdf", "text"),
    response = c("run_id", "schema_id", "revision", "dimension_id", "unit_type", "unit_id", "value", "status", "value_origin", "evidence_ids", "human_review"),
    evidence = c("evidence_id", "passage_id", "quote"),
    run = c("run_id", "schema_snapshot", "document_hashes", "software_version", "parameters", "tasks"),
    acquisition_attempt = c("attempt_id", "article_id", "source", "status", "parameters"),
    api_call = c("call_id", "run_id", "backend", "status", "parameters")
  )
  if (!is.list(x) || is.null(names(x))) {
    add_error("O objeto deve ser uma lista nomeada.")
  } else if (!string1(tipo) || !tipo %in% .tipos_contrato) {
    add_error(sprintf("Tipo de contrato desconhecido: %s", paste(tipo, collapse = ", ")))
  } else {
    missing <- setdiff(required_by_type[[tipo]], names(x))
    if (length(missing)) add_error(sprintf("Contrato `%s` sem campos: %s.", tipo, paste(missing, collapse = ", ")))
    for (field in intersect(c("article_id", "document_id", "study_id", "passage_id", "run_id", "dimension_id", "unit_id", "evidence_id", "attempt_id", "call_id", "schema_id", "backend", "model", "source", "path", "title", "label", "text", "quote", "unit_type", "value_origin", "software_version", "status"), names(x))) {
      if (!string1(x[[field]]) && !is.null(x[[field]])) add_error(sprintf("`%s` deve ser uma string não vazia.", field))
    }
    if (tipo == "document") {
      if (!enum1(x$acquisition_status, c("pending", "acquired", "failed", "not_available"))) add_error("`acquisition_status` inválido.")
      if (!enum1(x$extraction_status, c("pending", "extracted", "failed", "unreadable", "empty"))) add_error("`extraction_status` inválido.")
      if (!is.null(x$sha256) && (!string1(x$sha256) || !grepl("^[[:xdigit:]]{64}$", x$sha256))) add_error("`sha256` deve conter 64 dígitos hexadecimais.")
    }
    if (tipo == "study" && !is.list(x$passage_ids)) add_error("`passage_ids` deve ser uma lista de IDs de passagens.")
    if (tipo == "passage") {
      if (!is.numeric(x$page_pdf) || length(x$page_pdf) != 1L || is.na(x$page_pdf) || x$page_pdf < 1 || x$page_pdf %% 1 != 0) add_error("`page_pdf` deve ser um inteiro positivo.")
      if (!is.null(x$page_printed) && !(is.character(x$page_printed) || is.numeric(x$page_printed))) add_error("`page_printed` deve ser texto, número ou NULL.")
      if (!is.character(x$text) || length(x$text) != 1L || is.na(x$text)) add_error("`text` deve ser uma string.")
    }
    if (tipo == "response") {
      if (!is.numeric(x$revision) || length(x$revision) != 1L || is.na(x$revision) || x$revision < 1) add_error("`revision` deve ser positiva.")
      if (!enum1(x$status, .estados_resposta)) add_error("`status` de resposta não é reconhecido.")
      if (!enum1(x$value_origin, c("explicit", "inferred"))) add_error("`value_origin` deve ser explicit ou inferred.")
      if (!is.list(x$evidence_ids)) add_error("`evidence_ids` deve ser uma lista, inclusive para nenhuma evidência.")
      if (!is.list(x$human_review) || is.null(x$human_review$status) || !x$human_review$status %in% .estados_revisao) add_error("`human_review$status` deve ser pending, accepted, corrected ou rejected.")
      if (enum1(x$status, .estados_resposta) && x$status %in% c("nao_encontrado", "nao_aplicavel", "erro_extracao") && !is.null(x$value)) add_error(sprintf("Resposta `%s` deve ter value = NULL.", x$status))
      if (enum1(x$status, .estados_resposta) && x$status %in% c("extraido", "ambiguo", "conflitante") && is.null(x$value)) add_error(sprintf("Resposta `%s` requer um value preservado.", x$status))
      if (!is.null(schema)) {
        check <- schema_validar(schema, error = FALSE)
        if (!check$valid) add_error("O schema fornecido para comparação é inválido.") else {
          ids <- vapply(schema$dimensions, function(d) if (is.character(d$id)) d$id else "", character(1))
          pos <- match(x$dimension_id, ids)
          if (is.na(pos)) add_error(sprintf("Dimensão desconhecida: %s.", x$dimension_id)) else {
            dim <- schema$dimensions[[pos]]
            if (identical(x$value_origin, "inferred") && identical(dim$inference_policy, "explicit_only")) add_error("A dimensão proíbe inferências, mas a resposta foi marcada como inferred.")
            val <- x$value
            valid_value <- switch(dim$type,
              text = is.character(val) && length(val) == 1L,
              number = is.numeric(val) && length(val) == 1L && !is.na(val),
              date = (inherits(val, "Date") && length(val) == 1L) || (is.character(val) && length(val) == 1L && grepl("^\\d{4}-\\d{2}-\\d{2}$", val)),
              single_category = is.character(val) && length(val) == 1L && val %in% vapply(dim$categories, `[[`, character(1), "id"),
              multiple_categories = is.list(val) && all(vapply(val, function(v) is.character(v) && length(v) == 1L && v %in% vapply(dim$categories, `[[`, character(1), "id"), logical(1))),
              item_list = is.list(val), FALSE
            )
            if (!is.null(val) && !valid_value) add_error(sprintf("`value` não corresponde ao tipo `%s` da dimensão.", dim$type))
          }
        }
      }
      refs <- unlist(x$evidence_ids, use.names = FALSE)
      if (anyDuplicated(refs)) add_error("`evidence_ids` contém referências duplicadas.")
    }
    if (tipo == "evidence") {
      if (!is.character(x$quote) || length(x$quote) != 1L || is.na(x$quote) || !nzchar(x$quote)) add_error("`quote` deve ser um trecho não vazio.")
      if (!is.null(x$start) || !is.null(x$end)) {
        if (!is.numeric(x$start) || !is.numeric(x$end) || length(x$start) != 1L || length(x$end) != 1L || is.na(x$start) || is.na(x$end) || x$start < 1 || x$end < x$start) add_error("`start` e `end` devem formar uma faixa válida de caracteres.")
      }
    }
    if (tipo == "run") {
      if (!is.list(x$schema_snapshot)) add_error("`schema_snapshot` deve conter o instrumento usado na execução.")
      if (!is.list(x$document_hashes) || !is.list(x$parameters) || !is.list(x$tasks)) add_error("`document_hashes`, `parameters` e `tasks` devem ser listas.")
    }
    if (tipo %in% c("acquisition_attempt", "api_call")) {
      if (!is.list(x$parameters)) add_error("`parameters` deve ser uma lista sem credenciais.")
      if (.has_credential_field(x$parameters)) add_error("`parameters` não pode conter credenciais.")
      if (!is.null(x$started_at) && !is.character(x$started_at)) add_error("`started_at` deve ser timestamp textual.")
      if (!is.null(x$finished_at) && !is.character(x$finished_at)) add_error("`finished_at` deve ser timestamp textual.")
      if (!is.null(x$error) && !is.list(x$error) && !is.character(x$error)) add_error("`error` deve ser texto ou objeto estruturado.")
    }
  }

  if (!is.list(x) || is.null(names(x)) || !enum1(tipo, .tipos_contrato)) {
    result <- list(valid = FALSE, errors = unique(errors))
    if (error) stop(paste(result$errors, collapse = "\n"), call. = FALSE)
    return(result)
  }

  if (tipo == "evidence" && !is.null(corpus) && is.list(x)) {
    passages <- if (is.list(corpus$passages)) corpus$passages else corpus
    pids <- vapply(passages, function(p) if (is.list(p) && is.character(p$passage_id)) p$passage_id else "", character(1))
    at <- match(x$passage_id, pids)
    if (is.na(at)) add_error(sprintf("A evidência referencia passagem ausente `%s`.", x$passage_id)) else {
      passage_text <- passages[[at]]$text
      if (is.character(x$quote) && length(x$quote) == 1L && !is.null(passage_text) && !grepl(x$quote, passage_text, fixed = TRUE)) add_error("`quote` não ocorre literalmente na passagem referenciada.")
      if (!is.null(x$start) && !is.null(x$end) && is.character(passage_text) && length(passage_text) == 1L) {
        n <- nchar(passage_text, type = "chars")
        if (x$end > n || substr(passage_text, x$start, x$end) != x$quote) add_error("A faixa start/end não corresponde a quote na passagem.")
      }
    }
  }
  result <- list(valid = length(errors) == 0L, errors = unique(errors))
  if (error && !result$valid) stop(paste(result$errors, collapse = "\n"), call. = FALSE)
  result
}
