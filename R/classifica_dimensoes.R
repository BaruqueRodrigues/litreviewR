#' Classifica dimensões configuradas em um instrumento de revisão
#'
#' Gera perguntas Choice a partir das dimensões categóricas do instrumento e
#' envia apenas as dimensões cujo escopo corresponde à unidade fornecida.
#' Categorias múltiplas são avaliadas por perguntas binárias independentes.
#' Toda linha registra a unidade, o texto examinado, as probabilidades e as
#' referências de evidência recebidas; a API não cria evidências documentais.
#'
#' @param conteudo Texto efetivamente examinado (artigo, estudo ou passagem).
#' @param schema Instrumento retornado por `schema_padrao()` ou equivalente.
#' @param unidade Lista com `scope` (`article`, `study` ou `passage`),
#'   `unit_id`, e IDs contextuais opcionais (`article_id`, `study_id`,
#'   `passage_id`, `evidence_ids`).
#' @param confidence_threshold Limiar opcional entre zero e um. Quando omitido,
#'   aplica apenas um limiar explicitamente configurado na dimensão.
#' @param model Identificador do modelo TypeSafe.
#' @param api_key Chave opcional; por padrão, lida de `TYPESAFE_API_KEY`.
#' @param endpoint URL do endpoint System One.
#' @param timeout Limite em segundos para a chamada HTTP.
#' @param .transport Adaptador HTTP substituível para testes controlados.
#'
#' @return Data frame com uma linha por Choice (categoria única) ou por
#'   categoria (multicategoria). `status` é `extraido` ou `ambiguo`.
#' @export
classifica_dimensoes <- function(
    conteudo, schema, unidade, confidence_threshold = NULL,
    model = "jev-latest", api_key = Sys.getenv("TYPESAFE_API_KEY", unset = ""),
    endpoint = "https://api.typesafe.ai/v1/systemone", timeout = 60,
    .transport = .jev_http_post) {
  if (!is.character(conteudo) || length(conteudo) != 1L || is.na(conteudo) || !nzchar(conteudo)) {
    .jev_abort("invalid_state", "`conteudo` deve ser uma string não vazia.")
  }
  if (exists("schema_validar", mode = "function")) {
    checked <- schema_validar(schema, error = FALSE)
    if (!isTRUE(checked$valid)) {
      .jev_abort("invalid_schema", paste("Esquema inválido:", paste(checked$errors, collapse = "; ")))
    }
  }
  if (!is.list(unidade) || !unidade$scope %in% c("article", "study", "passage") ||
      is.null(unidade$unit_id) || length(unidade$unit_id) != 1L ||
      !is.character(unidade$unit_id) || is.na(unidade$unit_id) || !nzchar(unidade$unit_id)) {
    .jev_abort("invalid_unit", "`unidade` exige `scope` válido e `unit_id` não vazio.")
  }
  if (!is.null(confidence_threshold) &&
      (!is.numeric(confidence_threshold) || length(confidence_threshold) != 1L ||
       is.na(confidence_threshold) || confidence_threshold < 0 || confidence_threshold > 1)) {
    .jev_abort("invalid_threshold", "`confidence_threshold` deve ficar entre zero e um.")
  }
  dimensions <- schema$dimensions
  if (!is.list(dimensions)) .jev_abort("invalid_schema", "O esquema deve conter `dimensions` como lista.")
  dimensions <- Filter(function(d) {
    isTRUE(d$active %||% TRUE) && identical(d$scope, unidade$scope) &&
      "classify" %in% (d$operations %||% character()) &&
      d$type %in% c("single_category", "multiple_categories")
  }, dimensions)
  if (!length(dimensions)) return(.jev_empty_classification())
  requires_inference <- vapply(dimensions, function(d) !identical(d$inference_policy, "allow_inference"), logical(1))
  if (any(requires_inference)) {
    blocked <- vapply(dimensions[requires_inference], function(d) d$id %||% "?", character(1))
    .jev_abort(
      "inference_not_allowed",
      paste0("A classificação JEV produz atribuições inferidas. Configure `inference_policy = 'allow_inference'` nas dimensões: ",
             paste(blocked, collapse = ", "), ".")
    )
  }
  questions <- list()
  mapping <- list()
  dimension_by_id <- list()
  for (dimension in dimensions) {
    if (is.null(dimension$id) || is.null(dimension$categories) || !length(dimension$categories)) {
      .jev_abort("invalid_dimension", "Cada dimensão classificável precisa de ID e categorias.")
    }
    category_ids <- vapply(dimension$categories, function(category) category$id %||% NA_character_, character(1))
    if (anyNA(category_ids) || any(!nzchar(category_ids)) || anyDuplicated(category_ids)) {
      .jev_abort("invalid_dimension", paste0("Categorias inválidas ou duplicadas na dimensão `", dimension$id, "`."))
    }
    for (i in seq_along(dimension$categories)) {
      category <- dimension$categories[[i]]
      label <- category$label %||% category$id
      description <- category$description %||% ""
      if (dimension$type == "single_category") {
        qid <- sprintf("q%04d", length(questions) + 1L)
        indeterminate_id <- "__indeterminate__"
        while (indeterminate_id %in% category_ids) indeterminate_id <- paste0(indeterminate_id, "_")
        criteria <- stats::setNames(
          lapply(dimension$categories, function(item) {
            paste(item$label %||% item$id, item$description %||% "", sep = ": ")
          }),
          category_ids
        )
        criteria[[indeterminate_id]] <- "O texto examinado não fornece base suficiente para escolher uma categoria."
        if (i > 1L) next
        questions[[qid]] <- list(
          type = "choice",
          instructions = .jev_question_instruction(dimension, "Escolha exatamente uma categoria."),
          criteria = criteria
        )
        mapping[[qid]] <- list(dimension = dimension, category_id = NULL, category_label = NULL,
                               indeterminate_id = indeterminate_id)
        dimension_by_id[[dimension$id]] <- dimension
      } else {
        qid <- sprintf("q%04d", length(questions) + 1L)
        criteria <- list(
          yes = paste0("O conteúdo pertence a esta categoria: ", label, ". ", description),
          no = paste0("O conteúdo não pertence a esta categoria: ", label, ".")
        )
        questions[[qid]] <- list(
          type = "choice",
          instructions = paste(.jev_question_instruction(dimension, ""),
                               "Avalie somente a categoria indicada nesta pergunta.",
                               "Categoria:", label),
          criteria = criteria
        )
        mapping[[qid]] <- list(dimension = dimension, category_id = category$id, category_label = label)
        dimension_by_id[[dimension$id]] <- dimension
      }
    }
  }
  api_state <- list(
    content = conteudo,
    scope = unidade$scope,
    unit_id = unidade$unit_id,
    article_id = unidade$article_id %||% NULL,
    study_id = unidade$study_id %||% NULL,
    passage_id = unidade$passage_id %||% NULL
  )
  result <- jev_avalia(api_state, questions, model, endpoint, api_key, timeout, .transport)
  rows <- vector("list", length(mapping))
  for (i in seq_along(mapping)) {
    qid <- names(mapping)[[i]]
    spec <- mapping[[qid]]
    answer <- result$answers[[qid]]
    dimension <- spec$dimension
    threshold <- confidence_threshold
    if (is.null(threshold) && identical(dimension$review_rule$type %||% "", "below_threshold")) {
      threshold <- dimension$review_rule$threshold
    }
    indeterminate <- identical(answer$choice, spec$indeterminate_id) ||
      (!is.null(threshold) && answer$confidence < threshold)
    selected <- if (dimension$type == "single_category") {
      if (indeterminate) NA_character_ else answer$choice
    } else {
      if (indeterminate) NA else identical(answer$choice, "yes")
    }
    multi_selected <- if (dimension$type == "multiple_categories") selected else NA
    result_value <- if (dimension$type == "single_category") selected else if (isTRUE(selected)) spec$category_id else NA_character_
    rows[[i]] <- data.frame(
      schema_id = schema$schema_id %||% NA_character_,
      revision = schema$revision %||% NA_integer_,
      question_id = qid,
      dimension_id = dimension$id,
      unit_type = unidade$scope,
      unit_id = unidade$unit_id,
      article_id = unidade$article_id %||% NA_character_,
      study_id = unidade$study_id %||% NA_character_,
      passage_id = unidade$passage_id %||% NA_character_,
      category_id = spec$category_id %||% NA_character_,
      category_label = spec$category_label %||% NA_character_,
      value = result_value,
      membership = multi_selected,
      status = if (indeterminate) "ambiguo" else "extraido",
      value_origin = "inferred",
      confidence = answer$confidence,
      probabilities = I(list(answer$probabilities)),
      usage = I(list(result$usage)),
      evidence_ids = I(list(as.character(unidade$evidence_ids %||% character()))),
      coverage = "conteudo_informado",
      backend = "typesafe-systemone",
      model = result$model,
      content_examined = conteudo,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

.jev_question_instruction <- function(dimension, suffix) {
  parts <- c(dimension$instruction %||% dimension$label, dimension$definition, suffix)
  paste(Filter(function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x), parts), collapse = " ")
}

.jev_empty_classification <- function() {
  data.frame(
    schema_id = character(), revision = integer(), question_id = character(), dimension_id = character(),
    unit_type = character(), unit_id = character(), article_id = character(),
    study_id = character(), passage_id = character(), category_id = character(),
    category_label = character(), value = character(), membership = logical(), status = character(),
    value_origin = character(), confidence = numeric(), probabilities = I(list()), usage = I(list()),
    evidence_ids = I(list()), coverage = character(), backend = character(),
    model = character(), content_examined = character(), stringsAsFactors = FALSE
  )
}
