#' Instrumento geral configurável para ciências sociais
#'
#' @return Lista compatível com o formato JSON de instrumento 1.0.0.
#' @export
schema_padrao <- function() {
  definicoes <- list(
    list("research_question", "Pergunta de pesquisa", "Problema ou pergunta que o trabalho procura responder.", "Registre a pergunta como formulada pelos autores."),
    list("objective", "Objetivo", "Objetivo declarado pelo trabalho.", "Registre o objetivo declarado pelos autores."),
    list("central_concepts", "Conceitos centrais", "Conceitos centrais e definições usadas no trabalho.", "Liste os conceitos e suas definições conforme o texto."),
    list("theoretical_orientation", "Orientação teórica", "Corrente ou orientação teórica explicitamente adotada.", "Registre apenas a orientação que os autores adotam explicitamente."),
    list("hypotheses_propositions", "Hipóteses ou proposições", "Hipóteses, proposições ou expectativas formuladas pelos autores.", "Transcreva ou resuma as hipóteses e proposições declaradas."),
    list("explanatory_mechanism", "Mecanismo explicativo", "Processo que conecta causas, condições e resultados.", "Descreva o mecanismo tal como apresentado pelos autores."),
    list("context", "Contexto", "Local, período e contexto institucional ou social do trabalho.", "Registre o contexto geográfico, temporal e institucional informado."),
    list("unit_of_analysis", "Unidade de análise", "Entidades, pessoas, casos ou materiais analisados.", "Identifique a unidade de análise explicitamente descrita."),
    list("research_design", "Desenho de pesquisa", "Estratégia geral de investigação.", "Classifique ou descreva o desenho indicado pelos autores."),
    list("data_sources", "Dados", "Fontes, amostra, casos ou materiais utilizados.", "Descreva as fontes de dados e a amostra ou seleção de casos."),
    list("analysis_method", "Método de análise", "Métodos usados para analisar os dados.", "Registre os métodos analíticos descritos no artigo."),
    list("variables_categories", "Variáveis ou categorias", "Variáveis quantitativas ou categorias qualitativas relevantes.", "Liste os desfechos, fatores explicativos ou categorias usados."),
    list("main_results", "Resultados principais", "Achados que respondem à pergunta de pesquisa.", "Resuma os resultados relatados pelos autores."),
    list("stated_limitations", "Limitações declaradas", "Limitações reconhecidas pelos autores.", "Registre limitações declaradas; não deduza limitações próprias.")
  )

  dimensions <- lapply(definicoes, function(x) {
    list(
      id = x[[1]], label = x[[2]], definition = x[[3]], active = TRUE,
      type = "text", scope = "article", operations = list("extract"),
      instruction = x[[4]], search_terms = list(), include_examples = list(),
      exclude_examples = list(), evidence_required = TRUE,
      categories = list(), scoring_rubric = list(),
      applicability = list(all = list(), any = list()),
      inference_policy = "explicit_only",
      review_rule = list(type = "if_ambiguous", threshold = NULL, experimental = FALSE)
    )
  })

  list(
    schema_id = "litreview-social-sciences",
    format_version = "1.0.0",
    revision = 1L,
    title = "Instrumento geral de ciências sociais",
    area = "",
    instruction_language = "pt-BR",
    dimensions = dimensions
  )
}

#' Validar um instrumento de extração
#'
#' Verifica a estrutura, identificadores, tipos e referências declarativas do
#' instrumento. Nenhum código ou expressão contido no esquema é executado.
#'
#' @param schema Lista de instrumento.
#' @param error Se `TRUE`, interrompe quando o esquema é inválido.
#' @return Lista com `valid` e vetor `errors`.
#' @export
schema_validar <- function(schema, error = FALSE) {
  errors <- character()
  add_error <- function(message) errors <<- c(errors, message)
  has_fields <- function(x, fields) is.list(x) && all(fields %in% names(x))
  scalar_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  list_of <- function(x) is.list(x)
  enum_scalar <- function(x, allowed) scalar_string(x) && x %in% allowed
  enum_vector <- function(x, allowed, nonempty = TRUE) {
    is.character(x) && (!nonempty || length(x) > 0L) && !anyNA(x) && all(x %in% allowed)
  }

  required <- c("schema_id", "format_version", "revision", "title", "area",
                "instruction_language", "dimensions")
  if (!has_fields(schema, required)) {
    add_error(paste0("O instrumento deve conter: ", paste(required, collapse = ", "), "."))
    return(.schema_validation_result(errors, error))
  }
  for (field in c("schema_id", "format_version", "title", "area", "instruction_language")) {
    if (!scalar_string(schema[[field]])) add_error(sprintf("`%s` deve ser uma string escalar.", field))
  }
  if (!is.numeric(schema$revision) || length(schema$revision) != 1L || is.na(schema$revision) || schema$revision < 1) {
    add_error("`revision` deve ser um inteiro positivo.")
  }
  if (!list_of(schema$dimensions)) add_error("`dimensions` deve ser uma lista ordenada.")
  if (!list_of(schema$dimensions)) return(.schema_validation_result(errors, error))

  dim_required <- c("id", "label", "definition", "active", "type", "scope", "operations",
                    "instruction", "search_terms", "include_examples", "exclude_examples",
                    "evidence_required", "categories", "scoring_rubric", "applicability",
                    "inference_policy", "review_rule")
  dim_ids <- character()
  for (i in seq_along(schema$dimensions)) {
    d <- schema$dimensions[[i]]
    prefix <- sprintf("dimensions[[%d]]", i)
    if (!has_fields(d, dim_required)) {
      add_error(sprintf("%s deve conter os campos obrigatórios do contrato.", prefix))
      next
    }
    for (field in c("id", "label", "definition", "type", "scope", "instruction", "inference_policy")) {
      if (!scalar_string(d[[field]])) add_error(sprintf("%s$%s deve ser uma string escalar.", prefix, field))
    }
    if (scalar_string(d$id)) dim_ids <- c(dim_ids, d$id)
    if (!is.logical(d$active) || length(d$active) != 1L || is.na(d$active)) add_error(sprintf("%s$active deve ser lógico.", prefix))
    if (!is.logical(d$evidence_required) || length(d$evidence_required) != 1L || is.na(d$evidence_required)) add_error(sprintf("%s$evidence_required deve ser lógico.", prefix))
    type_ok <- enum_scalar(d$type, c("text", "number", "date", "single_category", "multiple_categories", "item_list"))
    scope_ok <- enum_scalar(d$scope, c("article", "study", "passage"))
    if (!type_ok) add_error(sprintf("%s$type não é reconhecido.", prefix))
    if (!scope_ok) add_error(sprintf("%s$scope não é reconhecido.", prefix))
    operations <- if (list_of(d$operations)) unlist(d$operations, use.names = FALSE) else d$operations
    if (!enum_vector(operations, c("extract", "classify"))) add_error(sprintf("%s$operations aceita `extract` e/ou `classify`.", prefix))
    inference_ok <- enum_scalar(d$inference_policy, c("explicit_only", "allow_inference"))
    if (!inference_ok) add_error(sprintf("%s$inference_policy não é reconhecida.", prefix))
    for (field in c("search_terms", "include_examples", "exclude_examples", "categories", "scoring_rubric")) {
      if (!list_of(d[[field]])) add_error(sprintf("%s$%s deve ser uma lista.", prefix, field))
    }
    categories <- d$categories
    categorical <- type_ok && d$type %in% c("single_category", "multiple_categories")
    if (categorical && !length(categories)) add_error(sprintf("%s requer categorias para o tipo %s.", prefix, d$type))
    if (type_ok && !categorical && length(categories)) add_error(sprintf("%s não pode definir categorias para o tipo %s.", prefix, d$type))
    cat_ids <- character()
    for (j in seq_along(categories)) {
      cat <- categories[[j]]
      if (!has_fields(cat, c("id", "label", "description")) ||
          !all(vapply(cat[c("id", "label", "description")], scalar_string, logical(1)))) {
        add_error(sprintf("%s$categories[[%d]] deve conter id, label e description como strings.", prefix, j))
      } else cat_ids <- c(cat_ids, cat$id)
    }
    if (anyDuplicated(cat_ids)) add_error(sprintf("%s contém IDs de categoria duplicados.", prefix))
    rubric <- d$scoring_rubric
    if (length(rubric) && type_ok && d$type != "number") add_error(sprintf("%s só pode usar rubrica de escore com tipo number.", prefix))
    if (length(rubric)) {
      for (j in seq_along(rubric)) {
        r <- rubric[[j]]
        if (!has_fields(r, c("value", "label", "description")) || !is.numeric(r$value) || length(r$value) != 1L ||
            !scalar_string(r$label) || !scalar_string(r$description)) add_error(sprintf("%s$scoring_rubric[[%d]] é inválida.", prefix, j))
      }
    }
    app <- d$applicability
    if (!has_fields(app, c("all", "any")) || !list_of(app$all) || !list_of(app$any)) {
      add_error(sprintf("%s$applicability deve conter listas `all` e `any`.", prefix))
    } else {
      for (group in c("all", "any")) for (j in seq_along(app[[group]])) {
        clause <- app[[group]][[j]]
        if (!has_fields(clause, c("dimension_id", "operator")) || !scalar_string(clause$dimension_id) ||
            !enum_scalar(clause$operator, c("equals", "not_equals", "in", "not_in", "has_value", "is_empty"))) {
          add_error(sprintf("%s$applicability$%s[[%d]] é inválida.", prefix, group, j))
        } else if (clause$operator %in% c("equals", "not_equals", "in", "not_in") && !"value" %in% names(clause)) {
          add_error(sprintf("%s$applicability$%s[[%d]] requer `value` para o operador %s.", prefix, group, j, clause$operator))
        }
      }
    }
    rule <- d$review_rule
    if (!has_fields(rule, c("type", "threshold", "experimental")) ||
        !enum_scalar(rule$type, c("none", "always", "if_ambiguous", "below_threshold", "unknown_category"))) {
      add_error(sprintf("%s$review_rule é inválida.", prefix))
    } else {
      if (rule$type == "below_threshold" && (!is.numeric(rule$threshold) || length(rule$threshold) != 1L || is.na(rule$threshold) || rule$threshold < 0 || rule$threshold > 1)) add_error(sprintf("%s$review_rule requer threshold entre zero e um.", prefix))
      if (rule$type != "below_threshold" && !is.null(rule$threshold)) add_error(sprintf("%s$review_rule só admite threshold para below_threshold.", prefix))
      if (!is.logical(rule$experimental) || length(rule$experimental) != 1L || is.na(rule$experimental)) add_error(sprintf("%s$review_rule$experimental deve ser lógico.", prefix))
    }
  }
  if (anyDuplicated(dim_ids)) add_error("O instrumento contém IDs de dimensão duplicados.")
  for (i in seq_along(schema$dimensions)) {
    d <- schema$dimensions[[i]]
    if (is.list(d$applicability)) for (group in c("all", "any")) {
      for (clause in d$applicability[[group]]) {
        if (is.list(clause) && is.character(clause$dimension_id) && length(clause$dimension_id) == 1L &&
            !clause$dimension_id %in% dim_ids) add_error(sprintf("A regra de `%s` referencia dimensão inexistente `%s`.", d$id %||% "?", clause$dimension_id))
        if (is.list(clause) && identical(clause$dimension_id, d$id)) add_error(sprintf("A dimensão `%s` não pode condicionar a própria aplicabilidade.", d$id))
      }
    }
  }
  .schema_validation_result(errors, error)
}

.schema_validation_result <- function(errors, error) {
  result <- list(valid = length(errors) == 0L, errors = unique(errors))
  if (error && !result$valid) stop(paste(result$errors, collapse = "\n"), call. = FALSE)
  result
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Serializar um instrumento para JSON
#' @param schema Instrumento válido.
#' @param pretty Indica se o JSON deve ser indentado.
#' @return String JSON.
#' @export
schema_para_json <- function(schema, pretty = TRUE) {
  schema_validar(schema, error = TRUE)
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("O pacote `jsonlite` é necessário para serializar esquemas.", call. = FALSE)
  as.character(jsonlite::toJSON(schema, auto_unbox = TRUE, pretty = pretty,
                               null = "null", na = "null", dataframe = "rows"))
}

#' Desserializar instrumento JSON
#' @param texto String JSON.
#' @param error Interrompe quando o JSON não puder ser lido ou validado.
#' @return Instrumento como lista ou erro de leitura/validação.
#' @export
schema_de_json <- function(texto, error = TRUE) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("O pacote `jsonlite` é necessário para ler esquemas.", call. = FALSE)
  out <- tryCatch(jsonlite::fromJSON(texto, simplifyVector = FALSE), error = function(e) e)
  if (inherits(out, "error")) {
    if (error) stop(conditionMessage(out), call. = FALSE)
    return(list(valid = FALSE, errors = conditionMessage(out)))
  }
  check <- schema_validar(out, error = FALSE)
  if (!check$valid && error) stop(paste(check$errors, collapse = "\n"), call. = FALSE)
  if (!check$valid) return(check)
  out
}

#' Ler instrumento de arquivo JSON
#' @param caminho Caminho do arquivo.
#' @param error Interrompe em erro de leitura ou validação.
#' @return Instrumento como lista.
#' @export
schema_ler <- function(caminho, error = TRUE) {
  if (!file.exists(caminho)) {
    msg <- sprintf("Arquivo de esquema não encontrado: %s", caminho)
    if (error) stop(msg, call. = FALSE)
    return(list(valid = FALSE, errors = msg))
  }
  schema_de_json(paste(readLines(caminho, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), error = error)
}

#' Gravar instrumento JSON
#' @param schema Instrumento válido.
#' @param caminho Caminho de saída.
#' @param pretty JSON indentado.
#' @return Caminho, de forma invisível.
#' @export
schema_escrever <- function(schema, caminho, pretty = TRUE) {
  json <- schema_para_json(schema, pretty = pretty)
  dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)
  writeLines(json, caminho, useBytes = TRUE)
  invisible(caminho)
}
