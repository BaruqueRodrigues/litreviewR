#' @keywords internal
mod_dimensoes_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4,
        shiny::selectInput(ns("dimension_id"), "Dimensão", choices = NULL),
        shiny::actionButton(ns("add"), "Nova dimensão", class = "btn-primary"),
        shiny::actionButton(ns("up"), "↑", title = "Mover para cima"),
        shiny::actionButton(ns("down"), "↓", title = "Mover para baixo"),
        shiny::actionButton(ns("toggle"), "Ativar/desativar"),
        shiny::actionButton(ns("delete"), "Excluir", class = "btn-danger")
      ),
      shiny::column(8,
        shiny::textInput(ns("label"), "Nome"),
        shiny::textInput(ns("definition"), "Definição"),
        shiny::selectInput(ns("type"), "Tipo", choices = c(
          "Texto" = "text", "Número" = "number", "Data" = "date",
          "Categoria única" = "single_category", "Categorias múltiplas" = "multiple_categories",
          "Lista de itens" = "item_list"
        )),
        shiny::selectInput(ns("scope"), "Escopo", choices = c(
          "Artigo" = "article", "Estudo" = "study", "Passagem" = "passage"
        )),
        shiny::checkboxGroupInput(ns("operations"), "Operações", choices = c(
          "Extrair informações" = "extract", "Classificar" = "classify"
        )),
        shiny::textAreaInput(ns("instruction"), "Instrução para os agentes", rows = 3),
        shiny::textAreaInput(ns("search_terms"), "Termos de busca (um por linha)", rows = 2),
        shiny::textAreaInput(ns("include_examples"), "Exemplos de inclusão (um por linha)", rows = 2),
        shiny::textAreaInput(ns("exclude_examples"), "Exemplos de exclusão (um por linha)", rows = 2),
        shiny::textAreaInput(ns("categories"), "Categorias (uma por linha: id | nome | descrição)", rows = 4),
        shiny::textAreaInput(ns("scoring_rubric"), "Rubrica de escore (uma por linha: valor | rótulo | descrição)", rows = 3),
        shiny::textAreaInput(ns("applicability"), "Regra declarativa (JSON; cláusulas all/any)", rows = 3,
                             placeholder = '{"all":[{"dimension_id":"research_design","operator":"equals","value":"comparative"}],"any":[]}'),
        shiny::selectInput(ns("inference_policy"), "Inferência", choices = c(
          "Somente explícito" = "explicit_only", "Permitir inferência (identificada)" = "allow_inference"
        )),
        shiny::selectInput(ns("review_type"), "Encaminhar para revisão", choices = c(
          "Sem regra automática" = "none", "Sempre" = "always", "Se ambíguo" = "if_ambiguous",
          "Abaixo de limiar" = "below_threshold", "Categoria desconhecida" = "unknown_category"
        )),
        shiny::numericInput(ns("review_threshold"), "Limiar experimental (0 a 1)", value = NA, min = 0, max = 1, step = .01),
        shiny::checkboxInput(ns("review_experimental"), "Limiar ainda experimental", FALSE),
        shiny::checkboxInput(ns("evidence_required"), "Exigir passagem de evidência", TRUE),
        shiny::checkboxInput(ns("active"), "Dimensão ativa", TRUE),
        shiny::uiOutput(ns("errors"))
      )
    )
  )
}

#' @keywords internal
mod_dimensoes_server <- function(id, dimensoes, on_change = function(x) NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_id <- shiny::reactive(input$dimension_id)
    selected <- shiny::reactive({
      ds <- dimensoes()
      if (!length(ds)) return(NULL)
      i <- match(selected_id(), vapply(ds, function(x) x$id %||% "", character(1)))
      if (length(i) != 1L || is.na(i)) ds[[1]] else ds[[i]]
    })
    shiny::observe({
      ds <- dimensoes()
      choices <- stats::setNames(vapply(ds, function(x) paste0(if (identical(x$active, FALSE)) "[inativa] " else "", x$label %||% x$id), character(1)), vapply(ds, function(x) x$id, character(1)))
      current <- shiny::isolate(input$dimension_id)
      shiny::updateSelectInput(session, "dimension_id", choices = choices,
        selected = if (!is.null(current) && current %in% names(choices)) current else names(choices)[1])
    })
    shiny::observeEvent(selected_id(), {
      x <- selected(); if (is.null(x)) return()
      shiny::updateTextInput(session, "label", value = x$label %||% "")
      shiny::updateTextInput(session, "definition", value = x$definition %||% "")
      shiny::updateSelectInput(session, "type", selected = x$type %||% "text")
      shiny::updateSelectInput(session, "scope", selected = x$scope %||% "article")
      shiny::updateCheckboxGroupInput(session, "operations", selected = unlist(x$operations %||% "extract"))
      shiny::updateTextAreaInput(session, "instruction", value = x$instruction %||% "")
      shiny::updateTextAreaInput(session, "search_terms", value = paste(x$search_terms %||% character(), collapse = "\n"))
      shiny::updateTextAreaInput(session, "include_examples", value = paste(x$include_examples %||% character(), collapse = "\n"))
      shiny::updateTextAreaInput(session, "exclude_examples", value = paste(x$exclude_examples %||% character(), collapse = "\n"))
      shiny::updateTextAreaInput(session, "categories", value = categories_to_text(x$categories))
      shiny::updateTextAreaInput(session, "scoring_rubric", value = rubric_to_text(x$scoring_rubric))
      shiny::updateTextAreaInput(session, "applicability", value = jsonlite::toJSON(x$applicability %||% list(all = list(), any = list()), auto_unbox = TRUE, pretty = TRUE, null = "null"))
      shiny::updateSelectInput(session, "inference_policy", selected = x$inference_policy %||% "explicit_only")
      shiny::updateSelectInput(session, "review_type", selected = x$review_rule$type %||% "if_ambiguous")
      shiny::updateNumericInput(session, "review_threshold", value = x$review_rule$threshold %||% NA_real_)
      shiny::updateCheckboxInput(session, "review_experimental", value = isTRUE(x$review_rule$experimental))
      shiny::updateCheckboxInput(session, "evidence_required", value = !identical(x$evidence_required, FALSE))
      shiny::updateCheckboxInput(session, "active", value = !identical(x$active, FALSE))
    }, ignoreInit = TRUE)

    persist_editor <- function() {
      ds <- dimensoes(); i <- match(selected_id(), vapply(ds, function(x) x$id %||% "", character(1)))
      if (length(i) != 1L || is.na(i)) return(invisible(NULL))
      rule <- tryCatch(jsonlite::fromJSON(input$applicability %||% "{}", simplifyVector = FALSE), error = function(e) e)
      if (inherits(rule, "error")) return(invisible(NULL))
      ds[[i]]$label <- input$label %||% ""
      ds[[i]]$definition <- input$definition %||% ""
      ds[[i]]$type <- input$type %||% "text"
      ds[[i]]$scope <- input$scope %||% "article"
      ds[[i]]$operations <- as.list(input$operations %||% character())
      ds[[i]]$instruction <- input$instruction %||% ""
      ds[[i]]$search_terms <- lines_to_list(input$search_terms)
      ds[[i]]$include_examples <- lines_to_list(input$include_examples)
      ds[[i]]$exclude_examples <- lines_to_list(input$exclude_examples)
      ds[[i]]$categories <- parse_categories(input$categories)
      ds[[i]]$scoring_rubric <- parse_rubric(input$scoring_rubric)
      ds[[i]]$applicability <- rule
      ds[[i]]$inference_policy <- input$inference_policy %||% "explicit_only"
      threshold <- input$review_threshold
      review_type <- input$review_type %||% "if_ambiguous"
      ds[[i]]$review_rule <- list(type = review_type,
        threshold = if (review_type != "below_threshold" || is.null(threshold) || is.na(threshold)) NULL else threshold,
        experimental = isTRUE(input$review_experimental))
      ds[[i]]$evidence_required <- isTRUE(input$evidence_required)
      ds[[i]]$active <- isTRUE(input$active)
      on_change(ds)
      invisible(NULL)
    }
    fields <- c("label", "definition", "type", "scope", "operations", "instruction", "search_terms", "include_examples", "exclude_examples", "categories", "scoring_rubric", "applicability", "inference_policy", "review_type", "review_threshold", "review_experimental", "evidence_required", "active")
    for (field in fields) local({
      active_field <- field
      shiny::observeEvent(input[[active_field]], persist_editor(), ignoreInit = TRUE)
    })

    shiny::observeEvent(input$add, {
      ds <- dimensoes()
      id <- paste0("dimension_", format(Sys.time(), "%Y%m%d%H%M%OS6"), "_", sample.int(99999, 1))
      ds[[length(ds) + 1L]] <- list(id = id, label = "Nova dimensão", definition = "", active = TRUE,
        type = "text", scope = "article", operations = list("extract"), instruction = "",
        search_terms = list(), include_examples = list(), exclude_examples = list(), evidence_required = TRUE,
        categories = list(), scoring_rubric = list(), applicability = list(all = list(), any = list()),
        inference_policy = "explicit_only", review_rule = list(type = "if_ambiguous", threshold = NULL, experimental = FALSE))
      on_change(ds); shiny::updateSelectInput(session, "dimension_id", selected = id)
    })
    shiny::observeEvent(input$delete, {
      ds <- dimensoes(); i <- match(selected_id(), vapply(ds, function(x) x$id %||% "", character(1)))
      if (length(i) == 1L && !is.na(i)) on_change(ds[-i])
    })
    move <- function(delta) {
      ds <- dimensoes()
      moved <- reorder_dimensions(ds, selected_id(), delta)
      if (!identical(moved, ds)) on_change(moved)
    }
    shiny::observeEvent(input$up, move(-1L)); shiny::observeEvent(input$down, move(1L))
    shiny::observeEvent(input$toggle, {
      ds <- set_dimension_active(dimensoes(), selected_id(), NULL)
      if (!identical(ds, dimensoes())) on_change(ds)
    })
    output$errors <- shiny::renderUI({
      x <- selected(); if (is.null(x)) return(shiny::helpText("Adicione uma dimensão para começar."))
      issues <- character()
      if (!nzchar(trimws(input$label %||% ""))) issues <- c(issues, "Informe um nome.")
      if (input$type %in% c("single_category", "multiple_categories") && !length(parse_categories(input$categories))) issues <- c(issues, "Adicione ao menos uma categoria para este tipo.")
      if (nzchar(input$applicability %||% "")) tryCatch(jsonlite::fromJSON(input$applicability, simplifyVector = FALSE), error = function(e) issues <<- c(issues, "A regra precisa ser JSON válido."))
      if (length(issues)) shiny::div(class = "alert alert-danger", paste(issues, collapse = " ")) else shiny::div(class = "text-success", "Dimensão configurada.")
    })
    list(dimensoes = dimensoes, selecionada = selected_id)
  })
}

`%||%` <- function(x, y) if (is.null(x)) y else x
lines_to_list <- function(x) {
  lines <- trimws(strsplit(x %||% "", "\n", fixed = TRUE)[[1]])
  as.list(lines[nzchar(lines)])
}
parse_categories <- function(x) {
  lines <- Filter(nzchar, trimws(strsplit(x %||% "", "\n", fixed = TRUE)[[1]]))
  lapply(lines, function(line) {
    bits <- trimws(strsplit(line, "|", fixed = TRUE)[[1]])
    id <- if (length(bits) >= 2L) bits[1] else gsub("[^a-z0-9]+", "_", tolower(bits[1]))
    label <- if (length(bits) >= 2L) bits[2] else bits[1]
    list(id = id, label = label, description = if (length(bits) > 2L) paste(bits[-c(1, 2)], collapse = " | ") else "")
  })
}
categories_to_text <- function(x) if (!length(x)) "" else paste(vapply(x, function(cat) paste(cat$id %||% "", cat$label %||% "", cat$description %||% "", sep = " | "), character(1)), collapse = "\n")
parse_rubric <- function(x) {
  lines <- Filter(nzchar, trimws(strsplit(x %||% "", "\n", fixed = TRUE)[[1]]))
  lapply(lines, function(line) { b <- trimws(strsplit(line, "|", fixed = TRUE)[[1]]); list(value = if (is.numeric(suppressWarnings(as.numeric(b[1])))) as.numeric(b[1]) else b[1], label = if (length(b) > 1) b[2] else b[1], description = if (length(b) > 2) paste(b[-c(1, 2)], collapse = " | ") else "") })
}
rubric_to_text <- function(x) if (!length(x)) "" else paste(vapply(x, function(r) paste(r$value %||% "", r$label %||% "", r$description %||% "", sep = " | "), character(1)), collapse = "\n")

set_dimension_active <- function(dimensions, id, active = NULL) {
  i <- match(id, vapply(dimensions, function(x) x$id %||% "", character(1)))
  if (length(i) != 1L || is.na(i)) return(dimensions)
  if (is.null(active)) active <- !isTRUE(dimensions[[i]]$active)
  dimensions[[i]]$active <- isTRUE(active)
  dimensions
}

reorder_dimensions <- function(dimensions, id, offset) {
  i <- match(id, vapply(dimensions, function(x) x$id %||% "", character(1)))
  j <- i + offset
  if (length(i) != 1L || is.na(i) || j < 1L || j > length(dimensions)) return(dimensions)
  tmp <- dimensions[[i]]; dimensions[[i]] <- dimensions[[j]]; dimensions[[j]] <- tmp
  dimensions
}
