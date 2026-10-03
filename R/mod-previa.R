build_schema_preview <- function(schema) {
  dims <- Filter(function(d) isTRUE(d$active), schema$dimensions %||% list())
  if (!length(dims)) return("Nenhuma dimensão ativa.")
  paste(vapply(dims, function(d) {
    category_text <- if (length(d$categories)) paste(vapply(d$categories, function(cat) paste0(cat$label, " — ", cat$description), character(1)), collapse = "; ") else "(sem categorias fechadas)"
    applicability <- d$applicability %||% list(all = list(), any = list())
    rule_text <- if (length(applicability$all) || length(applicability$any)) "aplicável conforme regra declarativa" else "sem condição de aplicabilidade"
    paste0("• ", d$label, " [", d$scope, "; ", d$type, "; ", paste(unlist(d$operations), collapse = "+"), "]\n",
      "  Objetivo: ", if (nzchar(d$definition %||% "")) d$definition else "(defina o objetivo)", "\n",
      "  Instrução: ", if (nzchar(d$instruction %||% "")) d$instruction else "(defina a instrução)", "\n",
      "  Categorias: ", category_text, "\n",
      "  Aplicabilidade: ", rule_text, "\n",
      "  Evidência: ", if (isTRUE(d$evidence_required)) "obrigatória" else "opcional",
      "; inferência: ", d$inference_policy %||% "explicit_only")
  }, character(1)), collapse = "\n\n")
}

mod_previa_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::h4("Prévia das instruções"), shiny::verbatimTextOutput(ns("preview")))
}

mod_previa_server <- function(id, schema) {
  shiny::moduleServer(id, function(input, output, session) {
    output$preview <- shiny::renderText(build_schema_preview(schema()))
  })
}
