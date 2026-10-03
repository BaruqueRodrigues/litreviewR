mod_taxonomia_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Categorias e regras declarativas"),
    shiny::p("Configure categorias quando a dimensão usar categoria única ou múltiplas categorias. Regras de aplicabilidade aceitam apenas cláusulas declarativas do contrato; nenhum código é executado.")
  )
}

mod_taxonomia_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) invisible(NULL))
}
