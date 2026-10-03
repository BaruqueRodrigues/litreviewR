mod_esquema_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textInput(ns("title"), "Nome do instrumento"),
    shiny::textInput(ns("area"), "Área de pesquisa"),
    shiny::textInput(ns("language"), "Idioma das instruções", value = "pt-BR")
  )
}

mod_esquema_server <- function(id, initial, on_change) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(initial(), {
      s <- initial()
      shiny::updateTextInput(session, "title", value = s$title %||% "")
      shiny::updateTextInput(session, "area", value = s$area %||% "")
      shiny::updateTextInput(session, "language", value = s$instruction_language %||% "pt-BR")
    }, ignoreInit = FALSE)
    for (field in c("title", "area", "language")) local({
      active_field <- field
      shiny::observeEvent(input[[active_field]], {
        s <- initial()
        if (is.null(s)) return()
        if (active_field == "title") s$title <- input[[active_field]] %||% ""
        if (active_field == "area") s$area <- input[[active_field]] %||% ""
        if (active_field == "language") s$instruction_language <- input[[active_field]] %||% "pt-BR"
        on_change(s)
      }, ignoreInit = TRUE)
    })
    invisible(NULL)
  })
}
