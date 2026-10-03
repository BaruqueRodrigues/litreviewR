#' Abrir o editor local de instrumentos de revisão
#'
#' Abre uma interface Shiny local para criar dimensões de extração e
#' classificação, configurar categorias e regras declarativas, validar o
#' instrumento e importar ou exportar seu formato JSON. A edição e a prévia não
#' fazem chamadas a serviços de IA.
#'
#' @param schema Instrumento inicial. Por padrão, usa `schema_padrao()`.
#' @param launch Se `TRUE`, inicia a aplicação local; se `FALSE`, devolve o
#'   objeto de aplicação Shiny para composição ou teste.
#' @param ... Argumentos opcionais encaminhados a `shiny::runApp()`.
#' @return Se `launch = TRUE`, o resultado de `shiny::runApp()`; caso contrário,
#'   um objeto `shiny.appobj`.
#' @export
configura_revisao <- function(schema = NULL, launch = interactive(), ...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("A interface requer o pacote 'shiny'. Instale-o com install.packages('shiny').", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("A interface para JSON requer o pacote 'jsonlite'.", call. = FALSE)
  }
  if (is.null(schema)) schema <- schema_padrao()
  resultado <- schema_validar(schema)
  if (!isTRUE(resultado$valid)) stop(paste("Instrumento inválido:", paste(resultado$errors, collapse = "; ")), call. = FALSE)

  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$title("Configurar instrumento de revisão"),
      shiny::tags$style(shiny::HTML(".container-fluid { max-width: 1250px; } .btn { margin: 2px; } .alert { padding: 8px; }"))),
    shiny::titlePanel("Configurar instrumento de revisão"),
    shiny::p("Defina as dimensões da sua área de pesquisa. Você pode editar o esquema geral, importar um instrumento JSON ou começar do zero removendo as dimensões que não usa."),
    shiny::tabsetPanel(
      shiny::tabPanel("Instrumento",
        shiny::h4("Identificação"), mod_esquema_ui("meta"),
        shiny::h4("Dimensões"), mod_dimensoes_ui("dims"), mod_taxonomia_ui("taxonomy"),
        shiny::hr(), shiny::uiOutput("validation"), shiny::textOutput("save_status")
      ),
      shiny::tabPanel("Prévia", mod_previa_ui("preview")),
      shiny::tabPanel("Importar / exportar",
        shiny::fileInput("import_file", "Importar instrumento JSON", accept = ".json,application/json"),
        shiny::actionButton("import", "Carregar instrumento"),
        shiny::downloadButton("export", "Baixar JSON"),
        shiny::uiOutput("io_status")
      )
    )
  )
  server <- function(input, output, session) {
    current <- shiny::reactiveVal(schema)
    status <- shiny::reactiveVal("Alterações ainda não salvas em arquivo.")
    update_schema <- function(next_schema) {
      previous <- current()
      previous$revision <- NULL
      candidate <- next_schema
      candidate$revision <- NULL
      if (identical(previous, candidate)) return(invisible(NULL))
      next_schema$revision <- as.integer((current()$revision %||% 0L) + 1L)
      current(next_schema)
      status(paste0("Instrumento em edição · revisão ", next_schema$revision))
    }
    mod_esquema_server("meta", current, update_schema)
    mod_dimensoes_server("dims", shiny::reactive(current()$dimensions), function(ds) {
      s <- current(); s$dimensions <- ds; update_schema(s)
    })
    mod_taxonomia_server("taxonomy")
    mod_previa_server("preview", current)
    output$save_status <- shiny::renderText(status())
    validation <- shiny::reactive(schema_validar(current()))
    output$validation <- shiny::renderUI({
      v <- validation()
      if (isTRUE(v$valid)) shiny::div(class = "alert alert-success", "Instrumento válido para salvar.")
      else shiny::div(class = "alert alert-danger", paste(v$errors, collapse = " · "))
    })
    io_status <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$import, {
      req <- input$import_file
      if (is.null(req) || !file.exists(req$datapath)) { io_status("Selecione um arquivo JSON."); return() }
      imported <- tryCatch(schema_ler(req$datapath), error = function(e) e)
      if (inherits(imported, "error")) { io_status(paste("Não foi possível importar:", conditionMessage(imported))); return() }
      v <- schema_validar(imported)
      if (!isTRUE(v$valid)) { io_status(paste("Instrumento inválido:", paste(v$errors, collapse = "; "))); return() }
      current(imported); status(paste0("Instrumento importado · revisão ", imported$revision)); io_status("Importação concluída.")
    })
    output$export <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_-]+", "_", current()$title %||% "instrumento"), ".json"),
      content = function(file) {
        v <- schema_validar(current())
        if (!isTRUE(v$valid)) stop(paste(v$errors, collapse = "; "), call. = FALSE)
        schema_escrever(current(), file)
      }
    )
    output$io_status <- shiny::renderUI(if (!is.null(io_status())) shiny::div(class = "alert alert-info", io_status()))
  }
  app <- shiny::shinyApp(ui, server)
  if (isTRUE(launch)) shiny::runApp(app, ...)
  else app
}
