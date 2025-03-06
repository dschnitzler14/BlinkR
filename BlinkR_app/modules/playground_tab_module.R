playground_module_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = "Playground",
      fluidPage(
        fluidRow(
            column(
              width = 12,
              div(
                class = "page-title-box",
                tags$h2(
                  tagList(shiny::icon("hand"), "Playground")
                )
      )
    )),
        fluidRow(
          column(
      12,
      box(
        title = "Writing R Code",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        fluidRow(
          column(
                6,
          includeMarkdown("markdown/playground/playground_how_to_use.md"),
          ),
          column(
            6, 
          editor_module_ui(ns("playground1")),
              )
            )
          )
        )
      )
    )
  )
}


playground_module_server <- function(id, session_folder_id) {
  moduleServer(id, function(input, output, session) {
    
data <- iris

predefined_code_playground <- read_file(
      "markdown/playground/predefined_code_playground.txt"
    )
editor_module_server("playground1", data = data, variable_name = "data", predefined_code = predefined_code_playground, return_type = "result", session_folder_id, save_header = "Playground1")

  })
}