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
      ),
fluidRow(
  column(
    width = 12,
    div(
      style = "
        display: flex; 
        justify-content: center; 
        align-items: center; 
        gap: 10px;          
        margin: 0; 
        padding: 10px;
      ",
      actionButton(
        ns("back_page_playground"),
        label = tagList(icon("arrow-left"), " Back"),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_playground"), 
        label = tagList("Next ", icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
    )
  )
}


playground_module_server <- function(id, session_folder_id, parent.session) {
  moduleServer(id, function(input, output, session) {
          vars <- get_experiment_vars()

    
data <- iris

predefined_code_playground <- read_file(
      "markdown/playground/predefined_code_playground.txt"
    )
editor_module_server("playground1", data = data, variable_name = "data", predefined_code = predefined_code_playground, return_type = "result", session_folder_id, save_header = "Playground1")


observeEvent(input$back_page_playground, {
        updateTabItems(parent.session, "sidebar", "Raw_Data")
      })
      observeEvent(input$next_page_playground, {
        updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
      })

  })
}