background_module_ui <- function(id){
  ns <- NS(id)
  background_tab <- tabItem(tabName = "background",
    fluidPage(
      fluidRow(
        column(12,
        box(title = "Start Your Background Reading",
            solidHeader = FALSE,
            collapsible = TRUE,
            width = 12,
            includeMarkdown(here("BlinkR_app", "markdown", "02_background", "background.Rmd"))
        ),
      tags$iframe(
        src = "https://pubmed.ncbi.nlm.nih.gov/",
        width = "100%",
        height = "800px",
        style = "border:none;"
      )
    )
  ),
    fluidRow(
      column(
      width = 12,
      div(
        style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
        actionButton(ns("back_page"),
                     label = tagList(icon("arrow-left"), "Back")),
        actionButton(ns("next_page"), 
              label = tagList("Next", icon("arrow-right")))
          )
        ),
      ),
    )
  )
}

background_module_server <- function(id, parent.session){
  moduleServer(
    id,
    function(input, output, server){
      
       observeEvent(input$back_page, {
      updateTabItems(parent.session, "sidebar", "Introduction")
    })
      observeEvent(input$next_page, {
      updateTabItems(parent.session, "sidebar", "Hypothesis")
    })
      
    }
  )
}