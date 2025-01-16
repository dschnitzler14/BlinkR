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
            includeMarkdown("markdown/02_background/background.Rmd")
        )
    )
  ),
    fluidRow(
      column(
      width = 12,
      div(
        style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
        actionButton(ns("back_page_background"),
                     label = tagList(icon("arrow-left"), "Back")),
        actionButton(ns("next_page_background"), 
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
      
       observeEvent(input$back_page_background, {
      updateTabItems(parent.session, "sidebar", "Introduction")
    })
      observeEvent(input$next_page_background, {
      updateTabItems(parent.session, "sidebar", "Hypothesis")
    })
      
    }
  )
}