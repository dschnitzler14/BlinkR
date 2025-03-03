introduction_module_ui <- function(id){
  ns <- NS(id)
  introduction_tab <- tabItem(tabName = "Introduction",
                              fluidPage(
                                fluidRow(
                                  box(
                                    title = "Log in",
                                    id = "introduction_box1",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/01_introduction/introduction_box1.Rmd")
                                  ),
                
                                ),
                                fluidRow(
                                  column(
                                  width = 12,
                                  div(
                                    style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
                                    actionButton(ns("next_page_intro"), 
                                                 label = tagList("Next", icon("arrow-right")))
                                      )
                                    ),
                                  ),
                              )
  )
}

introduction_module_server <- function(id, parent.session){
  moduleServer(
    id,
    function(input, output, server){
      
      observeEvent(input$next_page_intro, {
      updateTabItems(parent.session, "sidebar", "Background")
    })
      
    }
  )
}