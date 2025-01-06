introduction_module_ui <- function(id){
  ns <- NS(id)
  introduction_tab <- tabItem(tabName = "Introduction",
                              fluidPage(
                                fluidRow(
                                  box(
                                    title = "Who is this app for?",
                                    id = "introduction_box1",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown(here("BlinkR_app", "markdown", "01_introduction/introduction_box1.Rmd"))
                                  ),
                                  box(
                                    title = "How to use this app",
                                    id = "introduction_box2",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown(here("BlinkR_app", "markdown","01_introduction/introduction_box2.Rmd"))
                                  ),
                                  box(
                                    title = "Outline of experiment",
                                    id = "introduction_box3",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown(here("BlinkR_app", "markdown","01_introduction/introduction_box3.Rmd"))
                                    
                                  )
                                ),
                                fluidRow(
                                  column(
                                  width = 12,
                                  div(
                                    style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
                                    actionButton(ns("next_page"), 
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
      
      observeEvent(input$next_page, {
      updateTabItems(parent.session, "sidebar", "Background")
    })
      
    }
  )
}