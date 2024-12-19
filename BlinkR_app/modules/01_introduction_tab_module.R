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
                                )
                              )
  )
}

introduction_module_server <- function(id){
  moduleServer(
    id,
    function(input, output, server){
    }
  )
}