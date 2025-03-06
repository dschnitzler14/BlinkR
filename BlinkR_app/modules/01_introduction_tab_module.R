introduction_module_ui <- function(id){
  ns <- NS(id)
  introduction_tab <- tabItem(tabName = "Introduction",
                              fluidPage(
                                  fluidRow(
                                      column(
                                        width = 12,
                                        div(
                                          class = "page-title-box",
                                          tags$h2(
                                            tagList(shiny::icon("sun"), "Introduction")
                                          )
                                )
                              )),
                                fluidRow(
                                  box(
                                    title = tagList(icon("right-to-bracket"), "Log in"),
                                    id = "introduction_box1",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/01_introduction/introduction_box1.Rmd")
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