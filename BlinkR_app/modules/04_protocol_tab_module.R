protocol_module_ui <- function(id){
  ns <- NS(id)
  protocol_tab <- tabItem(tabName = "Protocol",
                          fluidPage(
                            fluidRow(
                              box(
                                title = "Protocol",
                                id = "protocol_",
                                collapsible = FALSE,
                                width = 12,
                                solidHeader = TRUE,
                                includeMarkdown(here("BlinkR_app", "markdown","04_protocol/protocol.Rmd"))
                              ),
                              box(
                                title = "What is you Protocol?",
                                id = "protocol_preparation",
                                collapsible = FALSE,
                                width = 12,
                                solidHeader = TRUE,
                                text_area_module_UI(ns("protocol")),
                              ),
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

protocol_module_server <- function(id, auth, parent.session){
  moduleServer(
    id,
    function(input, output, server){
      
      text_area_module_server("protocol", auth, "Protocol")
      
       observeEvent(input$back_page, {
      updateTabItems(parent.session, "sidebar", "Hypothesis")
    })
      observeEvent(input$next_page, {
      updateTabItems(parent.session, "sidebar", "Measurements")
    })

    }
  )
}