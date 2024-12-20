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
                            )
                          )
  )
}

protocol_module_server <- function(id){
  moduleServer(
    id,
    function(input, output, server){
    }
  )
}