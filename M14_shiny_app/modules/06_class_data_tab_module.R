# Class Data Module UI
class_data_module_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "Raw_Data",
    fluidPage(
      DT::dataTableOutput(ns("raw_data")) 
    )
  )
}

# Class Data Module Server
class_data_module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      dummy_data <- read.csv("data/dummy_blinking_data.csv")
      
      output$raw_data <- DT::renderDataTable({
        DT::datatable(dummy_data, options = list(paging = FALSE, searching = FALSE))
      })
    }
  )
}
