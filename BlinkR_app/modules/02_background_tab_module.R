background_module_ui <- function(id){
  ns <- NS(id)
  background_tab <- tabItem(
    tabName = "Background",
    fluidPage(
      fluidRow(
      tags$iframe(
        src = "https://pubmed.ncbi.nlm.nih.gov/",
        width = "100%",
        height = "800px",
        style = "border:none;"
      )
    )
    )
  )
}

background_module_server <- function(id){
  moduleServer(
    id,
    function(input, output, server){
    }
  )
}