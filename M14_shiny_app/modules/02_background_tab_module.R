background_module_ui <- function(id){
  ns <- NS(id)
  background_tab <- tabItem(
    tabName = "Background",
    fluidPage(
      fluidRow(
      # Embed the PubMed website using an iframe
      tags$iframe(
        src = "https://pubmed.ncbi.nlm.nih.gov/",
        width = "100%",       # Full width of the tab
        height = "800px",     # Set height as needed
        style = "border:none;" # Remove border for a clean look
      )
    )
    )
  )
}

background_module_server <- function(id){
  moduleServer(
    id,
    function(input, output, server){
      #function logic here
    }
  )
}