md_input_ui <- function(id, label) {
  ns <- NS(id)
  tagList(
    textAreaInput(ns("markdown"), label, "", rows = 10, width = "100%")
  )
}

md_input_server <- function(id){
  moduleServer(
    id,
    function(input, output, server){
 
        reactive({
        input$markdown
      })
      

    }
  )
}