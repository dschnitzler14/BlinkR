write_up_module_ui <- function(id) {
  ns <- NS(id)
  writing_up_tab <- tabItem(tabName = "Writing-Up",
                            fluidPage(
                              fluidRow(text_area_module_UI(ns("write_up1"))

                                
                              )
                            )
  )
}

write_up_module_server <- function(id){
  moduleServer(
    id,
    function(input, output, server){
      text_area_module_server("write_up1")
    }
  )
}
