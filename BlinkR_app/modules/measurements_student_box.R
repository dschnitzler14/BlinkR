student_tabBox_module_UI <- function(id, student_number) {
  ns <- NS(id)
  tabBox(
    title = paste("Student:", student_number),
    width = 12,
    tabPanel("Measurements", measurement_input_module_ui(ns("measurement_input")))
  )
}

student_tabBox_module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      measurement_input_module_server(session$ns("measurement_input"))
    }
  )
}
