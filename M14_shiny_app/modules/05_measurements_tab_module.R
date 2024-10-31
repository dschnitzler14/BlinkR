# numeric input module ----
measurement_input_module_ui <- function(id, label = "Label"){
  ns <- NS(id)
  tagList(
    numericInput(
      inputId = ns("numeric_input1"),
      label = label,
      value = 0,
      min = 5,
      max = 60
    )
  )
}

measurement_input_module_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      # Placeholder for future functionality if needed
    }
  )
}


# student tab box module ----
student_tabBox_module_UI <- function(id, student_number) {
  ns <- NS(id)
  tabBox(
    title = paste("Subject", student_number),
    id = ns("student_box"),
    width = 12,
    side = "left",
    
    tabPanel(
      title = "Unstressed Condition",
      fluidRow(
        column(width = 4, measurement_input_module_ui(id = ns("unstressed_condition_module_1"), label = "Blinks/ 1 Minute: Technical Replicate 1")),
        column(width = 4, measurement_input_module_ui(id = ns("unstressed_condition_module_2"), label = "Blinks/ 1 Minute: Technical Replicate 2")),
        column(width = 4, measurement_input_module_ui(id = ns("unstressed_condition_module_3"), label = "Blinks/ 1 Minute: Technical Replicate 3"))
      ),
      actionButton(ns("submit_unstressed"), "Submit Unstressed Measurements"),
      textOutput(ns("Submission_unstressed_outcome"))
    ),
    
    tabPanel(
      title = "Stressed Condition",
      fluidRow(
        column(width = 4, measurement_input_module_ui(id = ns("stressed_condition_module_1"), label = "Blinks/ 1 Minute: Technical Replicate 1")),
        column(width = 4, measurement_input_module_ui(id = ns("stressed_condition_module_2"), label = "Blinks/ 1 Minute: Technical Replicate 2")),
        column(width = 4, measurement_input_module_ui(id = ns("stressed_condition_module_3"), label = "Blinks/ 1 Minute: Technical Replicate 3"))
      ),
      actionButton(ns("submit_stressed"), "Submit Stressed Measurements"),
      textOutput(ns("Submission_stressed_outcome"))
    )
  )
}

student_tabBox_module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      measurement_input_module_server(id = "unstressed_condition_module_1")
      measurement_input_module_server(id = "unstressed_condition_module_2")
      measurement_input_module_server(id = "unstressed_condition_module_3")
      
      measurement_input_module_server(id = "stressed_condition_module_1")
      measurement_input_module_server(id = "stressed_condition_module_2")
      measurement_input_module_server(id = "stressed_condition_module_3")
      
      observeEvent(input$submit_unstressed, {
        output$Submission_unstressed_outcome <- renderText({
          paste("Submitted")
        })
      })
      
      observeEvent(input$submit_stressed, {
        output$Submission_stressed_outcome <- renderText({
          "Submitted"
        })
      })
    }
  )
}

# main measurements module ----
measurements_module_ui <- function(id, add_student_button_label = "Add Student"){
  ns <- NS(id)
  tabItem(
    tabName = "Measurements",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          actionButton(ns("add_student_button"), label = add_student_button_label),
          br(),
          br()
        )
      ),
      fluidRow(
        uiOutput(ns("students_ui"))
      )
    )
  )
}

measurements_module_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      student_counter <- reactiveVal(1)
      
      observeEvent(input$add_student_button, {
        student_counter(student_counter() + 1)
      })
      
      output$students_ui <- renderUI({
        student_ui_list <- lapply(1:student_counter(), function(i) {
          student_tabBox_module_UI(id = ns(paste0("student_module_", i)), student_number = i)
        })
        do.call(fluidRow, student_ui_list)
      })
      
      observe({
        lapply(1:student_counter(), function(i) {
          student_tabBox_module_server(id = paste0(id, "-student_module_", i))
        })
      })
    }
  )
}
