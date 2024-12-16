library(shiny)
library(shinydashboard)

# Numeric Input Module ----
measurement_input_module_ui <- function(id, label = "Label") {
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

measurement_input_module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      reactive({
        input$numeric_input1
      })
    }
  )
}

# Student Tab Module ----
# Student Tab Module ----
student_tabBox_module_UI <- function(id, student_number) {
  ns <- NS(id)
  tabBox(
    title = paste("Subject", student_number),
    id = ns("student_box"),
    width = 12,
    tabPanel(
      title = "Unstressed Condition",
      actionButton(ns("submit_button"), "Submit"),
      textOutput(ns("success_message"))
    )
  )
}

student_tabBox_module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$submit_button, {
        output$success_message <- renderText({
          "Success!"
        })
      })
    }
  )
}

# Measurements Module ----
measurements_module_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("students_ui"))
}

measurements_module_server <- function(id, student_data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observe({
        req(student_data())
        num_students <- nrow(student_data())
        
        # Dynamically render student modules
        output$students_ui <- renderUI({
          tagList(
            lapply(1:num_students, function(i) {
              student_tabBox_module_UI(
                id = paste0("student_module_", i),
                student_number = student_data()$Initials[i]
              )
            })
          )
        })
        
        # Attach server logic to each dynamically created module
        lapply(1:num_students, function(i) {
          student_tabBox_module_server(
            id = paste0("student_module_", i)
          )
        })
      })
    }
  )
}

# Main App UI ----
ui <- fluidPage(
  measurements_module_ui("measurements_module")
)

# Main App Server ----
server <- function(input, output, session) {
  # Sample student data
  student_data <- reactive({
    data.frame(
      ID = 1:2,
      Initials = c("A.B.", "C.D."),
      stringsAsFactors = FALSE
    )
  })
  
  # Connect measurements module
  measurements_module_server("measurements_module", student_data)
}

# Run App ----
shinyApp(ui, server)
