measurement_input_module_ui <- function(id, student_number, student_ID) {
  ns <- NS(id)
  tagList(
    tabBox(
      title = paste("Student:", student_number),
      width = 12,
      tabPanel(
        title = "Unstressed Measurements",
        fluidRow(
          column(4,
                 numericInput(
                   inputId = ns("unstressed_input1"),
                   label = "Blinks/ Minute Technical Replicate 1",
                   value = 0,
                   min = 0,
                   max = 100
                 )
          ),
          column(4,
                 numericInput(
                   inputId = ns("unstressed_input2"),
                   label = "Blinks/ Minute Technical Replicate 2",
                   value = 0,
                   min = 0,
                   max = 100
                 )
          ),
          column(4,
                 numericInput(
                   inputId = ns("unstressed_input3"),
                   label = "Blinks/ Minute Technical Replicate 3",
                   value = 0,
                   min = 0,
                   max = 100
                 )
          )
        ),
        actionButton(ns("Submit_Unstressed"), "Submit Unstressed Measurements"),
        uiOutput(ns("Submit_Unstressed_Feedback"))
      ),
      tabPanel(
        title = "Stressed Measurements",
        fluidRow(
          column(4,
                 numericInput(
                   inputId = ns("stressed_input1"),
                   label = "Blinks/ Minute Technical Replicate 1",
                   value = 0,
                   min = 0,
                   max = 100
                 )
          ),
          column(4,
                 numericInput(
                   inputId = ns("stressed_input2"),
                   label = "Blinks/ Minute Technical Replicate 2",
                   value = 0,
                   min = 0,
                   max = 100
                 )
          ),
          column(4,
                 numericInput(
                   inputId = ns("stressed_input3"),
                   label = "Blinks/ Minute Technical Replicate 3",
                   value = 0,
                   min = 0,
                   max = 100
                 )
          )
        ),
        actionButton(ns("Submit_Stressed"), "Submit Stressed Measurements"),
        uiOutput(ns("Submit_Stressed_Feedback"))
      )
    )
  )
}

measurement_input_module_server <- function(id, student_number = student_name,
                                            student_ID = student_ID,
                                            group_name = group_name,
                                            db_measurement) {
  moduleServer(
    id,
    function(input, output, session) {

      add_measurement <- function(stress_status, inputs) {
        if (any(sapply(inputs, is.null)) || any(sapply(inputs, function(x) x == 0))) {
          return(FALSE)
        }
        
        new_data <- data.frame(
          Group = group_name,
          ID = student_number,
          Stress_Status = stress_status,
          Technical_Replicate = 1:length(inputs),
          Blinks_Per_Minute = unlist(inputs),
          stringsAsFactors = FALSE
        )
        
        db_measurement(rbind(db_measurement(), new_data))
        return(TRUE)
      }
      
      observeEvent(input$Submit_Unstressed, {
        inputs <- list(
          input$unstressed_input1,
          input$unstressed_input2,
          input$unstressed_input3
        )
        
        if (add_measurement("Unstressed", inputs)) {
          output$Submit_Unstressed_Feedback <- renderUI({
            div(style = "color: green;", "Success: Unstressed measurements saved.")
          })
        } else {
          output$Submit_Unstressed_Feedback <- renderUI({
            div(style = "color: red;", "Please enter all three measurements.")
          })
        }
      })
      
      observeEvent(input$Submit_Stressed, {
        inputs <- list(
          input$stressed_input1,
          input$stressed_input2,
          input$stressed_input3
        )
        
        if (add_measurement("Stressed", inputs)) {
          output$Submit_Stressed_Feedback <- renderUI({
            div(style = "color: green;", "Success: Stressed measurements saved.")
          })
        } else {
          output$Submit_Stressed_Feedback <- renderUI({
            div(style = "color: red;", "Please enter all three measurements.")
          })
        }
      })
    }
  )
}

#this works begin
# measurement_input_module_server <- function(id, db_measurement) {
#   moduleServer(
#     id,
#     function(input, output, session) {
# 
#       observeEvent(input$Submit_Unstressed, {
#         if (is.null(input$unstressed_input1) ||
#             is.null(input$unstressed_input2) ||
#             is.null(input$unstressed_input3) ||
#             input$unstressed_input1 == 0 ||
#             input$unstressed_input2 == 0 ||
#             input$unstressed_input3 == 0) {
#           output$Submit_Unstressed_Feedback <- renderUI({
#             div(style = "color: red;", "Please enter all three measurements.")
#           })
#         } else {
#           output$Submit_Unstressed_Feedback <- renderUI({
#             div(style = "color: green;", "Success")
#           })
#         }
#       })
# 
#       observeEvent(input$Submit_Stressed, {
#         if (is.null(input$stressed_input1) ||
#             is.null(input$stressed_input2) ||
#             is.null(input$stressed_input3) ||
#             input$stressed_input1 == 0 ||
#             input$stressed_input2 == 0 ||
#             input$stressed_input3 == 0) {
#           output$Submit_Stressed_Feedback <- renderUI({
#             div(style = "color: red;", "Please enter all three measurements.")
#           })
#         } else {
#           output$Submit_Stressed_Feedback <- renderUI({
#             div(style = "color: green;", "Success")
#           })
#         }
#       })
#     }
#   )
# }
# this works end
