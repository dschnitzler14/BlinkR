measurement_input_module_ui <- function(id, student_name, student_ID, db_student_table){
  ns <- NS(id)
  tagList(
    tabBox(
      title = paste("Student:", student_name, " | ", student_ID),
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

measurement_input_module_server <- function(id, student_name, student_ID, group_name, db_measurement, db_student_table) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      add_measurement <- function(stress_status, inputs) {
        if (any(sapply(inputs, is.null)) || any(sapply(inputs, function(x) x == 0))) {
          showNotification("Error: Some inputs are invalid.", type = "error")
          return(FALSE)
        }
        
        new_data <- data.frame(
          Group = group_name,
          ID = student_ID,
          Stress_Status = stress_status,
          Technical_Replicate = 1:length(inputs),
          Blinks_Per_Minute = unlist(inputs),
          stringsAsFactors = FALSE
        )
        
        current_data <- db_measurement()
        
        if (nrow(current_data) > 0 && any(duplicated(rbind(current_data, new_data)))) {
          showNotification("Warning: Duplicate data not added.", type = "warning")
          return(FALSE)
        }
        
        db_measurement(rbind(current_data, new_data))
        return(TRUE)
        
      }
      
      observeEvent(input$Submit_Unstressed, {
        inputs <- list(
          input$unstressed_input1,
          input$unstressed_input2,
          input$unstressed_input3
        )
        
        if (add_measurement("Unstressed", inputs)) {
          showNotification("Success: Unstressed measurements saved.", type= "message")
          
      } else {
          showNotification("Please enter all three unstressed measurements.", type= "error")
      }
    })

      observeEvent(input$Submit_Stressed, {
        inputs <- list(
          input$stressed_input1,
          input$stressed_input2,
          input$stressed_input3
        )

        if (add_measurement("Stressed", inputs)) {
          showNotification("Success: Stressed measurements saved.", type= "message")
          
        } else {
          showNotification("Please enter all three stressed measurements.", type= "error")
          
        }
      })
    }
  )
}

