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

student_tabBox_module_server <- function(id, db, student_id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Initialize measurement input modules
      measurement_input_module_server(id = "unstressed_condition_module_1")
      measurement_input_module_server(id = "unstressed_condition_module_2")
      measurement_input_module_server(id = "unstressed_condition_module_3")
      measurement_input_module_server(id = "stressed_condition_module_1")
      measurement_input_module_server(id = "stressed_condition_module_2")
      measurement_input_module_server(id = "stressed_condition_module_3")
      
      # Submit unstressed measurements
      observeEvent(input$submit_unstressed, {
        cat("Unstressed Submit Button Clicked\n")  # Debugging log
        
        # Retrieve measurement values with individual prints
        unstressed_1 <- input[[ns("unstressed_condition_module_1-numeric_input1")]]
        unstressed_2 <- input[[ns("unstressed_condition_module_2-numeric_input1")]]
        unstressed_3 <- input[[ns("unstressed_condition_module_3-numeric_input1")]]
        
        cat("Unstressed values:\n", unstressed_1, unstressed_2, unstressed_3, "\n")  # Log input values
        
        # Ensure all values are valid before proceeding
        if (is.null(unstressed_1) || is.null(unstressed_2) || is.null(unstressed_3)) {
          cat("One or more unstressed values are NULL, aborting\n")
          return()  # Exit if any value is NULL
        }
        
        # Update the appropriate row in `db` for unstressed condition
        db_data <- db()
        
        # Check if the student ID exists in db before updating
        if (!student_id %in% db_data$ID) {
          cat("Student ID not found in database\n")
          return()
        }
        
        db_data[db_data$ID == student_id, c("Unstressed_1", "Unstressed_2", "Unstressed_3")] <- c(unstressed_1, unstressed_2, unstressed_3)
        db(db_data)  # Save changes to reactive `db`
        
        # Verify that the data updated in the reactiveVal
        cat("Database after unstressed update:\n")
        print(db())
        
        # Show success message
        output$Submission_unstressed_outcome <- renderText("Unstressed measurements submitted.")
        showNotification("Unstressed measurements submitted successfully.", type = "message")
      })
      
      # Submit stressed measurements
      observeEvent(input$submit_stressed, {
        cat("Stressed Submit Button Clicked\n")  # Debugging log
        
        # Retrieve measurement values with individual prints
        stressed_1 <- input[[ns("stressed_condition_module_1-numeric_input1")]]
        stressed_2 <- input[[ns("stressed_condition_module_2-numeric_input1")]]
        stressed_3 <- input[[ns("stressed_condition_module_3-numeric_input1")]]
        
        cat("Stressed values:\n", stressed_1, stressed_2, stressed_3, "\n")  # Log input values
        
        # Ensure all values are valid before proceeding
        if (is.null(stressed_1) || is.null(stressed_2) || is.null(stressed_3)) {
          cat("One or more stressed values are NULL, aborting\n")
          return()  # Exit if any value is NULL
        }
        
        # Update the appropriate row in `db` for stressed condition
        db_data <- db()
        
        # Check if the student ID exists in db before updating
        if (!student_id %in% db_data$ID) {
          cat("Student ID not found in database\n")
          return()
        }
        
        db_data[db_data$ID == student_id, c("Stressed_1", "Stressed_2", "Stressed_3")] <- c(stressed_1, stressed_2, stressed_3)
        db(db_data)  # Save changes to reactive `db`
        
        # Verify that the data updated in the reactiveVal
        cat("Database after stressed update:\n")
        print(db())
        
        # Show success message
        output$Submission_stressed_outcome <- renderText("Stressed measurements submitted.")
        showNotification("Stressed measurements submitted successfully.", type = "message")
      })
    }
  )
}




# main measurements module ----
measurements_module_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "Measurements",
    fluidPage(
      fluidRow(
        uiOutput(ns("students_ui"))  # Render student units dynamically
      )
    )
  )
}


measurements_module_server <- function(id, db) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Observe changes in the database and update the UI accordingly
      observe({
        req(db())  # Ensure db() is populated before using it
        num_students <- nrow(db())  # Get the number of students from the database
        
        # Render a unit for each student based on the database
        output$students_ui <- renderUI({
          student_ui_list <- lapply(1:num_students, function(i) {
            student_name <- db()$Initials[i]
            student_id <- db()$ID[i]
            
            # Generate a unique ID for each student's tab box module
            student_tabBox_module_UI(
              id = ns(paste0("student_module_", i)),
              student_number = student_name
            )
          })
          do.call(fluidRow, student_ui_list)
        })
        
        # Initialize each student's server logic, passing db and student ID
        lapply(1:num_students, function(i) {
          student_id <- db()$ID[i]
          student_tabBox_module_server(id = paste0(id, "-student_module_", i), db = db, student_id = student_id)
        })
      })
    }
  )
}


