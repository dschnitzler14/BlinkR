measurement_input_module_ui <- function(id, student_name, student_ID, db_student_table){
  ns <- NS(id)
  tagList(
    tabBox(
      title = paste("Student:", student_name, " | ID: ", student_ID),
      width = 12,
      tabPanel(
        title = "Consent",
        fluidRow(
          column(12,
          markdown("You must consent in order to submit measurements."),
          actionButton(
            inputId = ns("read_consent"),
            label = "Read Consent Statement",
            class = "fun-submit-button"
            ),
          checkboxInput(ns("consent_check"), strong("I have read and understood the consent agreement."), value = FALSE),

          )
        )
      ),
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
        #actionButton(ns("Submit_Unstressed"), "Submit Unstressed Measurements", class = "fun-submit-button"),
        conditionalPanel(
          condition = paste0("input['", ns("consent_check"), "'] == true"),
          actionButton(ns("Submit_Unstressed"), "Submit Unstressed Measurements", class = "fun-submit-button")
        )
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
        conditionalPanel(
          condition = paste0("input['", ns("consent_check"), "'] == true"),
          actionButton(ns("Submit_Stressed"), "Submit Stressed Measurements", class = "fun-submit-button")
        )
        #actionButton(ns("Submit_Stressed"), "Submit Stressed Measurements", class = "fun-submit-button"),
      ),
    )
  )
}

measurement_input_module_server <- function(id, student_name, student_ID, group_name, submission_ID, db_measurement, db_student_table) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      observeEvent(input$read_consent, {
        showModal(modalDialog(
          title = "Consent",
          includeMarkdown("markdown/05_measurement/consent.Rmd"),
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "l" 
        ))
      })
      
      state <- reactiveValues(
        unstressed_ids = list(),
        stressed_ids = list()
      )
      
      add_measurement <- function(stress_status, inputs, submission_ID) {
        
        if (any(sapply(inputs, is.null)) || any(sapply(inputs, function(x) x == 0))) {
          showNotification("Please enter all three measurements.", type = "error")
          return(FALSE)
        }
        
        existing_list <- if (stress_status == "Unstressed") state$unstressed_ids else state$stressed_ids
        if (submission_ID %in% existing_list) {
          showModal(modalDialog(
            title = "Overwrite Confirmation",
            paste("Data for", stress_status, "measurements already exists. Do you want to overwrite it?"),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm_overwrite"), "Overwrite")
            )
          ))
          
          observeEvent(input$confirm_overwrite, {
            removeModal()
            save_measurement(stress_status, inputs, submission_ID, overwrite = TRUE)
          }, once = TRUE, ignoreInit = TRUE)
          
          return(FALSE)
        }
        
        save_measurement(stress_status, inputs, submission_ID, overwrite = FALSE)
        
        return(TRUE)
      }
      
      save_measurement <- function(stress_status, inputs, submission_ID, overwrite = FALSE) {
        
        new_data <- data.frame(
          Group = as.character(group_name),
          Initials = as.character(student_name),
          ID = as.integer(student_ID),
          Stress_Status = as.character(stress_status),
          Technical_Replicate = as.integer(1:length(inputs)),
          Blinks_Per_Minute = as.integer(unlist(inputs)),
          Submission_ID = as.character(submission_ID),
          stringsAsFactors = FALSE
        )
        
        current_data <- db_measurement()
        
        if(overwrite) {
          current_data <- current_data[
            !(current_data$Submission_ID == submission_ID),
          ]
        }
        
        updated_data <- rbind(current_data, new_data)
        db_measurement(updated_data)
        
        
        if (stress_status == "Unstressed") {
          state$unstressed_ids <- unique(c(state$unstressed_ids, submission_ID))
        } else {
          state$stressed_ids <- unique(c(state$stressed_ids, submission_ID))
        }
        
        showNotification("Success: Measurements saved.", type = "message")
      }
      
      observeEvent(input$Submit_Unstressed, {
        inputs <- list(
          input$unstressed_input1,
          input$unstressed_input2,
          input$unstressed_input3
        )
        add_measurement("Unstressed", inputs, submission_ID)
      })
      
      observeEvent(input$Submit_Stressed, {
        inputs <- list(
          input$stressed_input1,
          input$stressed_input2,
          input$stressed_input3
        )
        add_measurement("Stressed", inputs, submission_ID)
      })
    }
  )
}
