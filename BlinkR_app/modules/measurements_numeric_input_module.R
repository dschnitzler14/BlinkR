measurement_input_module_ui <- function(id, student_name, student_ID, db_student_table){
  ns <- NS(id)
  vars <- get_experiment_vars()

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
        title = (sprintf("%s - Measurements", vars$level_b_text_name)),
        fluidRow(
          column(4,
                 numericInput(
                   inputId = ns("level_b_input1"),
                   label = "Technical Replicate 1",
                   value = 0,
                   min = 0,
                   max = 100
                 )
          ),
          column(4,
                 numericInput(
                   inputId = ns("level_b_input2"),
                   label = "Technical Replicate 2",
                   value = 0,
                   min = 0,
                   max = 100
                 )
          ),
          column(4,
                 numericInput(
                   inputId = ns("level_b_input3"),
                   label = "Technical Replicate 3",
                   value = 0,
                   min = 0,
                   max = 100
                 )
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("consent_check"), "'] == true"),
          actionButton(ns("Submit_Level_B"), "Submit Measurements", class = "fun-submit-button")
        )
      ),
      tabPanel(
        title = (sprintf("%s - Measurements", vars$level_a_text_name)),
        fluidRow(
          column(4,
                 numericInput(
                   inputId = ns("level_a_input1"),
                   label = "Technical Replicate 1",
                   value = 0,
                   min = 0,
                   max = 100
                 )
          ),
          column(4,
                 numericInput(
                   inputId = ns("level_a_input2"),
                   label = "Technical Replicate 2",
                   value = 0,
                   min = 0,
                   max = 100
                 )
          ),
          column(4,
                 numericInput(
                   inputId = ns("level_a_input3"),
                   label = "Technical Replicate 3",
                   value = 0,
                   min = 0,
                   max = 100
                 )
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("consent_check"), "'] == true"),
          actionButton(ns("Submit_Level_A"), "Submit Measurements", class = "fun-submit-button")
        )
      ),
    )
  )
}

measurement_input_module_server <- function(id, student_name, student_ID, group_name, submission_id, db_measurement, db_student_table) {
  moduleServer(
    id,
    function(input, output, session) {
            vars <- get_experiment_vars()

      
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
        level_b_id = list(),
        level_a_id = list()
      )
      
      add_measurement <- function(level, inputs, submission_id) {
        
        if (any(sapply(inputs, is.null)) || any(sapply(inputs, function(x) x == 0))) {
          showNotification("Please enter all three measurements.", type = "error", duration = 3)
          return(FALSE)
        }
        
        existing_list <- if (level == vars$level_b_variable_name) state$level_b_id else state$level_a_id
        if (submission_id %in% existing_list) {
          showModal(modalDialog(
            title = "Overwrite Confirmation",
            paste("Data for", level, "measurements already exists. Do you want to overwrite it?"),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm_overwrite"), "Overwrite")
            )
          ))
          
          observeEvent(input$confirm_overwrite, {
            removeModal()
            save_measurement(level, inputs, submission_id, overwrite = TRUE)
          }, once = TRUE, ignoreInit = TRUE)
          
          return(FALSE)
        }
        
        save_measurement(level, inputs, submission_id, overwrite = FALSE)
        
        return(TRUE)
      }
      
      save_measurement <- function(level, inputs, submission_id, overwrite = FALSE) {
        
        new_data <- data.frame(
          group = as.character(group_name),
          initials = as.character(student_name),
          id = as.integer(student_ID),
          #vars$levels_variable_name = as.character(level),
          technical_replicate = as.integer(1:length(inputs)),
          #vars$measurement_variable_name = as.integer(unlist(inputs)),
          submission_id = as.character(submission_id),
          stringsAsFactors = FALSE
        )
        new_data[[vars$levels_variable_name]] <- as.character(level)
        new_data[[vars$measurement_variable_name]] <- as.integer(unlist(inputs))

        current_data <- db_measurement()
        
        if(overwrite) {
          current_data <- current_data[
            !(current_data$submission_id == submission_id),
          ]
        }
        
        updated_data <- rbind(current_data, new_data)
        db_measurement(updated_data)
        
        
        if (level == vars$level_b_variable_name) {
          state$level_b_id <- unique(c(state$level_b_id, submission_id))
        } else {
          state$level_a_id <- unique(c(state$level_a_id, submission_id))
        }
        
        showNotification("Success: Measurements saved.", type = "message", duration = 3)
      }
      
      observeEvent(input$Submit_Level_B, {
        inputs <- list(
          input$level_b_input1,
          input$level_b_input2,
          input$level_b_input3
        )
        add_measurement(vars$level_b_variable_name, inputs, submission_id)
      })
      
      observeEvent(input$Submit_Level_A, {
        inputs <- list(
          input$level_a_input1,
          input$level_a_input2,
          input$level_a_input3
        )
        add_measurement(vars$level_a_variable_name, inputs, submission_id)
      })
    }
  )
}
