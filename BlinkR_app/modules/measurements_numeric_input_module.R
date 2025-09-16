measurement_input_module_ui <- function(id, i18n, student_name, student_ID, db_student_table){
  ns <- NS(id)
  vars <- get_experiment_vars()

  tagList(
    tabBox(
      title = paste(i18n$t("Student:"), student_name, " | ID: ", student_ID),
      width = 12,
      tabPanel(
        title = i18n$t("Consent"),
        fluidRow(
          column(12,
          markdown(i18n$t("You must consent in order to submit measurements.")),
          actionButton(
            inputId = ns("read_consent"),
            label = i18n$t("Read Consent Statement"),
            class = "fun-submit-button"
            ),
          checkboxInput(ns("consent_check"), strong(i18n$t("I have read and understood the consent agreement.")), value = FALSE),

          )
        )
      ),
      tabPanel(
        title = (sprintf(i18n$t("%s - Measurements"), vars$level_b_text_name)),
        fluidRow(
          column(4,
                 numericInput(
                   inputId = ns("level_b_input1"),
                   label = i18n$t("Technical Replicate 1"),
                   value = 0,
                   min = 0,
                   max = 100
                 )
          ),
          column(4,
                 numericInput(
                   inputId = ns("level_b_input2"),
                   label = i18n$t("Technical Replicate 2"),
                   value = 0,
                   min = 0,
                   max = 100
                 )
          ),
          column(4,
                 numericInput(
                   inputId = ns("level_b_input3"),
                   label = i18n$t("Technical Replicate 3"),
                   value = 0,
                   min = 0,
                   max = 100
                 )
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("consent_check"), "'] == true"),
          actionButton(ns("Submit_Level_B"), i18n$t("Submit Measurements"), class = "fun-submit-button")
        )
      ),
      tabPanel(
        title = (sprintf(i18n$t("%s - Measurements"), vars$level_a_text_name)),
        fluidRow(
          column(4,
                 numericInput(
                   inputId = ns("level_a_input1"),
                   label = i18n$t("Technical Replicate 1"),
                   value = 0,
                   min = 0,
                   max = 100
                 )
          ),
          column(4,
                 numericInput(
                   inputId = ns("level_a_input2"),
                   label = i18n$t("Technical Replicate 2"),
                   value = 0,
                   min = 0,
                   max = 100
                 )
          ),
          column(4,
                 numericInput(
                   inputId = ns("level_a_input3"),
                   label = i18n$t("Technical Replicate 3"),
                   value = 0,
                   min = 0,
                   max = 100
                 )
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("consent_check"), "'] == true"),
          actionButton(ns("Submit_Level_A"), i18n$t("Submit Measurements"), class = "fun-submit-button")
        )
      ),
    )
  )
}

measurement_input_module_server <- function(id, i18n, student_name, student_ID, group_name, submission_id, db_measurement, db_student_table, include_markdown_language) {
  moduleServer(
    id,
    function(input, output, session) {
            vars <- get_experiment_vars()

    iv_measurements_b <- InputValidator$new()
    iv_measurements_b$add_rule("level_b_input1", sv_numeric())
    iv_measurements_b$add_rule("level_b_input2", sv_numeric())
    iv_measurements_b$add_rule("level_b_input3", sv_numeric())

    iv_measurements_b$enable()

    iv_measurements_a <- InputValidator$new()
    iv_measurements_a$add_rule("level_a_input1", sv_numeric())
    iv_measurements_a$add_rule("level_a_input2", sv_numeric())
    iv_measurements_a$add_rule("level_a_input3", sv_numeric())
    
    iv_measurements_a$enable()

      ns <- session$ns
      
      observeEvent(input$read_consent, {
        showModal(modalDialog(
          title = i18n$t("Consent"),
          uiOutput(ns("consent_markdown")),
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "l" 
        ))
      })

    output$consent_markdown <- renderUI({
    include_markdown_language("05_measurement/consent.Rmd")
  })

      
      state <- reactiveValues(
        level_b_id = list(),
        level_a_id = list()
      )
      
      add_measurement <- function(level, inputs, submission_id) {
        
        if (any(sapply(inputs, is.null)) || any(sapply(inputs, is.na)) || any(sapply(inputs, function(x) x == 0))) {
          showNotification(i18n$t("Please enter all three measurements."), type = "error", duration = 3)
          return(FALSE)
        }
        
        existing_list <- if (level == vars$level_b_variable_name) state$level_b_id else state$level_a_id
        if (submission_id %in% existing_list) {
          showModal(modalDialog(
            title = i18n$t("Overwrite Confirmation"),
            paste(i18n$t("Data for"), level, i18n$t("measurements already exists. Do you want to overwrite it?")),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm_overwrite"), i18n$t("Overwrite"))
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
          technical_replicate = as.integer(1:length(inputs)),
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
        
        showNotification(i18n$t("Success: Measurements saved."), type = "message", duration = 3)
      }
      
      observeEvent(input$Submit_Level_B, {
      req(iv_measurements_b$is_valid())

      cat("Level B Inputs:", input$level_b_input1, input$level_b_input2, input$level_b_input3, "\n")

        inputs <- list(
          input$level_b_input1,
          input$level_b_input2,
          input$level_b_input3
        )
        add_measurement(vars$level_b_variable_name, inputs, submission_id)
      })
      
      observeEvent(input$Submit_Level_A, {
        req(iv_measurements_a$is_valid())

        cat("Level A Inputs:", input$level_a_input1, input$level_a_input2, input$level_a_input3, "\n")

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
