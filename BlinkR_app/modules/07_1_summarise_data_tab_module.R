analysis_summarise_data_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Summarise_Data",
    fluidPage(
      fluidRow(
        column(
          12,
          box(
            title = "Summarising The Data",
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            solidHeader = TRUE,
            fluidRow(
              column(
                4,
                markdown("In order to summarise the data, enter the code below into the code editor:"),
                wellPanel(
                  markdown(
                    "
                    ```
                    data_summary <- average_trs %>%
                      group_by(Stress_Status) %>%
                      summarise(
                        n = n(),
                        mean = mean(Average_Blinks_Per_Minute, na.rm = TRUE),
                        sd = sd(Average_Blinks_Per_Minute, na.rm = TRUE),
                        sem = sd / sqrt(n)
                      )
                    ```
                    "
                  )
                ),
                uiOutput(ns("summary_code_feedback"))
              
              ),
              column(8, editor_module_ui(ns("summarise_editor")),
                     uiOutput(ns("save_summary_result"))
              )
            )
          ),
        ),
        fluidRow(
          column(
            width = 12,
            div(
              style = "display: flex; justify-content: center; align-items: center; gap: 20px; height: 100px;",
              actionButton(
                ns("statistics"),
                label = tagList(icon("equals"), "Run Statistical Analysis"),
                class = "action-button custom-action",
                `data-id` = "stats"
              ),
              actionButton(
                ns("figure"),
                label = tagList(icon("chart-simple"), "Create a Figure"),
                class = "action-button custom-action",
                `data-id` = "create_figure"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              style = "display: flex; justify-content: center; align-items: center; height: 100px;",
              actionButton(
                ns("dashboard"),
                label = tagList(icon("dashboard"), "Go to Analysis Dashboard"),
                class = "action-button custom-dark-yellow"
              )
            )
          )
          
        ))))}


analysis_summarise_data_module_server <- function(id, results_data, parent.session, saved_results, session_folder_id) {
  moduleServer(id, function(input, output, session) {
    
    average_trs <- reactive({ NULL })
    
    average_trs_results <- results_data %>%
      select(-"Group", -"Initials", -"Submission_ID") %>%
      dplyr::group_by(ID, Stress_Status) %>%
      dplyr::summarise(
        Average_Blinks_Per_Minute = mean(Blinks_Per_Minute, na.rm = TRUE),
        .groups = 'drop'
      )
    
    average_trs <- reactive({ average_trs_results })
    
    data_summary <- average_trs_results %>%
      group_by(Stress_Status) %>%
      summarise(
        n = n(),
        mean = mean(Average_Blinks_Per_Minute, na.rm = TRUE),
        sd = sd(Average_Blinks_Per_Minute, na.rm = TRUE),
        sem = sd / sqrt(n)
      )
    
    
    predefined_code_summarise <- "data_summary <- average_trs %>%
      group_by(Stress_Status) %>%
      summarise(
        n = n(),
        mean = mean(Average_Blinks_Per_Minute, na.rm = TRUE),
        sd = sd(Average_Blinks_Per_Minute, na.rm = TRUE),
        sem = sd / sqrt(n)
      )"
    
    summarise_result <- editor_module_server("summarise_editor", data = average_trs, variable_name = "average_trs", 
                                             predefined_code = predefined_code_summarise, return_type = "result", 
                                             session_folder_id, save_header = "Summarise Result Code")
    
    observe({
      req(!is.null(summarise_result()), !is.null(summarise_result()$result))

      if (tibble::is_tibble(summarise_result()$result)) {
        output$summary_code_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
            markdown("
        Let's first take a look at the code. Just like in first step, we are using the `dplyr` recipe book.
        "),
          radioButtons(
            session$ns("summary_code_quiz"), 
            label = "Do you remember what `summarise` does?", 
            choices = list(
              "Creates a new dataset" = "option1", 
              "Creates new columns" = "option2", 
              "Uses meta-data to summarise" = "option3"
            ),
            selected = character(0)
          ),
          uiOutput(session$ns("summary_code_quiz_feedback")),
          markdown("Next let's take a look at the result."),
          numericInput(
            inputId = session$ns("mean_unstressed_group_quiz"),
            label = "What is the mean of the unstressed group?",
            value = 0,
            min = 5,
            max = 60
          ),
          actionButton(
            session$ns("submit_mean_unstressed_group_quiz_answer"),
            label = "Submit",
            class = "fun-submit-button"
          ),
          uiOutput(session$ns("mean_unstressed_group_quiz_feedback")),
          numericInput(
            inputId = session$ns("sem_stressed_group_quiz"),
            label = "What is the standard error of the mean (sem) of the stressed group?",
            value = 0,
            min = 5,
            max = 60
          ),
          actionButton(
            session$ns("submit_sem_stressed_group_quiz_answer"),
            label = "Submit",
            class = "fun-submit-button"
          ),
          uiOutput(session$ns("submit_sem_stressed_group_quiz_feedback")),
          radioButtons(
            session$ns("summary_result_interpretation_quiz"), 
            label = "Can we tell from this if this is statistically significant?", 
            choices = list(
              "Yes" = "option1", 
              "No" = "option2", 
              "I don't know" = "option3"
            ),
            selected = character(0)
          ),
          uiOutput(session$ns("summary_result_interpretation_quiz_feedback")),
          
          )
        })
        
        output$save_summary_result <- renderUI({
          actionButton(
            session$ns("save_summary_results_button"),
            label = tagList(icon("save"), "Save Results to Dashboard"),
            class = "action-button custom-action"
          )
        })
        
      } else {
        output$summary_code_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        
        output$save_summary_result <- renderUI({
          NULL
        })
      }
    })
    
    observeEvent(input$summary_code_quiz, {
      feedback <- if (input$summary_code_quiz == "option2") {
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
      
      output$summary_code_quiz_feedback <- renderUI({
        feedback
      })
    })
    
    unstressed_mean <- reactive({
      sr <- summarise_result()
      
      if (is.null(sr) || is.null(sr$result)) {
        return(NULL)
      }
      if (!tibble::is_tibble(sr$result)) {
        return(NULL)
      }
      if (!all(c("Stress_Status", "mean") %in% names(sr$result))) {
        return(NULL)
      }
      
      df_unstressed <- sr$result %>%
        dplyr::filter(Stress_Status == "Unstressed")
      
      if (nrow(df_unstressed) == 0) {
        return(NULL)
      }
      
      df_unstressed$mean[1]
    })


    unstressed_mean_round <- reactive({
    unstressed_mean_round_val <- unstressed_mean()
    if (is.null(unstressed_mean_round_val)) return(NULL)
    round(unstressed_mean_round_val, 2)
  })

 
    
observeEvent(input$submit_mean_unstressed_group_quiz_answer, {
  val <- unstressed_mean_round()

  if (is.null(val)) {
    output$mean_unstressed_group_quiz_feedback <- renderUI({
      div(class = "error-box", "\U1F914 We do not have a valid mean yet!")
    })
    return()
  }

  user_answer_mean_unstressed <- as.numeric(input$mean_unstressed_group_quiz)

  if (is.na(user_answer_mean_unstressed)) {
    feedback <- div(class = "error-box", "\U1F914 Please enter a numeric value!")
  } else {
    tolerance <- 0.5
    
    if (abs(user_answer_mean_unstressed - val) <= tolerance) {
      feedback <- div(class = "success-box", "\U1F64C Correct!")
    } else {
      feedback <- div(class = "error-box", "\U1F914 Not quite - try again!")
    }
  }

  output$mean_unstressed_group_quiz_feedback <- renderUI({
    feedback
  })
})

    #for stressed sem
    stressed_sem <- reactive({
  sr <- summarise_result()

  if (is.null(sr) || is.null(sr$result)) {
    return(NULL)
  }
  if (!tibble::is_tibble(sr$result)) {
    return(NULL)
  }
  if (!all(c("Stress_Status", "sem") %in% names(sr$result))) {
    return(NULL)
  }

  df_stressed <- sr$result %>%
    dplyr::filter(Stress_Status == "Stressed")

  if (nrow(df_stressed) == 0) {
    return(NULL)
  }

  df_stressed$sem[1]
})

stressed_sem_round <- reactive({
  val <- stressed_sem()
  if (is.null(val)) return(NULL)
  round(val, 2)
})

observeEvent(input$submit_sem_stressed_group_quiz_answer, {
  val <- stressed_sem_round()
  
  if (is.null(val)) {
    output$submit_sem_stressed_group_quiz_feedback <- renderUI({
      div(class = "error-box", "\U1F914 We do not have a valid SEM yet!")
    })
    return()
  }
  
  user_answer_sem_stressed <- as.numeric(input$sem_stressed_group_quiz)
  
  if (is.na(user_answer_sem_stressed)) {
    feedback <- div(class = "error-box", "\U1F914 Please enter a numeric value!")
  } else {
    tolerance <- 0.1
    
    if (abs(user_answer_sem_stressed - val) <= tolerance) {
      feedback <- div(class = "success-box", "\U1F64C Correct!")
    } else {
      feedback <- div(class = "error-box", "\U1F914 Not quite - try again!")
    }
  }
  
  output$submit_sem_stressed_group_quiz_feedback <- renderUI({
    feedback
  })
})

    
    #for significance radio quiz
    observeEvent(input$summary_result_interpretation_quiz, {
      feedback <- if (input$summary_result_interpretation_quiz == "option2") {
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
      
      output$summary_result_interpretation_quiz_feedback <- renderUI({
        feedback
      })
    })
    
    summary_updated <- reactiveVal(FALSE)
    
    observeEvent(input$save_summary_results_button, {
      if (!is.null(summarise_result())) {
        key <- "summary"
        saved_results$scripts[[key]] <- summarise_result()
        
        result_as_char <- capture.output(print(saved_results$scripts[[key]]))
        
        temp_file <- tempfile(fileext = ".txt")
        writeLines(result_as_char, con = temp_file)
        
        path <- drive_get(as_id(session_folder_id))
        
        drive_upload(
          media = temp_file,
          path = path,
          name = paste0(key, ".txt"),
          overwrite = TRUE,
        )
        
        unlink(temp_file)
        
        showNotification("Summary script saved successfully & Uploaded to Drive.", type = "message")
      } else {
        showNotification("No summary script to save.", type = "error")
      }
    })
    
    
    observeEvent(input$statistics, {
      updateTabItems(parent.session, "sidebar", "Statistical_Analysis")
    })
    observeEvent(input$figure, {
      updateTabItems(parent.session, "sidebar", "Create_Figure")
    })
    
    #home button
    observeEvent(input$dashboard, {
      updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
    }) 
    
  })
}
