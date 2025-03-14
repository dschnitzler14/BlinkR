analysis_summarise_data_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Summarise_Data",
    fluidPage(
       fluidRow(
            column(
              width = 12,
              div(
                class = "page-title-box",
                tags$h2(
                  tagList(shiny::icon("rectangle-list"), "Analysis: Summarise Data")
                )
      )
    )),
      fluidRow(
        useShinyjs(),
        uiOutput(ns("step1_box")),
        uiOutput(ns("step2_box")),
        uiOutput(ns("step3_box")),
        uiOutput(ns("step4_box")),
        uiOutput(ns("step5_box")),
        uiOutput(ns("step6_box")),
        uiOutput(ns("step7_box")),
        uiOutput(ns("step8_box")),
        uiOutput(ns("step9_box"))
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
          
        ),
        fluidRow(
  column(
    width = 12,
    div(
      style = "
        display: flex; 
        justify-content: center; 
        align-items: center; 
        gap: 10px;          
        margin: 0; 
        padding: 10px;
      ",
      actionButton(
        ns("back_page_summarise"),
        label = tagList(icon("arrow-left"), " Back"),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_summarise"), 
        label = tagList("Next ", icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
))}


analysis_summarise_data_module_server <- function(id, results_data, parent.session, saved_results, session_folder_id) {
  moduleServer(id, function(input, output, session) {
  
  #step1 data prep
    average_trs <- reactive({ NULL })
    
    average_trs_results <- results_data %>%
      select(-"Group", -"Initials", -"Submission_ID") %>%
      dplyr::group_by(ID, Stress_Status) %>%
      dplyr::summarise(
        Average_Blinks_Per_Minute = mean(Blinks_Per_Minute, na.rm = TRUE),
        .groups = 'drop'
      )
    
    average_trs <- reactive({ average_trs_results })
  
  #step1 calculation
    unstressed_data <- reactive({ NULL })

    unstressed_data_result <- average_trs() %>%
      filter(Stress_Status == "Unstressed")
    
    unstressed_data <- reactive({ unstressed_data_result })

  #step2 calculation
    unstressed_mean <- reactive({ NULL })

    unstressed_mean_result <- mean(unstressed_data()$Average_Blinks_Per_Minute, na.rm = TRUE) 
    
    unstressed_mean <- reactive({ unstressed_mean_result })

  #step3 calculation

    unstressed_sd <- reactive({ NULL })

    unstressed_sd_result <- sd(unstressed_data()$Average_Blinks_Per_Minute, na.rm = TRUE)

    unstressed_sd <- reactive({ unstressed_sd_result })

  #step4 calculation
  
    unstressed_n <- reactive({ NULL })

    unstressed_n_result <- nrow(unstressed_data())

    unstressed_n <- reactive({ unstressed_n_result })

    unstressed_se <- reactive({ NULL })

    unstressed_se_result <- unstressed_sd() / sqrt(unstressed_n())

    unstressed_se <- reactive({ unstressed_se_result })

## stressed reactive values
#step1 calculation
    stressed_data <- reactive({ NULL })

    stressed_data_result <- average_trs() %>%
      filter(Stress_Status == "Stressed")
    
    stressed_data <- reactive({ stressed_data_result })

  #step2 calculation
    stressed_mean <- reactive({ NULL })

    stressed_mean_result <- mean(stressed_data()$Average_Blinks_Per_Minute, na.rm = TRUE) 
    
    stressed_mean <- reactive({ stressed_mean_result })

  #step3 calculation

    stressed_sd <- reactive({ NULL })

    stressed_sd_result <- sd(stressed_data()$Average_Blinks_Per_Minute, na.rm = TRUE)

    stressed_sd <- reactive({ stressed_sd_result })

  #step4 calculation
  
    stressed_n <- reactive({ NULL })

    stressed_n_result <- nrow(stressed_data())

    stressed_n <- reactive({ stressed_n_result })

    stressed_se <- reactive({ NULL })

    stressed_se_result <- stressed_sd() / sqrt(stressed_n())

    stressed_se <- reactive({ stressed_se_result })

# step1: filter to unstressed

  predefined_code_step1 = read_file("markdown/07_analysis/predefined_code_summarise_filter_unstressed.txt")
  summarise_result_step1 <- editor_module_server("step1_editor", average_trs, "average_trs", predefined_code = predefined_code_step1, return_type = "result", session_folder_id, save_header = "Step 1: Summarise Data")

  output$step1_box <- renderUI({
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step1_box",
              title = "1️⃣ Filter to Unstressed Data",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_summarise_data_filter_unstressed.Rmd"),
                  uiOutput(session$ns("summary_code_feedback_step1"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step1_editor"))
                  )
              ),
              
            )
          ),
          
        ),
        
      )
    })


  observe({
      req(!is.null(summarise_result_step1()), !is.null(summarise_result_step1()$result))

      if (tibble::is_tibble(summarise_result_step1()$result)) {
        output$summary_code_feedback_step1 <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
          )
        })
        } else {
        output$summary_code_feedback_step1 <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
      })


# step2: mean of unstressed

  predefined_code_step2 = read_file("markdown/07_analysis/predefined_code_calculate_mean_unstressed.txt")
  summarise_result_step2 <- editor_module_server("step2_editor", unstressed_data, "unstressed_data", predefined_code = predefined_code_step2, return_type = "result", session_folder_id, save_header = "Step 2: Summarise Data")

output$step2_box <- renderUI({
    req(
    !is.null(summarise_result_step1()),
    tibble::is_tibble(summarise_result_step1()$result)
    )
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step2_box",
              title = "2️⃣ Calculate the Mean of Unstressed Data",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_summarise_data_mean_unstressed.Rmd"),
                  uiOutput(session$ns("summary_code_feedback_step2"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step2_editor"))
                  )
              )
            )
          ),
          
        )
      )
    })

observe({
  req(!is.null(summarise_result_step2()), !is.null(summarise_result_step2()$result))
  
  if ( is.numeric(summarise_result_step2()$result) &&
       length(summarise_result_step2()$result) == 1 ) {
    
    output$summary_code_feedback_step2 <- renderUI({
      tagList(
        div(class = "success-box", "\U1F64C Great!")
      )
    })
    
  } else {
    
    output$summary_code_feedback_step2 <- renderUI({
      div(class = "error-box", "\U1F914 Not quite - try again!")
    })
    
  }
})


# step3: sd of unstressed

  predefined_code_step3 = read_file("markdown/07_analysis/predefined_code_calculate_sd_unstressed.txt")
  summarise_result_step3 <- editor_module_server("step3_editor", unstressed_data, "unstressed_data", predefined_code = predefined_code_step3, return_type = "result", session_folder_id, save_header = "Step 2: Summarise Data")

output$step3_box <- renderUI({
      req(!is.null(summarise_result_step2()), !is.null(summarise_result_step2()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step3_box",
              title = "3️⃣ Calculate the Standard Deviation of Unstressed Data",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_summarise_data_sd_unstressed.Rmd"),
                  uiOutput(session$ns("summary_code_feedback_step3"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step3_editor"))
                  )
              )
            )
          ),
          
        )
      )
    })

observe({
  req(!is.null(summarise_result_step3()), !is.null(summarise_result_step3()$result))
  
  if ( is.numeric(summarise_result_step3()$result) &&
       length(summarise_result_step3()$result) == 1 ) {
    
    output$summary_code_feedback_step3 <- renderUI({
      tagList(
        div(class = "success-box", "\U1F64C Great!")
      )
    })
    
  } else {
    
    output$summary_code_feedback_step3 <- renderUI({
      div(class = "error-box", "\U1F914 Not quite - try again!")
    })
    
  }
})

# step4: n and sem of unstressed

predefined_code_step4 = read_file("markdown/07_analysis/predefined_code_calculate_sem_unstressed.txt")
summarise_result_step4 <- editor_module_server("step4_editor",list(
    unstressed_data = unstressed_data,
    unstressed_sd   = unstressed_sd
  ), c("unstressed_data", "unstressed_sd"), predefined_code_step4, "result", session_folder_id, "Step 4: Summarise Data")

output$step4_box <- renderUI({
      req(!is.null(summarise_result_step3()), !is.null(summarise_result_step3()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step4_box",
              title = "4️⃣ Calculate the Standard Error of the Mean of Unstressed Data",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_summarise_data_sem_unstressed.Rmd"),
                  uiOutput(session$ns("summary_code_feedback_step4"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step4_editor"))
                  )
              )
            )
          ),
          
        )
      )
    })

observe({
  req(!is.null(summarise_result_step4()), !is.null(summarise_result_step4()$result))
  
  if ( is.numeric(summarise_result_step4()$result) &&
       length(summarise_result_step4()$result) == 1 ) {
    
    output$summary_code_feedback_step4 <- renderUI({
      tagList(
        div(class = "success-box", "\U1F64C Great!")
      )
    })
    
  } else {
    
    output$summary_code_feedback_step4 <- renderUI({
      div(class = "error-box", "\U1F914 Not quite - try again!")
    })
    
  }
})


# step5: your turn with stressed data - filter

predefined_code_step5 = read_file("markdown/07_analysis/predefined_code_calculate_filter_stressed.txt")
summarise_result_step5 <- editor_module_server("step5_editor", average_trs, "average_trs", predefined_code = predefined_code_step5, return_type = "result", session_folder_id, save_header = "Step 5: Summarise Data")


output$step5_box <- renderUI({
  req(!is.null(summarise_result_step4()), !is.null(summarise_result_step4()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step5_box",
              title = "5️⃣ Your turn! First, filter the data to the STRESSED group",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_summarise_data_filter_stressed.Rmd"),
                  uiOutput(session$ns("summary_code_feedback_step5"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step5_editor"))
                  )
              )
            )
          ),
          
        )
      )
    })

observe({
  req(!is.null(summarise_result_step5()), !is.null(summarise_result_step5()$result))
  
  if (tibble::is_tibble(summarise_result_step5()$result)) {
    
    output$summary_code_feedback_step5 <- renderUI({
      tagList(
        div(class = "success-box", "\U1F64C Great!"),
        
      )

 
    })
    
  } else {
    
    output$summary_code_feedback_step5 <- renderUI({
      div(class = "error-box", "\U1F914 Not quite - try again!")
    })
    
  }
})

# step6: your turn with stressed data - mean

predefined_code_step6 = read_file("markdown/07_analysis/predefined_code_calculate_mean_stressed.txt")
summarise_result_step6 <- editor_module_server("step6_editor", stressed_data, "stressed_data", predefined_code = predefined_code_step6, return_type = "result", session_folder_id, save_header = "Step 5: Summarise Data")

output$step6_box <- renderUI({
  req(!is.null(summarise_result_step5()), !is.null(summarise_result_step5()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step6_box",
              title = "6️⃣ Your turn! Calculate the mean for the stressed group.",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_summarise_data_mean_stressed.Rmd"),
                  uiOutput(session$ns("summary_code_feedback_step6"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step6_editor"))
                  )
              )
            )
          ),
          
        )
      )
    })


observe({
  req(!is.null(summarise_result_step6()), !is.null(summarise_result_step6()$result))
  
  #mean_value <- as.numeric(summarise_result_step6()$result[[1]])
  stressed_mean_value <- as.numeric(stressed_mean())

if (!is.null(summarise_result_step6()$result[[1]]) &&
    is.numeric(summarise_result_step6()$result[[1]]) &&
    length(summarise_result_step6()$result[[1]]) == 1 &&
    (summarise_result_step6()$result[[1]] == stressed_mean_value)) {
    
    output$summary_code_feedback_step6 <- renderUI({
      tagList(
        div(class = "success-box", "\U1F64C Great!"),
        
      )

 
    })
    
  } else {
    
    output$summary_code_feedback_step6 <- renderUI({
      div(class = "error-box", "\U1F914 Not quite - try again!")
    })
    
  }
})


# step7: your turn with stressed data - sd

predefined_code_step7 = read_file("markdown/07_analysis/predefined_code_calculate_sd_stressed.txt")
summarise_result_step7 <- editor_module_server("step7_editor", stressed_data, "stressed_data", predefined_code = predefined_code_step7, return_type = "result", session_folder_id, save_header = "Step 5: Summarise Data")


output$step7_box <- renderUI({
  req(!is.null(summarise_result_step6()), !is.null(summarise_result_step6()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step7_box",
              title = "7️⃣ Your turn! Calculate the sd for the stressed group.",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_summarise_data_sd_stressed.Rmd"),
                  uiOutput(session$ns("summary_code_feedback_step7"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step7_editor"))
                  )
              )
            )
          ),
          
        )
      )
    })

observe({
  req(!is.null(summarise_result_step7()), !is.null(summarise_result_step7()$result))
  
  #sd_value <- as.numeric(summarise_result_step7()$result[[1]])
  stressed_sd_value <- as.numeric(stressed_sd())

if (!is.null(summarise_result_step7()$result[[1]]) &&
    is.numeric(summarise_result_step7()$result[[1]]) &&
    length(summarise_result_step7()$result[[1]]) == 1 &&
    (summarise_result_step7()$result[[1]] == stressed_sd_value)) {
    
    output$summary_code_feedback_step7 <- renderUI({
      tagList(
        div(class = "success-box", "\U1F64C Great!"),
        
      )

 
    })
    
  } else {
    
    output$summary_code_feedback_step7 <- renderUI({
      div(class = "error-box", "\U1F914 Not quite - try again!")
    })
    
  }
})

# step8: your turn with stressed data - n and sem

predefined_code_step8 = read_file("markdown/07_analysis/predefined_code_calculate_sem_stressed.txt")
summarise_result_step8 <- editor_module_server("step8_editor", list(stressed_data, stressed_n, stressed_sd), c("stressed_data", "stressed_n", "stressed_sd"), predefined_code = predefined_code_step8, return_type = "result", session_folder_id, save_header = "Step 5: Summarise Data")


output$step8_box <- renderUI({
  req(!is.null(summarise_result_step7()), !is.null(summarise_result_step7()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step8_box",
              title = "8️⃣ Your turn! Calculate the n and sem for the stressed group.",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_summarise_data_sem_stressed.Rmd"),
                  uiOutput(session$ns("summary_code_feedback_step8"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step8_editor"))
                  )
              )
            )
          ),
          
        )
      )
    })

observe({
  req(!is.null(summarise_result_step8()), !is.null(summarise_result_step8()$result))
  
  #sem_value <- as.numeric(summarise_result_step8()$result[[1]])
  #n_value <- as.numeric(summarise_result_step8()$result[[1]])
  stressed_n_value <- as.numeric(stressed_n())
  stressed_sem_value <- as.numeric(stressed_sem())
  stressed_sd_value <- as.numeric(stressed_sd())

if (!is.null(summarise_result_step8()$result) &&
    is.numeric(summarise_result_step8()$result) &&
    length(summarise_result_step8()$result) == 1 &&
    (summarise_result_step8()$result[[1]] == stressed_n_value)) {
    
    output$summary_code_feedback_step8 <- renderUI({
      tagList(
        div(class = "success-box", "\U1F64C Great!"),
        strong("Now, now you have the n, you can calculate the sem")
      )
    })

  } else if (!is.null(summarise_result_step8()$result) &&
    is.numeric(summarise_result_step8()$result) &&
    length(summarise_result_step8()$result) == 1 &&
    (summarise_result_step8()$result[[1]] == stressed_sem_value)) {
    
    output$summary_code_feedback_step8 <- renderUI({
      tagList(
        div(class = "success-box", "\U1F64C Great!")
      )
    })

  } else if (!is.null(summarise_result_step8()$result) &&
    is.numeric(summarise_result_step8()$result) &&
    length(summarise_result_step8()$result) == 1 &&
    (summarise_result_step8()$result[[1]] == stressed_sd_value)) {
    
    output$summary_code_feedback_step8 <- renderUI({
      tagList(
      div(class = "error-box", "\U1F914 Not quite - try again!")
      )
    })

  } else {
    
    output$summary_code_feedback_step8 <- renderUI({
      div(class = "error-box", "\U1F914 Not quite - try again!")
    })
    
  }
})




# step9: dplyr shortcut
predefined_code_step9 = read_file("markdown/07_analysis/predefined_code_calculate_dplyr.txt")
summarise_result_step9 <- editor_module_server("step9_editor", average_trs, "average_trs", predefined_code_step9, "result", session_folder_id, "Step 6: Summarise Data with Dplyr")

output$step9_box <- renderUI({
  req(!is.null(summarise_result_step8()), !is.null(summarise_result_step8()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step9_box",
              title = "6️⃣ Use Dplyr to quickly summarise your data",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_summarise_data_dplyr.Rmd"),
                  uiOutput(session$ns("summary_code_feedback_step9"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step9_editor")),
                  uiOutput(session$ns("save_summary_result"))
                  )
              )
            )
          ),
          
        )
      )
    })

observe({
  req(!is.null(summarise_result_step9()), !is.null(summarise_result_step9()$result))
  
  if (tibble::is_tibble(summarise_result_step9()$result)) {
    
    output$summary_code_feedback_step9 <- renderUI({
      tagList(
        div(class = "success-box", "\U1F64C Great!"),
        markdown("Next let's take a look at the result."),
          numericInput(
            inputId = session$ns("mean_unstressed_group_quiz"),
            label = "What is the mean of the unstressed group?",
            value = 0,
            min = 5,
            max = 60
          ),
          div(
          style = "text-align: center;",
          actionButton(
            session$ns("submit_mean_unstressed_group_quiz_answer"),
            label = "Submit",
            class = "fun-submit-button"
          )
          ),
          uiOutput(session$ns("mean_unstressed_group_quiz_feedback")),
          numericInput(
            inputId = session$ns("sem_stressed_group_quiz"),
            label = "What is the standard error of the mean (sem) of the stressed group?",
            value = 0,
            min = 5,
            max = 60
          ),
          div(
                style = "text-align: center;",
          actionButton(
            session$ns("submit_sem_stressed_group_quiz_answer"),
            label = "Submit",
            class = "fun-submit-button"
          )),
          uiOutput(session$ns("submit_sem_stressed_group_quiz_feedback")),
          radioButtons(
            session$ns("summary_result_interpretation_quiz"), 
            label = "Can we tell from this if this is statistically significant?", 
            choices = list(
              "Yes" = "option1", 
              "No" = "option2"
            ),
            selected = character(0)
          ),
          uiOutput(session$ns("summary_result_interpretation_quiz_feedback")),
      )
    })

    output$save_summary_result <- renderUI({
        tagList(
          div(
      style = "display: flex; justify-content: center; align-items: center; width: 100%;",
          actionButton(
            session$ns("save_summary_results_button"),
            label = tagList(icon("save"), "Save Results to Dashboard"),
            class = "action-button custom-action",
            `data-id` = "summary_save"
          )
          )
        )
        }) 
        
    
  } else {
    
    output$summary_code_feedback_step9 <- renderUI({
      div(class = "error-box", "\U1F914 Not quite - try again!")
    })
    
    # output$save_summary_result <- renderUI({
    #       NULL
    #     })
  }
})
    
    unstressed_mean <- reactive({
      sr <- summarise_result_step9()
      
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
  sr <- summarise_result_step9()

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
      if (!is.null(summarise_result_step9())) {
        key <- "summary"
        saved_results$scripts[[key]] <- summarise_result_step9()
        
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
        
        showNotification("Summary script saved successfully & Uploaded to Drive.", type = "message", duration = 3)
      } else {
        showNotification("No summary script to save.", type = "error", duration = 3)
      }
    })

# navigation buttons

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

    observeEvent(input$back_page_summarise, {
        updateTabItems(parent.session, "sidebar", "Prepare_Data")
      })
      observeEvent(input$next_page_summarise, {
        updateTabItems(parent.session, "sidebar", "Create_Figure")
      })
 
    
  })
}