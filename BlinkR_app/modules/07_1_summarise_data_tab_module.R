analysis_summarise_data_module_ui <- function(id, i18n) {
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
                  tagList(shiny::icon("rectangle-list"), i18n$t("Analysis: Summarise Data"))
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
                label = tagList(icon("equals"), i18n$t("Run Statistical Analysis")),
                class = "action-button custom-action",
                `data-id` = "stats"
              ),
              actionButton(
                ns("figure"),
                label = tagList(icon("chart-simple"), i18n$t("Create a Figure")),
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
                label = tagList(icon("dashboard"), i18n$t("Go to Analysis Dashboard")),
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
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_summarise"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
))}


analysis_summarise_data_module_server <- function(id, i18n, results_data, parent.session, saved_results, session_folder_id, process_markdown) {
  moduleServer(id, function(input, output, session) {
  vars <- get_experiment_vars()

  #step1 data prep
    average_trs <- reactive({ NULL })
    
    average_trs_results <- results_data() %>%
      select(-"group", -"initials", -"submission_id") %>%
      dplyr::group_by(id, !!sym(vars$levels_variable_name)) %>%
      dplyr::summarise(
        average_measurement = mean(!!sym(vars$measurement_variable_name), na.rm = TRUE),
        .groups = 'drop'
      )
    
    average_trs <- reactive({ average_trs_results })
  
  #step1 calculation level b
    level_b_data <- reactive({ NULL })

    level_b_data_result <- average_trs() %>%
      filter(!!sym(vars$levels_variable_name) == vars$level_b_variable_name)
    
    level_b_data <- reactive({ level_b_data_result })

  #step2 calculation level b
    level_b_mean <- reactive({ NULL })

    level_b_mean_result <- mean(level_b_data()$average_measurement, na.rm = TRUE) 
    
    level_b_mean <- reactive({ level_b_mean_result })

  #step3 calculation level b

    level_b_sd <- reactive({ NULL })

    level_b_sd_result <- sd(level_b_data()$average_measurement, na.rm = TRUE)

    level_b_sd <- reactive({ level_b_sd_result })

  #step4 calculation level b
  
    level_b_n <- reactive({ NULL })

    level_b_n_result <- nrow(level_b_data())

    level_b_n <- reactive({ level_b_n_result })

    level_b_sem <- reactive({ NULL })

    level_b_sem_result <- level_b_sd() / sqrt(level_b_n())

    level_b_sem <- reactive({ level_b_sem_result })

## level_a reactive values
#step1 calculation level a

    level_a_data <- reactive({
  average_trs() %>%
    filter(
      !!sym(vars$levels_variable_name) == vars$level_a_variable_name
    )
})

  #step2 calculation level a

level_a_mean <- reactive({
  mean(level_a_data()$average_measurement, na.rm = TRUE)
})

  #step3 calculation level a

level_a_sd <- reactive({
  sd(level_a_data()$average_measurement, na.rm = TRUE)
})

  #step4 calculation level a

level_a_n <- reactive({
  nrow(level_a_data())
})

level_a_sem <- reactive({
  level_a_sd() / sqrt(level_a_n())
})

# step1: filter to level_b

  predefined_code_step1 <- whisker.render(
  read_file("markdown/07_analysis/predefined_code_summarise_filter_level_b.txt"),
  vars
  )
  summarise_result_step1 <- editor_module_server("step1_editor", i18n, average_trs, "average_trs", predefined_code = predefined_code_step1, return_type = "result", session_folder_id, save_header = i18n$t("Step 1: Summarise Data"))

  output$analysis_filter_b <- renderUI({
    process_markdown("07_analysis/analysis_summarise_data_filter_level_b.Rmd")
  })

  output$step1_box <- renderUI({
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step1_box",
              title = sprintf(i18n$t("1️⃣ Filter to %s Data"), vars$level_b_variable_name),
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  uiOutput(session$ns("analysis_filter_b")),
                  uiOutput(session$ns("summary_code_feedback_step1"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step1_editor"), i18n)
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
            div(class = "success-box", i18n$t("\U1F64C Great!")),
          )
        })
        } else {
        output$summary_code_feedback_step1 <- renderUI({
          div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
        })
        }
      })


# step2: mean of level_b

  level_b_data_name <- paste0(vars$level_b_variable_name, "_data")
  predefined_code_step2 <- whisker.render(
  read_file("markdown/07_analysis/predefined_code_calculate_mean_level_b.txt"),
  vars
  )
  summarise_result_step2 <- editor_module_server("step2_editor", i18n, level_b_data, level_b_data_name, predefined_code = predefined_code_step2, return_type = "result", session_folder_id, save_header = i18n$t("Step 2: Summarise Data"))


output$analysis_mean_b <- renderUI({
  process_markdown("07_analysis/analysis_summarise_data_mean_level_b.Rmd")
})

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
              title = sprintf(i18n$t("2️⃣ Calculate the Mean of %s Data"), vars$level_b_text_name),
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  uiOutput(session$ns("analysis_mean_b")),
                  uiOutput(session$ns("summary_code_feedback_step2"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step2_editor"), i18n)
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
        div(class = "success-box", i18n$t("\U1F64C Great!"))
      )
    })
    
  } else {
    
    output$summary_code_feedback_step2 <- renderUI({
      div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
    })
    
  }
})


# step3: sd of level_b

predefined_code_step3 <- whisker.render(
read_file("markdown/07_analysis/predefined_code_calculate_sd_level_b.txt"),
vars
)
summarise_result_step3 <- editor_module_server("step3_editor", i18n, level_b_data, level_b_data_name, predefined_code = predefined_code_step3, return_type = "result", session_folder_id, save_header = i18n$t("Step 2: Summarise Data"))

output$analysis_sd_b <- renderUI({
  process_markdown("07_analysis/analysis_summarise_data_sd_level_b.Rmd")
})

output$step3_box <- renderUI({
      req(!is.null(summarise_result_step2()), !is.null(summarise_result_step2()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step3_box",
              title = sprintf(i18n$t("3️⃣ Calculate the Standard Deviation of %s Data"), vars$level_b_text_name),
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  uiOutput(session$ns("analysis_sd_b")),
                  uiOutput(session$ns("summary_code_feedback_step3"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step3_editor"), i18n)
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
        div(class = "success-box", i18n$t("\U1F64C Great!"))
      )
    })
    
  } else {
    
    output$summary_code_feedback_step3 <- renderUI({
      div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
    })
    
  }
})

# step4: n and sem of level_b

level_b_sd_name <- paste0(vars$level_b_variable_name, "_sd")

predefined_code_step4 <- whisker.render(
  read_file("markdown/07_analysis/predefined_code_calculate_sem_level_b.txt"),
  vars
  )

summarise_result_step4 <- editor_module_server("step4_editor",i18n, list(
    level_b_data = level_b_data,
    level_b_sd   = level_b_sd
  ), c(level_b_data_name, level_b_sd_name), predefined_code_step4, "result", session_folder_id, i18n$t("Step 4: Summarise Data"))

output$analysis_sem_b <- renderUI({
  process_markdown("07_analysis/analysis_summarise_data_sem_level_b.Rmd")
})

output$step4_box <- renderUI({
      req(!is.null(summarise_result_step3()), !is.null(summarise_result_step3()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step4_box",
              title = sprintf(i18n$t("4️⃣ Calculate the Standard Error of the Mean of %s Data"), vars$level_b_text_name),
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  uiOutput(session$ns("analysis_sem_b")),
                  uiOutput(session$ns("summary_code_feedback_step4"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step4_editor"), i18n)
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
        div(class = "success-box", i18n$t("\U1F64C Great!"))
      )
    })
    
  } else {
    
    output$summary_code_feedback_step4 <- renderUI({
      div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
    })
    
  }
})


# step5: your turn with level_a data - filter

predefined_code_step5 <- whisker.render(
  read_file("markdown/07_analysis/predefined_code_calculate_filter_level_a.txt"),
  vars
  )

summarise_result_step5 <- editor_module_server("step5_editor", i18n, average_trs, "average_trs", predefined_code = predefined_code_step5, return_type = "result", session_folder_id, save_header = i18n$t("Step 5: Summarise Data"))

output$analysis_filter_a <- renderUI({
  process_markdown("07_analysis/analysis_summarise_data_filter_level_a.Rmd")
})

output$step5_box <- renderUI({
  req(!is.null(summarise_result_step4()), !is.null(summarise_result_step4()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step5_box",
              title = sprintf(i18n$t("5️⃣ Your turn! First, filter the data to the %s group"), vars$level_a_variable_name),
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  uiOutput(session$ns("analysis_filter_a")),
                  uiOutput(session$ns("summary_code_feedback_step5"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step5_editor"), i18n)
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
        div(class = "success-box", i18n$t("\U1F64C Great!")),
        
      )

 
    })
    
  } else {
    
    output$summary_code_feedback_step5 <- renderUI({
      div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
    })
    
  }
})

# step6: your turn with level_a data - mean

level_a_data_name <- paste0(vars$level_a_variable_name, "_data")
level_a_sd_name <- paste0(vars$level_a_variable_name, "_sd")
level_a_n_name <- paste0(vars$level_a_variable_name, "_n")
level_a_sem_name <- paste0(vars$level_a_variable_name, "_sem")

predefined_code_step6 <- whisker.render(
  read_file("markdown/07_analysis/predefined_code_calculate_mean_level_a.txt"),
  vars
  )
summarise_result_step6 <- editor_module_server("step6_editor", i18n, level_a_data, level_a_data_name, predefined_code = predefined_code_step6, return_type = "result", session_folder_id, save_header = i18n$t("Step 5: Summarise Data"))

output$analysis_mean_a <- renderUI({
  process_markdown("07_analysis/analysis_summarise_data_mean_level_a.Rmd")
})

output$step6_box <- renderUI({
  req(!is.null(summarise_result_step5()), !is.null(summarise_result_step5()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step6_box",
              title = sprintf(i18n$t("6️⃣ Your turn! Calculate the mean for the  %s group"), vars$level_a_text_name),
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  uiOutput(session$ns("analysis_mean_a")),
                  uiOutput(session$ns("summary_code_feedback_step6"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step6_editor"), i18n)
                  )
              )
            )
          ),
          
        )
      )
    })


observe({
  req(!is.null(summarise_result_step6()), !is.null(summarise_result_step6()$result))
  
  level_a_mean_value <- as.numeric(level_a_mean())

if (!is.null(summarise_result_step6()$result[[1]]) &&
    is.numeric(summarise_result_step6()$result[[1]]) &&
    length(summarise_result_step6()$result[[1]]) == 1 &&
    (summarise_result_step6()$result[[1]] == level_a_mean_value)) {
    
    output$summary_code_feedback_step6 <- renderUI({
      tagList(
        div(class = "success-box", i18n$t("\U1F64C Great!")),
        
      )

 
    })
    
  } else {
    
    output$summary_code_feedback_step6 <- renderUI({
      div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
    })
    
  }
})


# step7: your turn with level_a data - sd

predefined_code_step7 <- whisker.render(
  read_file("markdown/07_analysis/predefined_code_calculate_sd_level_a.txt"),
  vars
  )
summarise_result_step7 <- editor_module_server("step7_editor", i18n, level_a_data, level_a_data_name, predefined_code = predefined_code_step7, return_type = "result", session_folder_id, save_header = i18n$t("Step 5: Summarise Data"))

output$analysis_sd_a <- renderUI({
  process_markdown("07_analysis/analysis_summarise_data_sd_level_a.Rmd")
})

output$step7_box <- renderUI({
  req(!is.null(summarise_result_step6()), !is.null(summarise_result_step6()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step7_box",
              title = sprintf(i18n$t("7️⃣ Your turn! Calculate the sd for the %s group"), vars$level_a_text_name),
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  uiOutput(session$ns("analysis_sd_a")),
                  uiOutput(session$ns("summary_code_feedback_step7"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step7_editor"), i18n)
                  )
              )
            )
          ),
          
        )
      )
    })

observe({
  req(!is.null(summarise_result_step7()), !is.null(summarise_result_step7()$result))
  
  level_a_sd_value <- as.numeric(level_a_sd())

if (!is.null(summarise_result_step7()$result[[1]]) &&
    is.numeric(summarise_result_step7()$result[[1]]) &&
    length(summarise_result_step7()$result[[1]]) == 1 &&
    (summarise_result_step7()$result[[1]] == level_a_sd_value)) {
    
    output$summary_code_feedback_step7 <- renderUI({
      tagList(
        div(class = "success-box", i18n$t("\U1F64C Great!")),
        
      )

 
    })
    
  } else {
    
    output$summary_code_feedback_step7 <- renderUI({
      div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
    })
    
  }
})

# step8: your turn with level_a data - n and sem

predefined_code_step8 <- whisker.render(
  read_file("markdown/07_analysis/predefined_code_calculate_sem_level_a.txt"),
  vars
  )

summarise_result_step8 <- editor_module_server("step8_editor", i18n, list(level_a_data, level_a_n, level_a_sd), c(level_a_data_name, level_a_n_name, level_a_sd_name), predefined_code = predefined_code_step8, return_type = "result", session_folder_id, save_header = i18n$t("Step 5: Summarise Data"))

output$analysis_sem_a <- renderUI({
  process_markdown("07_analysis/analysis_summarise_data_sem_level_a.Rmd")
})

output$step8_box <- renderUI({
  req(!is.null(summarise_result_step7()), !is.null(summarise_result_step7()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step8_box",
              title = sprintf(i18n$t("8️⃣ Your turn! Calculate the n and sem for the %s group"), vars$level_a_text_name),
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  uiOutput(session$ns("analysis_sem_a")),
                  uiOutput(session$ns("summary_code_feedback_step8"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step8_editor"), i18n)
                  )
              )
            )
          ),
          
        )
      )
    })

observe({
  req(!is.null(summarise_result_step8()), !is.null(summarise_result_step8()$result))

  level_a_n_value  <- as.numeric(level_a_n())
  level_a_sem_value <- as.numeric(level_a_sem())

  result_values <- summarise_result_step8()$result
  
  if (!is.numeric(result_values)) {
    output$summary_code_feedback_step8 <- renderUI({
      div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
    })
    return()
  }

  if (
    length(result_values) == 1 &&
    isTRUE(all.equal(result_values[[1]], level_a_n()))
  ) {
    output$summary_code_feedback_step8 <- renderUI({
      tagList(
        div(class = "success-box", i18n$t("\U1F64C Great!")),
        strong(i18n$t("Now you have the n. Next, calculate the sem!"))
      )
    })

  } else if (
    length(result_values) == 1 &&
    isTRUE(all.equal(result_values[[1]], level_a_sem()))
  ) {
    output$summary_code_feedback_step8 <- renderUI({
      tagList(
        div(class = "success-box", i18n$t("\U1F64C Great!"))
      )
    })

  } else if (
    length(result_values) == 2 &&
    isTRUE(all.equal(result_values, c(level_a_n(), level_a_sem())))
  ) {
    output$summary_code_feedback_step8 <- renderUI({
      tagList(
        div(class = "success-box", i18n$t("\U1F64C Great!"))
      )
    })

  } else {
    output$summary_code_feedback_step8 <- renderUI({
      div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
    })
  }
})

# step9: dplyr shortcut
predefined_code_step9 <- whisker.render(
  read_file("markdown/07_analysis/predefined_code_calculate_dplyr.txt"),
  vars
  )

summarise_result_step9 <- editor_module_server("step9_editor", i18n, average_trs, "average_trs", predefined_code_step9, "result", session_folder_id, i18n$t("Step 6: Summarise Data with Dplyr"))

output$analysis_dplyr <- renderUI({
  process_markdown("07_analysis/analysis_summarise_data_dplyr.Rmd")
})

output$step9_box <- renderUI({
  req(!is.null(summarise_result_step8()), !is.null(summarise_result_step8()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step9_box",
              title = i18n$t("9️⃣ Use Dplyr to quickly summarise your data"),
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  uiOutput(session$ns("analysis_dplyr")),
                  uiOutput(session$ns("summary_code_feedback_step9"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step9_editor"), i18n),
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
        div(class = "success-box", i18n$t("\U1F64C Great!")),
        markdown(i18n$t("Next let's take a look at the result.")),
          numericInput(
            inputId = session$ns("mean_level_b_variable_group_quiz"),
            label = sprintf(i18n$t("What is the mean of the %s group"), vars$level_b_variable_name),
            value = 0,
            min = 5,
            max = 60
          ),
          div(
          style = "text-align: center;",
          actionButton(
            session$ns("submit_mean_level_b_variable_group_quiz_answer"),
            label = i18n$t("Submit"),
            class = "fun-submit-button"
          )
          ),
          uiOutput(session$ns("mean_level_b_variable_group_quiz_feedback")),
          numericInput(
            inputId = session$ns("sem_level_a_group_quiz"),
            label = sprintf(i18n$t("What is the standard error of the mean (sem) of the %s group"), vars$level_a_variable_name),
            value = 0,
            min = 5,
            max = 60
          ),
          div(
                style = "text-align: center;",
          actionButton(
            session$ns("submit_sem_level_a_group_quiz_answer"),
            label = i18n$t("Submit"),
            class = "fun-submit-button"
          )),
          uiOutput(session$ns("submit_sem_level_a_group_quiz_feedback")),
          radioButtons(
            session$ns("summary_result_interpretation_quiz"), 
            label = i18n$t("Can we tell from this if this is statistically significant?"), 
            choiceNames = list(
                  i18n$t("Yes"),
                  i18n$t("No")
                ),
                choiceValues = list(
                  "option1", 
                  "option2"
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
            label = tagList(icon("save"), i18n$t("Save Results to Dashboard")),
            class = "action-button custom-action",
            `data-id` = "summary_save"
          )
          )
        )
        }) 
        
    
  } else {
    
    output$summary_code_feedback_step9 <- renderUI({
      div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
    })
    
  }
})
    
    level_b_mean <- reactive({
      sr <- summarise_result_step9()
      
      if (is.null(sr) || is.null(sr$result)) {
        return(NULL)
      }
      if (!tibble::is_tibble(sr$result)) {
        return(NULL)
      }
      if (!all(c(vars$levels_variable_name, "mean") %in% names(sr$result))) {
        return(NULL)
      }
      
      df_level_b <- sr$result %>%
        dplyr::filter(!!sym(vars$levels_variable_name) == vars$level_b_variable_name)
      
      if (nrow(df_level_b) == 0) {
        return(NULL)
      }
      
      df_level_b$mean[1]
    })


    level_b_mean_round <- reactive({
    level_b_mean_round_val <- level_b_mean()
    if (is.null(level_b_mean_round_val)) return(NULL)
    round(level_b_mean_round_val, 2)
  })

 
    
observeEvent(input$submit_mean_level_b_variable_group_quiz_answer, {
  val <- level_b_mean_round()

  if (is.null(val)) {
    output$mean_level_b_variable_group_quiz_feedback <- renderUI({
      div(class = "error-box", i18n$t("\U1F914 We do not have a valid mean yet!"))
    })
    return()
  }

  user_answer_mean_level_b <- as.numeric(input$mean_level_b_variable_group_quiz)

  if (is.na(user_answer_mean_level_b)) {
    feedback <- div(class = "error-box", i18n$t("\U1F914 Please enter a numeric value!"))
  } else {
    tolerance <- 0.5
    
    if (abs(user_answer_mean_level_b - val) <= tolerance) {
      feedback <- div(class = "success-box", i18n$t("\U1F64C Correct!"))
    } else {
      feedback <- div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
    }
  }

  output$mean_level_b_variable_group_quiz_feedback <- renderUI({
    feedback
  })
})

    #for level_a sem
  level_a_sem_step9 <- reactive({
  sr <- summarise_result_step9()

  if (is.null(sr) || is.null(sr$result)) {
    return(NULL)
  }
  if (!tibble::is_tibble(sr$result)) {
    return(NULL)
  }
  if (!all(c(vars$levels_variable_name, "sem") %in% names(sr$result))) {
    return(NULL)
  }

  df_level_a <- sr$result %>%
    dplyr::filter(!!sym(vars$levels_variable_name) == vars$level_a_variable_name)

  if (nrow(df_level_a) == 0) {
    return(NULL)
  }

  df_level_a$sem[1]
})

level_a_sem_step9_round <- reactive({
  val <- level_a_sem_step9()
  if (is.null(val)) return(NULL)
  round(val, 2)
})

observeEvent(input$submit_sem_level_a_group_quiz_answer, {
  val <- level_a_sem_step9_round()
  
  if (is.null(val)) {
    output$submit_sem_level_a_group_quiz_feedback <- renderUI({
      div(class = "error-box", i18n$t("\U1F914 We do not have a valid SEM yet!"))
    })
    return()
  }
  
  user_answer_sem_level_a <- as.numeric(input$sem_level_a_group_quiz)
  
  if (is.na(user_answer_sem_level_a)) {
    feedback <- div(class = "error-box", i18n$t("\U1F914 Please enter a numeric value!"))
  } else {
    tolerance <- 0.1
    
    if (abs(user_answer_sem_level_a - val) <= tolerance) {
      feedback <- div(class = "success-box", i18n$t("\U1F64C Correct!"))
    } else {
      feedback <- div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
    }
  }
  
  output$submit_sem_level_a_group_quiz_feedback <- renderUI({
    feedback
  })
})

    
    #for significance radio quiz
    observeEvent(input$summary_result_interpretation_quiz, {
      feedback <- if (input$summary_result_interpretation_quiz == "option2") {
        div(class = "success-box", i18n$t("\U1F64C Correct!"))
      } else {
        div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
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
        
        showNotification(i18n$t("Summary script saved successfully & Uploaded to Drive."), type = "message", duration = 3)
      } else {
        showNotification(i18n$t("No summary script to save."), type = "error", duration = 3)
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