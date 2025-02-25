analysis_stats_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "Statistical_Analysis", fluidPage(fluidRow(column(
    12,
    box(
      title = "Statistical Preparation",
      collapsible = TRUE,
      collapsed = FALSE,
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(
          3,
          markdown(
            "We need to first test our assumptions, before we run a t-test. Let's run through a checklist. Tick off each assumption as we go along!"
          ),
          HTML(
            '<div class="yellow-box">
                                    <p>These are standard assumptions for parametric tests, and are listed in decreasing order of importance.</p>
                                    <ol>
                                      <li>Random sampling: samples are randomly sampled from a population to prevent bias.</li>
                                      <li>Independence: Independently obtained to avoid pseudo-replication.</li>
                                      <li>Homogeneity of Variances (no outliers): is the variation amongst the groups within the same range?</li>
                                      <li>Normality within each group: do the groups fall in a normal distribution, or is the data skewed?</li>
                                    </ol>
                                  </div>'
          ),
          checkboxGroupInput(
            ns("assumptions_checklist"),
            label = "Assumptions Checklist",
            choices = c(
              "Random sampling" = "random",
              "Independence" = "indep",
              "Homogeneity of Variances" = "homvar",
              "Normality" = "norm"
            )
          ),
          uiOutput(ns("assumptions_feedback"))
        ),
        
        column(
          9,
          actionButton(ns("run_hist_Plot"), "Generate Histogram to check for Normality"),
          plotOutput(ns("hist_plot")),
          uiOutput(ns("hist_explainer_ui")),
          uiOutput(ns("analysis_t-test")),
          uiOutput(ns("analysis_wilcoxon_test")),
          box(
            title = "View code used to generate plot",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            status = "info",
            wellPanel(
              markdown(
                "
                 ```
                 hist <- hist(average_trs$average_blinks_per_minute,
                 main = \"Distribution of Blinks/Minute\",
                 xlab = \"Average Blinks/Minute\",
                 ylab = \"Frequency\",
                 col = \"grey49\",
                 border = \"black\")
                 ```
                "
              )
            )
          ),
          align = "center"
        ),
      )
    ),
  
    box(
      title = "Analyse the Data",
      collapsible = TRUE,
      collapsed = FALSE,
      width = 12,
      solidHeader = TRUE,
      fluidRow(
        column(
          4,
          markdown("Finally, let\'s analyse the data!"),
          uiOutput(ns("analysis_test_ui")),
          radioButtons(
            ns("t_test_type_selector"),
            label = "What type of t-test should we use?",
            choices = c(
              "two-sample" = "two",
              "paired" = "paired"
            ),
            selected = character(0)
          ),
          
          uiOutput(ns("t_test_selector_output")),
          uiOutput(ns("t_test_code_feedback"))
        ),
        column(8, uiOutput(ns("editor_ui")),
               uiOutput(ns("save_stats_result"))
               )
      )
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      div(
        style = "display: flex; justify-content: center; align-items: center; gap: 20px; height: 100px;",
        actionButton(
          ns("summarise"),
          label = tagList(icon("rectangle-list"), "Summarise the Data"),
          class = "action-button custom-action",
          `data-id` = "summarise_data"
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

  ))))
}




analysis_stats_module_server <- function(id, results_data, parent.session, saved_results, session_folder_id) {
  moduleServer(id, function(input, output, session) {
    
    average_trs_t_test <- reactive({
      NULL
    })
    
    average_trs_t_test_data <- results_data %>%
      select(-"Group", -"Initials", -"Submission_ID") %>%
      dplyr::group_by(ID, Stress_Status) %>%
      dplyr::summarise(
        Average_Blinks_Per_Minute = mean(Blinks_Per_Minute, na.rm = TRUE),
        .groups = 'drop'
      )
    
    average_trs_t_test <- reactive({
      average_trs_t_test_data
    })
    
    
    observeEvent(input$assumptions_checklist, {
      feedback <- NULL
      
      if ("homvar" %in% input$assumptions_checklist) {
        feedback <- div(class = "error-box",
                        "\U1F914 We don't know yet. Generate the plots to find out!")
      }
      if ("norm" %in% input$assumptions_checklist) {
        feedback <- div(class = "error-box",
                        "\U1F914 We don't know yet. Generate the plots to find out!")
      }
      
      if (any(c("random", "indep") %in% input$assumptions_checklist) &&
          !all(c("random", "indep") %in% input$assumptions_checklist) &&
          !("homvar" %in% input$assumptions_checklist) &&
          !("norm" %in% input$assumptions_checklist)) {
        feedback <- div(class = "success-box", "\U1F64C Correct! What Else?")
      }
      
      if (all(c("random", "indep") %in% input$assumptions_checklist) &&
          !("homvar" %in% input$assumptions_checklist) &&
          !("norm" %in% input$assumptions_checklist)) {
        feedback <- div(class = "success-box", "\U1F64C Correct!")
      }
      
      output$assumptions_feedback <- renderUI({
        feedback
      })
    })
    
    average_trs_assumptions <- reactive({ NULL })
    
    average_trs_assumptions_data <- results_data %>%
      select(-"Group", -"Initials", -"Submission_ID") %>%
      dplyr::group_by(ID, Stress_Status) %>%
      dplyr::summarise(
        Average_Blinks_Per_Minute = mean(Blinks_Per_Minute, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(Stress_Status = factor(Stress_Status, levels = c("Unstressed", "Stressed")))
    
    average_trs_assumptions <- reactive({ average_trs_assumptions_data })
    
    
    observeEvent(input$run_hist_Plot, {
      req(average_trs_assumptions())
      
      output$hist_plot <- renderPlot({
        hist(
          average_trs_assumptions()$Average_Blinks_Per_Minute,
          main  = "Distribution of Blinks/Minute",
          xlab  = "Average Blinks/Minute",
          ylab  = "Frequency",
          col   = "grey49",
          border= "black"
        )
        saved_results$recorded_plots[["hist_plot"]] <- recordPlot()
        
        temp_file <- tempfile(fileext = ".png")
        png(temp_file, width = 800, height = 600)
        replayPlot(saved_results$recorded_plots[["hist_plot"]])
        dev.off()
        
        path <- drive_get(as_id(session_folder_id))
        
        drive_upload(
          media = temp_file,
          path = path,
          name = paste0("hist_plot.png"),
          overwrite = TRUE
        )
        
        recordPlot(NULL)
        
        unlink(temp_file)
        
        showNotification("Plot saved successfully.", type = "message")
        
       
        
      })
      
      output$hist_explainer_ui <- renderUI({
        box(
          id = session$ns("hist_explainer_box"),
          title = "Histogram",
          collapsible = FALSE,
          collapsed = FALSE,
          width = 12,
          includeMarkdown(
            "markdown/07_analysis/analysis_hist_plot_explainer.Rmd"
          ),
          actionButton(session$ns("normal"), "The Data is Normal"),
          actionButton(session$ns("not_normal"), "The Data is Not Normal")
        )
      })
    })

  observeEvent(input$normal, {
  output$analysis_test_ui <- renderUI({
    tagList(
      radioButtons(
        ns("t_test_type_selector"),
        label = "What type of t-test should we use?",
        choices = c(
          "Two-sample" = "two",
          "Paired" = "paired"
        ),
        selected = character(0)
      ),
      uiOutput(ns("t_test_selector_output")),
      uiOutput(ns("t_test_code_feedback"))
    )
  })
})

observeEvent(input$not_normal, {
  output$analysis_test_ui <- renderUI({
    tagList(
      box(
        title = "Wilcoxon Test",
        collapsible = TRUE,
        collapsed = FALSE,
        width = 12,
        solidHeader = TRUE,
        includeMarkdown("markdown/07_analysis/analysis_wilcoxon_test.Rmd"),
        editor_module_ui(session$ns("wilcoxon_test_editor"))
      )
    )
  })
})
    
    #Step 4: Run T-Test
  
    perform_two_sample_t_test <- function(data) {
      if (nrow(data) < 2) return(NULL)
      
      out <- tryCatch({
        t.test(
          Average_Blinks_Per_Minute ~ Stress_Status,
          var.equal = TRUE,
          data = data
        )
      }, error = function(e) {
        NULL
      })
      
      out
    }
    
    perform_paired_t_test <- function(data) {
      if (nrow(data) < 2) return(NULL)
      
      wide_data <- tryCatch({
        data %>%
          pivot_wider(
            names_from = Stress_Status, 
            values_from = Average_Blinks_Per_Minute
          )
      }, error = function(e) {
        NULL
      })
      if (is.null(wide_data)) return(NULL)
      
      out <- tryCatch({
        t.test(
          wide_data$Stressed,
          wide_data$Unstressed,
          paired = TRUE
        )
      }, error = function(e) {
        NULL
      })
      
      out
    }
    
    t_test <- perform_two_sample_t_test(average_trs_t_test_data)
    paired_test_result <- perform_paired_t_test(average_trs_t_test_data)
    
    observeEvent(input$t_test_type_selector, {
      req(input$t_test_type_selector)
      if (input$t_test_type_selector == "two"){
        t_test_selector_output <- includeMarkdown("markdown/07_analysis/analysis_two_sided_t_test.Rmd")
        
      } else {
        t_test_selector_output <- includeMarkdown("markdown/07_analysis/analysis_paired_t_test.Rmd")
        
        }
      
      output$t_test_selector_output <- renderUI({
        t_test_selector_output
      })
      
      output$t_test_code_feedback <- renderUI({
        NULL
      })
      
      output$save_stats_result <- renderUI({
        NULL
      })
      
      output$editor_ui <- renderUI({
        NULL
      })
      
      output$editor_ui <- renderUI({
        
        if (input$t_test_type_selector == "two") {
          if (!is.null(t_test)) {
            editor_module_ui(session$ns("t_test_editor_two_sided"))
          } else {
            tags$div(
              "Sorry, you can't perform a two-sided t-test with this data",
              style = "color: red; font-weight: bold; margin-top: 20px;"
            )
          }
        } else {
          if (!is.null(paired_test_result)) {
            editor_module_ui(session$ns("t_test_editor_paired"))
          } else {
            tags$div(
              "Sorry, you can't perform a paired t-test with this data",
              style = "color: red; font-weight: bold; margin-top: 20px;"
            )
          }
        }
      })
      
      
    })
    
    
    predefined_code_two_sided_t_test <- read_file(
      "markdown/07_analysis/predefined_code_two_sided_t_test.txt"
    )
    
    predefined_code_paired_t_test <- read_file(
      "markdown/07_analysis/predefined_code_paired_t_test.txt"
    )
    
    t_test_result <- editor_module_server("t_test_editor_two_sided", data = average_trs_t_test, variable_name = "average_trs", predefined_code = predefined_code_two_sided_t_test, return_type = "result", session_folder_id, save_header = "Two-Sided T-Test Code")
    t_test_paired_result <- editor_module_server("t_test_editor_paired", data = average_trs_t_test, variable_name = "average_trs", predefined_code = predefined_code_paired_t_test, return_type = "result", session_folder_id, save_header = "Paired T-Test Code")
   
  
    
    alternative_hypothesis <- reactive({
  test_res <- t_test_result()
  if (is.null(test_res) || is.null(test_res$result)) return(NULL)
  if (!"alternative" %in% names(test_res$result)) return(NULL)
  test_res$result$alternative
})
    
    p_value <- reactive({
      test_res_p <- t_test_result()
      if (is.null(test_res_p) || is.null(test_res_p$result)) return(NULL)
      if (!"p.value" %in% names(test_res_p$result)) return(NULL)
      test_res_p$result$p.value
    })
  
    p_value_round <- reactive({
    val <- p_value()
    if (is.null(val)) return(NULL)
    round(val, 2)
  })

    
    observe({
      req(!is.null(t_test_result()), !is.null(t_test_result()$result))
      alt <- alternative_hypothesis()

      if (is.null(alt)) {
         output$t_test_code_feedback <- renderUI({
        div(class = "error-box", "\U1F914 Not quite - try again!")
      })
     
      } else if (isTRUE(alt == "two.sided")) {
         output$t_test_code_feedback <- renderUI({
      tagList(
        div(class = "success-box", "\U1F64C Great Job!"),
        includeMarkdown("markdown/07_analysis/analysis_two_sided_code_feedback.Rmd"),
        numericInput(
          inputId = session$ns("two_sided_p_value_quiz"),
          label = "What is the p-value? (2 decimal places)",
          value = 0.00,
          min = 0,
          max = 100
        ),
        actionButton(
          session$ns("submit_two_sided_p_value_quiz_answer"),
          label = "Submit",
          class = "fun-submit-button"
        ),
        uiOutput(session$ns("submit_two_sided_p_value_quiz_feedback")),
        radioButtons(
          session$ns("submit_two_sided_p_value_quiz_significant"),
          label = "Is this a statistically significant result?",
          choices = list("Yes" = "option1", "No" = "option2"),
          selected = character(0)
        ),
        uiOutput(session$ns("submit_two_sided_p_value_quiz_significant_feedback"))
      )
    })
    output$save_stats_result <- renderUI({
      actionButton(
        session$ns("save_stats_two_sample_results_button"),
        label = tagList(icon("save"), "Save Results to Dashboard"),
        class = "action-button custom-action"
      )
    })
      } else {
        output$t_test_code_feedback <- renderUI({
      div(class = "error-box", "\U1F914 Not quite - try again!")
    })
    output$save_stats_result <- renderUI({ NULL })
      }
    })
    

  method_paired <- reactive({
  req(!is.null(t_test_paired_result()), !is.null(t_test_paired_result()$result))

  test_res_paired <- t_test_paired_result()
  if (is.null(test_res_paired) || is.null(test_res_paired$result)) return(NULL)
  if (!"method" %in% names(test_res_paired$result)) return(NULL)
  test_res_paired$result$method
})
    

      p_value_paired <- reactive({
      test_res_p_paired <- t_test_paired_result()
      if (is.null(test_res_p_paired) || is.null(test_res_p_paired$result)) return(NULL)
      if (!"p.value" %in% names(test_res_p_paired$result)) return(NULL)
      test_res_p_paired$result$p.value
    })
  
    p_value_round_paired <- reactive({
    val <- p_value_paired()
    if (is.null(val)) return(NULL)
    round(val, 2)
  })
    
    # Observer for t_test_paired_result
  observe({
       method <- method_paired()
  
  if (is.null(method)) {
    output$t_test_code_feedback <- renderUI({
      div(class = "error-box", "\U1F914 Not quite - try again!")
    })
    output$save_stats_result <- renderUI({
      NULL
    })
    
  } else if (isTRUE(method == "Paired t-test")) {
    output$t_test_code_feedback <- renderUI({
      tagList(
        div(class = "success-box", "\U1F64C Great Job!"),
        includeMarkdown("markdown/07_analysis/analysis_paired_code_feedback.Rmd"),
        numericInput(
          inputId = session$ns("paired_p_value_quiz"),
          label = "What is the p-value? (2 decimal places)",
          value = 0.00,
          min = 0,
          max = 100
        ),
        actionButton(
          session$ns("submit_paired_p_value_quiz_answer"),
          label = "Submit",
          class = "fun-submit-button"
        ),
        uiOutput(session$ns("submit_paired_p_value_quiz_feedback")),
        radioButtons(
          session$ns("submit_paired_p_value_quiz_significant"),
          label = "Is this is a statistically significant result?", 
          choices = list(
            "Yes" = "option1", 
            "No"  = "option2", 
            "I don't know" = "option3"
          ),
          selected = character(0)
        ),
        uiOutput(session$ns("submit_paired_p_value_quiz_significant_feedback"))
      )
    })
    output$save_stats_result <- renderUI({
      actionButton(
        session$ns("save_stats_paired_results_button"),
        label = tagList(icon("save"), "Save Results to Dashboard"),
        class = "action-button custom-action"
      )
    })
    
  } else {
    output$t_test_code_feedback <- renderUI({
      div(class = "error-box", "\U1F914 Not quite - try again!")
    })
    output$save_stats_result <- renderUI({
      NULL
    })
  }
})
    
    
    #two-sided
observeEvent(input$submit_two_sided_p_value_quiz_answer, {
  
  val <- p_value_round()
  
  if (is.null(val)) {
    output$submit_two_sided_p_value_quiz_feedback <- renderUI({
      div(class = "error-box", "\U1F914 We do not have a valid p-value yet!")
    })
    return()
  }

  user_answer_p_value <- as.numeric(input$two_sided_p_value_quiz)
  tolerance <- 0.01


  if (is.na(user_answer_p_value)) {
    feedback <- div(class = "error-box", "\U1F914 Please enter a numeric value!")
  } else if (abs(user_answer_p_value - val) < tolerance) {
    feedback <- div(class = "success-box", "\U1F64C Correct!")
  } else {
    feedback <- div(class = "error-box", "\U1F914 Not quite - try again!")
  }
  
  output$submit_two_sided_p_value_quiz_feedback <- renderUI({ feedback })
})
     
    observeEvent(input$submit_two_sided_p_value_quiz_significant, {
      req(p_value())
      p_value <- as.numeric(p_value())
      feedback <- if (p_value() <= 0.05 && input$submit_two_sided_p_value_quiz_significant == "option1"){
        div(class = "success-box", "\U1F64C Correct!")
      } else if (p_value() >= 0.05 && input$submit_two_sided_p_value_quiz_significant == "option2"){
        div(class = "success-box", "\U1F64C Correct!")
      } else if (p_value() <= 0.05 && input$submit_two_sided_p_value_quiz_significant == "option2"){
        div(class = "error-box", "\U1F914 Not quite - try again!")
      } else if (p_value() >= 0.05 && input$submit_two_sided_p_value_quiz_significant == "option1"){
        div(class = "error-box", "\U1F914 Not quite - try again!")
        
      }
      output$submit_two_sided_p_value_quiz_significant_feedback <- renderUI({
        feedback
      })
      
    })
    
    
    
    #paired
    observeEvent(input$submit_paired_p_value_quiz_answer, {
  val <- p_value_round_paired()

  if (is.null(val)) {
    output$submit_paired_p_value_quiz_feedback <- renderUI({
      div(class = "error-box", "\U1F914 We do not have a valid paired p-value yet!")
    })
    return()
  }

  user_answer_p_value <- as.numeric(input$paired_p_value_quiz)

  tolerance <- 0.01


  if (is.na(user_answer_p_value)) {
    feedback <- div(class = "error-box", "\U1F914 Please enter a numeric value!")
  } else if (abs(user_answer_p_value - val) < tolerance) {
    feedback <- div(class = "success-box", "\U1F64C Correct!")
  } else {
    feedback <- div(class = "error-box", "\U1F914 Not quite - try again!")
  }

  output$submit_paired_p_value_quiz_feedback <- renderUI({ feedback })
})

    
    observeEvent(input$submit_paired_p_value_quiz_significant, {
      req(p_value_paired())
      p_value <- as.numeric(p_value_paired())
      feedback <- if (p_value_paired() <= 0.05 && input$submit_paired_p_value_quiz_significant == "option1"){
        div(class = "success-box", "\U1F64C Correct!")
      } else if (p_value_paired() >= 0.05 && input$submit_paired_p_value_quiz_significant == "option2"){
        div(class = "success-box", "\U1F64C Correct!")
      } else if (p_value_paired() <= 0.05 && input$submit_paired_p_value_quiz_significant == "option2"){
        div(class = "error-box", "\U1F914 Not quite - try again!")
      } else if (p_value_paired() >= 0.05 && input$submit_paired_p_value_quiz_significant == "option1"){
        div(class = "error-box", "\U1F914 Not quite - try again!")
        
      }
      
      output$submit_paired_p_value_quiz_significant_feedback <- renderUI({
        feedback
      })
      
    })

    
    observeEvent(input$save_stats_two_sample_results_button, {
      if (!is.null(t_test_result())) {
        key <- "stats_two_sample"
        saved_results$scripts[[key]] <- t_test_result()
        saved_results$scripts[["stats_paired"]] <- NULL
        
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

        showNotification("Two-Sample T-Test result saved successfully.", type = "message")
      } else {
        showNotification("No result to save.", type = "error")
      }
    })

    
    observeEvent(input$save_stats_paired_results_button, {
      if (!is.null(t_test_paired_result())) {
        key <- "stats_paired"
        saved_results$scripts[[key]] <- t_test_paired_result()
        saved_results$scripts[["stats_two_sample"]] <- NULL
        
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
        
        showNotification("Paired T-Test result saved successfully.", type = "message")
      } else {
        showNotification("No result to save.", type = "error")
      }
    })


    wilcoxon_test_result <- editor_module_server(
  "wilcoxon_test_editor", 
  data = average_trs_t_test, 
  variable_name = "average_trs", 
  predefined_code = read_file("markdown/07_analysis/predefined_code_wilcoxon_test.txt"), 
  return_type = "result", 
  session_folder_id, 
  save_header = "Wilcoxon Test Code"
)

observe({
  req(!is.null(wilcoxon_test_result()), !is.null(wilcoxon_test_result()$result))
  
  if (inherits(wilcoxon_test_result()$result, "htest")) {
    output$wilcoxon_code_feedback <- renderUI({
      div(class = "success-box", "\U1F64C Great Job!")
    })
  } else {
    output$wilcoxon_code_feedback <- renderUI({
      div(class = "error-box", "\U1F914 Not quite - try again!")
    })
  }
})
    
    
    observeEvent(input$summarise, {
      updateTabItems(parent.session, "sidebar", "Summarise_Data")
    })
   
    observeEvent(input$figure, {
      updateTabItems(parent.session, "sidebar", "Create_Figure")
    })
    
    observeEvent(input$dashboard, {
      updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
    })
    
    
  })
}