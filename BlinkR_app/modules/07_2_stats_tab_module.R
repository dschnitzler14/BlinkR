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
          3,
          actionButton(ns("run_qq_Plot"), "Generate Q-Q Plot"),
          plotOutput(ns("q_q_plot")),
          uiOutput(ns("qq_explainer_ui")),
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
                 variance_qq_plot <- qqPlot(average_trs$average_blinks_per_minute,
                 main = \"Q-Q Plot of Average Blinks/Minute\",
                 xlab = \"Theoretical Quantiles\",
                 ylab = \"Sample Quantiles\",
                 col = \"blue\",
                 pch = 20)
                 ```
                "
              )
            )
          ),
          align = "center",
        ),
        column(
          3,
          actionButton(ns("run_box_Plot"), "Generate Boxplot"),
          plotOutput(ns("box_plot")),
          uiOutput(ns("boxplot_explainer_ui")),
          
          box(
            title = "View code used to generate plot",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            status = "info",
            wellPanel(
              includeMarkdown(here("BlinkR_app", "markdown","07_analysis","analysis_boxplot_code.Rmd"))
            )
          ),
          align = "center"
        ),
        column(
          3,
          actionButton(ns("run_hist_Plot"), "Generate Histogram"),
          plotOutput(ns("hist_plot")),
          uiOutput(ns("hist_explainer_ui")),
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
        #column(8, editor_module_ui(ns("t_test_editor")))
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
          label = tagList(icon("dashboard"), "Go to Dashboard"),
          class = "action-button custom-dark-yellow"
        )
      )
    )

  ))))
}




analysis_stats_module_server <- function(id, results_data, parent.session, saved_results, session_folder_id) {
  moduleServer(id, function(input, output, session) {
    # Load data
    data_read <- read.csv(here("BlinkR_app", "data","dummy_blinking_data.csv"))
    
    data <- reactive({
      data_read
    })
    
    #Step 3: Check Assumptions
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
    
    average_trs_assumptions <- data_read %>%
      dplyr::group_by(id, stress_status) %>%
      dplyr::summarise(
        average_blinks_per_minute = mean(blinks_per_minute, na.rm = TRUE),
        .groups = 'drop'
      )
    
    observeEvent(input$run_qq_Plot, {
      output$q_q_plot <- renderPlot({
        qqPlot(
          average_trs_assumptions$average_blinks_per_minute,
          main = "Q-Q Plot of Average Blinks/Minute",
          xlab = "Theoretical Quantiles",
          ylab = "Sample Quantiles",
          col = "blue",
          pch = 20
        )
      })
      
      saved_results$recorded_plots[["q_q_plot"]] <- recordPlot()
      
      
      output$qq_explainer_ui <- renderUI({
        box(
          id = session$ns("qq_explainer_box"),
          title = "Q-Q Plot",
          collapsible = TRUE,
          collapsed = FALSE,
          width = 12,
          includeMarkdown(here("BlinkR_app", "markdown", "07_analysis", "analysis_qq_plot_explainer.Rmd"))
        )
      })
    })
    
    
    average_trs_assumptions$stress_status <- factor(average_trs_assumptions$stress_status,levels = c("unstressed", "stressed"))
    
    observeEvent(input$run_box_Plot, {
      output$box_plot <- renderPlot({
        boxplot(
          average_blinks_per_minute ~ stress_status,
          data = average_trs_assumptions,
          xlab = "Stress Status",
          ylab = "Blinks Per Minute",
          main = "Variance: Blinks/Minute by Stress Status",
          col = c("grey49", "lightgrey")
        )
        stripchart(
          average_blinks_per_minute ~ stress_status,
          data = average_trs_assumptions,
          add = TRUE,
          vertical = TRUE,
          method = "jitter",
          pch = 21,
          bg = "maroon"
        )
      })
    
      saved_results$recorded_plots[["box_plot"]] <- recordPlot()
      
      
      output$boxplot_explainer_ui <- renderUI({
        box(
          id = session$ns("boxplot_explainer_box"),
          title = "Box Plot",
          collapsible = TRUE,
          collapsed = FALSE,
          width = 12,
          includeMarkdown(here("BlinkR_app", "markdown","07_analysis","analysis_box_plot_explainer.Rmd"))
        )
      })
    })
    
    
    observeEvent(input$run_hist_Plot, {
      output$hist_plot <- renderPlot({
        hist(
          average_trs_assumptions$average_blinks_per_minute,
          main = "Distribution of Blinks/Minute",
          xlab = "Average Blinks/Minute",
          ylab = "Frequency",
          col = "grey49",
          border = "black"
        )
        
      })
      
      saved_results$recorded_plots[["hist_plot"]] <- recordPlot()
      
      output$hist_explainer_ui <- renderUI({
        box(
          id = session$ns("hist_explainer_box"),
          title = "Histogram",
          collapsible = TRUE,
          collapsed = FALSE,
          width = 12,
          includeMarkdown(here("BlinkR_app", "markdown","07_analysis","analysis_hist_plot_explainer.Rmd"))
        )
      })
    })
    
    #Step 4: Run T-Test
    
    
    # average_trs <- reactive({
    #   as.data.frame(average_trs_assumptions)
    # })
    

    observeEvent(input$t_test_type_selector, {
      t_test_selector_output <- if (input$t_test_type_selector == "two"){
        markdown("This type of t-test is appropriate for samples that are entirely independent of one-another.
        In order to analyse your data with a two-sample t-test, use this code:
        ```
        t_test <- t.test(average_blinks_per_minute ~ stress_status, var.equal = TRUE, data = average_trs)
        ```
        ")
      } else {
        markdown("This type of t-test is appropriate for samples that are paired, for example repeated measures within the same subject (e.g. before and after).
        In order to analyse your data with a two-sample t-test, we first need to restructure our table and then run to the t-test. Copy this whole code into the editor:
        ```
        average_trs_paired_wide <- average_trs_paired %>%
          pivot_wider(names_from = stress_status, values_from = average_blinks_per_minute)
          
        t_test_paired <- t.test(
          average_trs_paired_wide$stressed,
          average_trs_paired_wide$unstressed,
          paired = TRUE
        )        
        ```
        
        You will notice that this t-test has an extra argument `paired = TRUE`, which indicates that the data is paired.
        ")
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
          editor_module_ui(session$ns("t_test_editor_two_sided"))
        } else {
          editor_module_ui(session$ns("t_test_editor_paired"))
        }
      })
      
      })
    
    t_test_result <- editor_module_server("t_test_editor_two_sided", data = average_trs)
    
    t_test_paired_result <- editor_module_server("t_test_editor_paired", data = average_trs_paired)
    
    
    #two-sided
    t_test <- t.test(average_blinks_per_minute ~ stress_status, var.equal = TRUE, data = average_trs)
    
    alternative_hypothesis <- t_test$alternative
    
    p_value <- t_test$p.value
    p_value_round <- round(p_value,2)
    
    #paired
    data_paired <- read.csv(here("BlinkR_app", "data", "dummy_data_repeated_measures.csv"), header = TRUE)
    
    average_trs_paired <- data_paired %>%
      group_by(id, stress_status) %>%
      summarise(average_blinks_per_minute = mean(blinks_per_minute, na.rm = TRUE), .groups = 'drop')
    as.data.frame(average_trs)
    
    average_trs_paired_wide <- average_trs_paired %>%
      pivot_wider(names_from = stress_status, values_from = average_blinks_per_minute)
    
    t_test_paired <- t.test(
      average_trs_paired_wide$stressed,
      average_trs_paired_wide$unstressed,
      paired = TRUE
    )
    
    p_value_paired <- t_test_paired$p.value
    p_value_round_paired <- round(p_value_paired,2)
    
    method_paired <- t_test_paired$method
    
    # Observer for t_test_result
    observe({
      req(!is.null(t_test_result()))
      if (alternative_hypothesis == "two.sided" && inherits(t_test_result(), "htest")) {
        output$t_test_code_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great Job!"),
            markdown("
        Let's first take a look at the code. We are telling R to use the cookbook `stats` (which I loaded in the background for you),
        and from that book, to use the recipe `t.test`. The ingredients for this recipe are:
        - **`average_blinks_per_minute ~ stress_status`**: It is best practice to order this command `dependent variable ~ independent variable` or `numerical~categorical`. 
        - **`var.equal = TRUE`**: This ingredient tells R that the variances are equal, as we confirmed in the previous step
        - **`data = average_trs`**: Finally, we need to tell R which data to use
        "),
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
              label = "Is this is a statistically significant result?", 
              choices = list(
                "Yes" = "option1", 
                "No" = "option2"
              ),
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
        
        output$save_stats_result <- renderUI({
          NULL
        })
      }
    })
    
    # Observer for t_test_paired_result
    observe({
      req(!is.null(t_test_paired_result()))
      if (method_paired == "Paired t-test" && (inherits(t_test_paired_result(), "htest") | is.data.frame(t_test_paired_result()))) {
        output$t_test_code_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great Job!"),
            markdown("
        Let's first take a look at the code. 
        First, we had to restructure our data so we could pass it to the paired t-test function.
        - **`average_trs_paired_wide <- average_trs_paired %>%`**: again, using a cookbook called `tidyr`, we started our \"sentence\" with the command to create a new dataset called `average_trs_paired_wide` using the dataset `average_trs`
        - **`pivot_wider(names_from = stress_status, values_from = average_blinks_per_minute)`**: using the `pivot_wider`command, we next told R to move the names from the `stress_status` into their own columns (you can check back to the viewing our data section to see the current data structure) and to populate these new columns with the corresponding values found in `average_blinks_per_minute`.
        In the next step, we run our paired t-test using the cookbook `stats` (which I loaded in the background for you), and from that book, to use the recipe `t.test`. The ingredients for this recipe are:
        - **`average_trs_paired_wide$stressed`**: Values from the column `stressed`.
        - **`average_trs_paired_wide$unstressed`**: Values from the column `unstressed`.
        - **`paired = TRUE`**: This tells R that the data is paired and to run a paired t-test.
        "),
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
                "No" = "option2", 
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
      user_answer_p_value <- as.numeric(input$two_sided_p_value_quiz)
      feedback <- 
        if (!is.na(user_answer_p_value) && user_answer_p_value != "" &&
            user_answer_p_value == p_value_round) {        
          div(class = "success-box", "\U1F64C Correct!")
        } else {
          div(class = "error-box", "\U1F914 Not quite - try again!")
        }
      
      output$submit_two_sided_p_value_quiz_feedback <- renderUI({
        feedback
      })
    })
    
    observeEvent(input$submit_two_sided_p_value_quiz_significant, {
      req(p_value)
      p_value <- as.numeric(p_value)
      feedback <- if (p_value <= 0.05 && input$submit_two_sided_p_value_quiz_significant == "option1"){
        div(class = "success-box", "\U1F64C Correct!")
      } else if (p_value >= 0.05 && input$submit_two_sided_p_value_quiz_significant == "option2"){
        div(class = "success-box", "\U1F64C Correct!")
      } else if (p_value <= 0.05 && input$submit_two_sided_p_value_quiz_significant == "option2"){
        div(class = "error-box", "\U1F914 Not quite - try again!")
      } else if (p_value >= 0.05 && input$submit_two_sided_p_value_quiz_significant == "option1"){
        div(class = "error-box", "\U1F914 Not quite - try again!")
        
      }
      output$submit_two_sided_p_value_quiz_significant_feedback <- renderUI({
        feedback
      })
      
    })
    
    
    
    #paired
    observeEvent(input$submit_paired_p_value_quiz_answer, {
      user_answer_p_value <- as.numeric(input$paired_p_value_quiz)
      feedback <- 
        if (!is.na(user_answer_p_value) && user_answer_p_value != "" &&
            user_answer_p_value == p_value_round_paired) {        
          div(class = "success-box", "\U1F64C Correct!")
        } else {
          div(class = "error-box", "\U1F914 Not quite - try again!")
        }
      
      output$submit_paired_p_value_quiz_feedback <- renderUI({
        feedback
      })
    })
    
    observeEvent(input$submit_paired_p_value_quiz_significant, {
      req(p_value_paired)
      p_value <- as.numeric(p_value_paired)
      feedback <- if (p_value_paired <= 0.05 && input$submit_paired_p_value_quiz_significant == "option1"){
        div(class = "success-box", "\U1F64C Correct!")
      } else if (p_value_paired >= 0.05 && input$submit_paired_p_value_quiz_significant == "option2"){
        div(class = "success-box", "\U1F64C Correct!")
      } else if (p_value_paired <= 0.05 && input$submit_paired_p_value_quiz_significant == "option2"){
        div(class = "error-box", "\U1F914 Not quite - try again!")
      } else if (p_value_paired >= 0.05 && input$submit_paired_p_value_quiz_significant == "option1"){
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