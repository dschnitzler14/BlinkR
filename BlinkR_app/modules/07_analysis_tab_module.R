analysis_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Analysis",
    fluidPage(
      fluidRow(
        column(
          12,
          shinydashboard::box(
            title = "Step 1: View Data",
            collapsible = TRUE,
            width = 12,
            fluidRow(
              column(
                4,
                markdown(
                  "First, let's have a look at the data. Try running `head(data)` and see what happens."
                ),
                uiOutput(ns("analysis_code_feedback")),
                uiOutput(ns("analysis_step2_quiz_feedback"))
              ),
              column(
                8,
                editor_module_ui(ns("step1_editor"))
              )
            )
          ),
          shinydashboard::box(
            title = "Step 2: Prepare Data",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            fluidRow(
              column(
                4,
                markdown(
                  "Next, let's average out the technical replicates for each student. Try the following code:"
                ),
                wellPanel(
                  markdown("
                  ```
                  average_trs <- data %>%
                    group_by(id, stress_status) %>%
                    summarise(average_blinks_per_minute = mean(blinks_per_minute, na.rm = TRUE), .groups = 'drop')
                  ```")
                ),
                uiOutput(ns("analysis_code_feedback2")),
                uiOutput(ns("analysis_step3_quiz_feedback"))
              ),
              column(
                8,
                editor_module_ui(ns("step2_editor"))
              )
            )
          ),
          box(
            title = "Step 3: Statistical Preparation",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            fluidRow(
              column(
                3,
                markdown(
                  "We need to first test our assumptions, before we run a t-test. Let's run through a checklist. Tick off each assumption as we go along!"
                ),
                HTML('<div class="yellow-box">
                        <p>These are standard assumptions for parametric tests, and are listed in decreasing order of importance.</p>
                        <ol>
                          <li>Random sampling: samples are randomly sampled from a population to prevent bias.</li>
                          <li>Independence: Independently obtained to avoid pseudo-replication.</li>
                          <li>Homogeneity of Variances (no outliers): is the variation amongst the groups within the same range?</li>
                          <li>Normality within each group: do the groups fall in a normal distribution, or is the data skewed?</li>
                        </ol>
                      </div>'),
                checkboxGroupInput(ns("assumptions_checklist"),
                              label = "Assumptions Checklist",
                              choices = c("Random sampling" = "random",
                                          "Independence" = "indep",
                                          "Homogeneity of Variances" = "homvar",
                                          "Normality" = "norm")
                              ),
                uiOutput(ns("assumptions_feedback"))
              ),
              column(
                3,
                actionButton(ns("run_qq_Plot"), "Generate Q-Q Plot"),
                plotOutput(ns("q_q_plot")),
                #uiOutput(ns("download_qq")),
                box(
                  title = "View code used to generate plot",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  width = 12,
                  status = "info",
                  wellPanel(
                  markdown("
                           ```
                           variance_qq_plot <- qqPlot(average_trs$average_blinks_per_minute,
                           main = \"Q-Q Plot of Average Blinks/Minute\",
                           xlab = \"Theoretical Quantiles\",
                           ylab = \"Sample Quantiles\",
                           col = \"blue\",
                           pch = 20)
                           ```
                    ")
                  )
                ),
                align = "center",
              ),
              column(3,
               actionButton(ns("run_box_Plot"), "Generate Boxplot"),
               plotOutput(ns("box_plot")),
               box(
                 title = "View code used to generate plot",
                 collapsible = TRUE,
                 collapsed = TRUE,
                 width = 12,
                 status = "info",
                 wellPanel(
                   markdown("
                           ```
                           average_trs$stress_status <- factor(average_trs$stress_status, levels = c(\"unstressed\", \"stressed\"))
                          
                          variance_boxplot <- boxplot(average_blinks_per_minute ~ stress_status, 
                                                      data = average_trs,
                                                      xlab = \"Stress Status\",
                                                      ylab = \"Blinks Per Minute\",
                                                      main = \"Variance: Blinks/Minute by Stress Status\",
                                                      col = c(\"grey49\", \"lightgrey\"))  
                          variance_boxplot <- stripchart(average_blinks_per_minute ~ stress_status, 
                                     data = average_trs,
                                     add = TRUE, 
                                     vertical = TRUE, 
                                     method = \"jitter\", 
                                     pch = 21, 
                                     bg = \"maroon\")
                           ```
                    ")
                 )
               ),
               align = "center"
                     ),
              column(3,
               actionButton(ns("run_hist_Plot"), "Generate Histogram"),
               plotOutput(ns("hist_plot")),
               box(
                 title = "View code used to generate plot",
                 collapsible = TRUE,
                 collapsed = TRUE,
                 width = 12,
                 status = "info",
                 wellPanel(
                   markdown("
                           ```
                           hist <- hist(average_trs$average_blinks_per_minute,
                           main = \"Distribution of Blinks/Minute\",
                           xlab = \"Average Blinks/Minute\",
                           ylab = \"Frequency\",
                           col = \"grey49\",
                           border = \"black\")
                           ```
                    ")
                 )
               ),
               align = "center"
              ),
            )
          ),
          box(
            title = "Step 4: Analyse the Data",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            fluidRow(
              column(
                4,
                markdown(
                  "Finally, let\'s analyse the data!"
                ),
                radioButtons(ns("step_4_t_test_type_quiz"),
                             label = "What type of t-test should we use?",
                             choices = c("one-sample" = "one",
                                         "two-sample" = "two",
                                         "paired" = "paired"),
                             selected = character(0)),
                uiOutput(ns("step_4_t_test_type_quiz_feedback")),
                markdown("Now let's run this code:"),
                wellPanel(
                  markdown("
                  ```
                  t_test <- t.test(average_blinks_per_minute ~ stress_status, var.equal = TRUE, data = average_trs)

                  ```")
                ),
                uiOutput(ns("t_test_feedback")),
              ),
              column(
                8,
                editor_module_ui(ns("step4_editor"))
              )
            )
          ),
          box(
            title = "Step 5: Creating a Figure",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            fluidRow(
              column(
                4,
                markdown(
                  "Time to make a figure!"
                ),
                radioButtons(ns("step5_figure_type_quiz"),
                             label = "What type of figure would be best here?",
                             choices = c("bar chart" = "bar",
                                         "line graph" = "line",
                                         "scatter plot" = "scatter"),
                             selected = character(0)),
                uiOutput(ns("step5_figure_type_quiz_feedback")),
                box(
                  title = "View code used to generate plot",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  width = 12,
                  status = "success",
                  wellPanel(
                    markdown("
                          First, we need to summarise our data before we plot it:
                           ```
                           data_summary <- average_trs %>%
                              group_by(stress_status) %>%
                              summarise(
                                n = n(),
                                mean = mean(average_blinks_per_minute, na.rm = TRUE),
                                sd = sd(average_blinks_per_minute, na.rm = TRUE),
                                sem = sd / sqrt(n)
                              )
                            
                            data_summary$stress_status <- factor(data_summary$stress_status, levels = c(\"unstressed\", \"stressed\"))

                           ```
                           
                           Next, we can create our plot:
                           ```
                           barplot <- ggplot(data_summary, aes(x = stress_status, y = mean, fill = stress_status)) + 
                          geom_bar(stat = \"identity\", color = \"black\", position = position_dodge()) +
                          geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .2, position = position_dodge(.9)) +
                          scale_fill_manual(values = c(\"unstressed\" = \"grey49\", \"stressed\" = \"lightgrey\")) + 
                          labs(x = \"Stress Status\",
                               y = \"Mean Blinks/Minute\",
                               title = \"Mean Blinks/Minute by Stress Status\") +
                          theme_minimal() +
                          theme(legend.position = \"none\") +
                          ylim(0, max(data_summary$mean + data_summary$sem) * 1.2)
                          ```
                    ")
                  )
                ),
              ),
              column(
                4,
                editor_module_ui(ns("step5_editor"))
              ),
              column(
                4,
                plotOutput(ns("results_plot")),
              )
            )
          ),
        )
      )
    )
  )
}



analysis_module_server <- function(id, results_data) {
  moduleServer(id, function(input, output, session) {
    # Load data
    #data_read <- read.csv("/Users/Danny_1/GitHub/BlinkR/BlinkR_app/data/dummy_blinking_data.csv")

    data <- reactive({ data_read })
    
    # Step 1: View Data
    step1_result <- editor_module_server("step1_editor", data = data)
    
    observe({
      if (is.data.frame(step1_result())) {
        output$analysis_code_feedback <- renderUI({
          radioButtons(
            session$ns("analysis_step2_quiz"), 
            label = HTML('<div class="success-box">\U1F44D That\'s our data! Looks good! What do you think the next step in our analysis is?</div>'), 
            choices = list(
              "Get average blinks/minute for each condition?" = "option1", 
              "Get average from technical replicates for each subject?" = "option2", 
              "Do statistical analysis?" = "option3"
            ),
            selected = character(0)
          )
        })
      }
    })
    
    observeEvent(input$analysis_step2_quiz, {
      feedback <- if (input$analysis_step2_quiz == "option2") {
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
      
      output$analysis_step2_quiz_feedback <- renderUI({
        feedback
      })
    })
    
    # Step 2: Pre-Process Data
    step2_result <- editor_module_server("step2_editor", data = data)
    
    observe({
      if (is.data.frame(step2_result())) {
        output$analysis_code_feedback2 <- renderUI({
          radioButtons(
            session$ns("analysis_step3_quiz"), 
            label = HTML('<div class="success-box">\U1F44D Great Job! Now, before we dive in what do we need to do next?</div>'), 
            choices = list(
              "Check our assumptions for the t-test?" = "option1", 
              "Run a t-test?" = "option2", 
              "Create a figure?" = "option3"
            ),
            selected = character(0)
          )
        })
      }
    })
    
    observeEvent(input$analysis_step3_quiz, {
      feedback <- if (input$analysis_step3_quiz == "option1") {
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
      
      output$analysis_step3_quiz_feedback <- renderUI({
        feedback
      })
    })

    #Step 3: Check Assumptions
    observeEvent(input$assumptions_checklist, {
      feedback <- NULL 
      
      if ("homvar" %in% input$assumptions_checklist) {
        feedback <- div(class = "error-box", "\U1F914 We don't know yet. Generate the plots to find out!")
      }
      if ("norm" %in% input$assumptions_checklist) {
        feedback <- div(class = "error-box", "\U1F914 We don't know yet. Generate the plots to find out!")
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
      dplyr::summarise(average_blinks_per_minute = mean(blinks_per_minute, na.rm = TRUE), .groups = 'drop')
    
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
    })

    
    average_trs_assumptions$stress_status <- factor(average_trs_assumptions$stress_status, levels = c("unstressed", "stressed"))

    observeEvent(input$run_box_Plot, {
      output$box_plot <- renderPlot({
        boxplot(average_blinks_per_minute ~ stress_status, 
                                data = average_trs_assumptions,
                                xlab = "Stress Status",
                                ylab = "Blinks Per Minute",
                                main = "Variance: Blinks/Minute by Stress Status",
                                col = c("grey49", "lightgrey"))  
        stripchart(average_blinks_per_minute ~ stress_status, 
                                   data = average_trs_assumptions,
                                   add = TRUE, 
                                   vertical = TRUE, 
                                   method = "jitter", 
                                   pch = 21, 
                                   bg = "maroon")
        
      })
    })
    
    
    observeEvent(input$run_hist_Plot, {
      output$hist_plot <- renderPlot({
        hist(average_trs_assumptions$average_blinks_per_minute,
             main = "Distribution of Blinks/Minute",
             xlab = "Average Blinks/Minute",
             ylab = "Frequency",
             col = "grey49",
             border = "black")
        
      })
    })
    
    #Step 4: Run T-Test
    
    observeEvent(input$step_4_t_test_type_quiz, {
      feedback <- if (input$step_4_t_test_type_quiz == "two") {
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
      
      output$step_4_t_test_type_quiz_feedback <- renderUI({
        feedback
      })
    })
    
    
    average_trs <- reactive({
      as.data.frame(average_trs_assumptions)
      })

    step4_result <- editor_module_server("step4_editor", data = average_trs)
    
    
    observe({
      req(step4_result())
      if (inherits(step4_result(), "htest")) {
        output$t_test_feedback <- renderUI({
          div(class = "success-box", "\U1F64C Correct!")
        })
      } else {
        output$t_test_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
      }
    })
    
      
    #Step 5: Make Figure
    observeEvent(input$step5_figure_type_quiz, {
      feedback <- if (input$step5_figure_type_quiz == "bar") {
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
      
      output$step5_figure_type_quiz_feedback <- renderUI({
        feedback
      })
    })
    
    
    step5_result <- editor_module_server("step5_editor", data = average_trs)
    
    observeEvent(step5_result(), {
      output$results_plot <- renderPlot({
        req(step5_result())
        plot <- step5_result()
        x
        if (inherits(plot, "ggplot")) {
          print(plot)
        } else if (is.function(plot)) {
          plot()
        } else {
          plot.new()
          text(0.5, 0.5, "No valid plot returned.", cex = 1.5)
        }
      })
    })

  })
}



