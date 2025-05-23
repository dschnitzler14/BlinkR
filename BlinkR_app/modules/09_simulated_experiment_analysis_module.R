simulated_experiment_analysis_module_ui <- function(id, i18n) {
    ns <- NS(id)
    simulated_experiment_analysis <- tabItem(tabName = "Simulated_Experiment_Analysis",
                              fluidPage(
                                fluidRow(
                                        column(
                                          width = 12,
                                          div(
                                            class = "page-title-box",
                                            tags$h2(
                                              tagList(shiny::icon("dashboard"), i18n$t("Simulated Experiment: Analysis"))
                                            )
                                  ),
                                  div(
                                        class = "yellow-box",
                                          tagList(i18n$t("Remember, this is a simulated experiment. The data you see here is not real."))
                                      ),
                                )),
                                fluidRow(
                                  box(
                                    title = tagList(shiny::icon("dashboard"), i18n$t("Analysing the Data")),
                                    id = "simulated_analysis1",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("sim_exp_analysis_box1_markdown"))
                                  ),
                                  box(
                                    title = tagList(shiny::icon("magnifying-glass"), i18n$t("Prepare the Data")),
                                    id = "simulated_analysis2",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    fluidRow(
                                      column(6,
                                      markdown(i18n$t("First, let's prepare the data by generating an average of each technical replicate.")),
                                      uiOutput(ns("average_trs_caf_feedback")),
                                      ),
                                      column(6,
                                      editor_module_ui(ns("average_trs_caf_editor"), i18n)
                                      )
                                    )
                                  ),
                                  box(
                                    title = tagList(shiny::icon("rectangle-list"), i18n$t("Summarising the Data")),
                                    id = "simulated_analysis3",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    fluidRow(
                                      column(6,
                                      markdown(i18n$t("Now, let's work out the summary statistics")),
                                      uiOutput(ns("summarise_caf_feedback")),
                                      ),
                                      column(6,
                                      editor_module_ui(ns("summarise_caf_editor"), i18n)
                                      )
                                    )
                                    
                                  ),
                                  box(
                                    title = tagList(shiny::icon("chart-simple"), i18n$t("Creating a Figure")),
                                    id = "simulated_analysis4",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    fluidRow(
                                      column(6,
                                      markdown(i18n$t("In this next step, we will create a figure")),
                                      uiOutput(ns("caf_figure_editor_feedback"))
                                      ),
                                      column(6,
                                      editor_module_ui(ns("figure_caf_editor"), i18n)

                                      )
                                    )
                                  ),
                                  box(
                                    title = tagList(shiny::icon("equals"), i18n$t("Statistical Analysis")),
                                    id = "simulated_analysis5",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    fluidRow(
                                      column(6,
                                      markdown(i18n$t("In order to decide on which statistical test we will carry out, let's check if our data is normally distributed.")),
                                       actionButton(ns("caf_run_hist_Plot"), 
                                          tagList(shiny::icon("circle-plus"), i18n$t("Generate Histogram to check for Normality")),
                                          class = "fun-generate-button")
                                        ),
                                      column(6,
                                      plotOutput(ns("caf_hist_plot"))
                                      )
                                    ),
                                    fluidRow(
                                      column(6, 
                                      markdown(i18n$t("Now, we can run the statistical analysis.")),
                                       uiOutput(ns("caf_t_test_feedback"))
                                      ),
                                      column(6,
                                      editor_module_ui(ns("caf_t_test_editor"), i18n),
                                      )
                                    ),
                                    fluidRow(
                                      column(6, 
                                      markdown(i18n$t("Finally, let's calculate the effect size.")),
                                       uiOutput(ns("caf_effect_size_feedback"))
                                      ),
                                      column(6,
                                      editor_module_ui(ns("caf_effect_size_editor"), i18n),
                                      )
                                    )
                                  ),
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
        ns("back_page_raw_data"),
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_writing_up"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
                              )
    )
    }
 
simulated_experiment_analysis_module_server <- function(id, i18n, caf_data_read, parent.session, include_markdown_language) {
  moduleServer(id, function(input, output, session) {

output$sim_exp_analysis_box1_markdown <- renderUI({
  include_markdown_language("09_simulated_experiment/sim_exp_analysis_box1.Rmd")
})

  caf_data <- reactive({ NULL })

  caf_data_df <- as.data.frame(caf_data_read)

  caf_data <- reactive({ caf_data_df })

  average_trs_caf <- reactive({ NULL })

  average_trs_caf_results <- caf_data() %>%
          group_by(ID, Caffeine_Status) %>%
          summarise(Average_HR = mean(HR, na.rm = TRUE), .groups = 'drop')

  average_trs_caf <- reactive({ average_trs_caf_results })

  caf_summary <- reactive({ NULL })

  caf_summary_results <- average_trs_caf() %>%
    group_by(Caffeine_Status) %>%
    summarise(
      n = n(),
      mean = mean(Average_HR, na.rm = TRUE),
      sd = sd(Average_HR, na.rm = TRUE),
      sem = sd / sqrt(n)
    )

  caf_summary <- reactive({ caf_summary_results })

# Step1: Prepare Data 
    predefined_code_prepare_caf_data <- read_file("markdown/09_simulated_experiment/sim_analysis_prepare_data.Rmd")
      
    prepare_caf_data_result <- editor_module_server("average_trs_caf_editor", data = caf_data, variable_name = "caf_data", predefined_code = predefined_code_prepare_caf_data, return_type = "result", session_folder_id, save_header = "", code_history = FALSE)
    
observe({
      req(!is.null(prepare_caf_data_result()), !is.null(prepare_caf_data_result()$result))
      feedback <- if (is.data.frame(prepare_caf_data_result()$result) && nrow(prepare_caf_data_result()$result) > 0) {
          tagList(
            div(class = "success-box", i18n$t("🙌 Good Job!")),
          )
      
      } else if (!is.null(prepare_caf_data_result())) {
        div(class = "error-box", i18n$t("🤔 Not quite - try again!"))
      } else {
        NULL
      }
      
      output$average_trs_caf_feedback <- renderUI({
        feedback
    })
    
    })

# Step2: Summarise Data

  predefined_code_summarise_caf_data <- read_file("markdown/09_simulated_experiment/sim_analysis_summarise.Rmd")
      
  summarise_caf_data_result <- editor_module_server("summarise_caf_editor", data = average_trs_caf, variable_name = "average_trs_caf", predefined_code = predefined_code_summarise_caf_data, return_type = "result", session_folder_id, save_header = "", code_history = FALSE)
  
  observe({
  req(!is.null(summarise_caf_data_result()), !is.null(summarise_caf_data_result()$result))
  if (tibble::is_tibble(summarise_caf_data_result()$result)) {
    output$summarise_caf_feedback <- renderUI({
      tagList(
        div(class = "success-box", i18n$t("🙌 Great!")),
      )
    })
  } else {
    
    output$summarise_caf_feedback <- renderUI({
      div(class = "error-box", i18n$t("🤔 Not quite - try again!"))
    })
  }
})


# Step3: Figure
  predefined_code_fig_caf_data <- read_file("markdown/09_simulated_experiment/sim_analysis_fig.Rmd")
      
  fig_caf_data_result <- editor_module_server("figure_caf_editor", data = list(average_trs_caf, caf_summary), variable_name = c("average_trs_caf", "caf_summary"), predefined_code = predefined_code_fig_caf_data, return_type = "result", session_folder_id, save_header = "", code_history = FALSE)


  observe({
  req(!is.null(fig_caf_data_result()), !is.null(fig_caf_data_result()$result))

  if (isTRUE(fig_caf_data_result()$is_plot) && inherits(fig_caf_data_result()$result, "ggplot")) {
      output$caf_figure_editor_feedback <- renderUI({
        tagList(
          div(class = "success-box", i18n$t("🙌 Great Job!")),
        )
      })
  
    } else {
      output$caf_figure_editor_feedback <- renderUI({
        div(class = "error-box", i18n$t("🤔 Not quite - try again!"))
      })
      
    }
  })
  

# Step4: Stats
# Histogram

hist_plot_reactive <- eventReactive(input$caf_run_hist_Plot, {
  req(average_trs_caf())
  
  hist(average_trs_caf()$Average_HR,
             main = i18n$t("Distribution of HR"),
             xlab = i18n$t("Average HR/Minute"),
             ylab = i18n$t("Frequency"),
             col = "grey49",
             border = "black")
  
  recorded <- recordPlot()
  recorded
})

observeEvent(input$caf_run_hist_Plot, {

      output$caf_hist_plot <- renderPlot({
      replayPlot(hist_plot_reactive())
    })


  })

  # t-test

  predefined_code_caf_t_test <- read_file("markdown/09_simulated_experiment/sim_analysis_t_test.Rmd")
  
  caf_t_test_result <- editor_module_server("caf_t_test_editor", average_trs_caf, "average_trs_caf", predefined_code = predefined_code_caf_t_test, return_type = "result", session_folder_id, save_header = "", code_history = FALSE)

observe({
      req(!is.null(caf_t_test_result()), !is.null(caf_t_test_result()$result))

      if (inherits(caf_t_test_result()$result, "htest")) {
        output$caf_t_test_feedback <- renderUI({
          tagList(
            div(class = "success-box", i18n$t("🙌 Great!")),
          )
        })
        } else {
        output$caf_t_test_feedback <- renderUI({
          div(class = "error-box", i18n$t("🤔 Not quite - try again!"))
        })
        }
      })

# effectsize

predefined_code_caf_effect_size <- read_file("markdown/09_simulated_experiment/sim_analysis_effect_size.Rmd")

caf_effect_size_result <- editor_module_server("caf_effect_size_editor", average_trs_caf, "average_trs_caf", predefined_code = predefined_code_caf_effect_size, return_type = "result", session_folder_id, save_header = "", code_history = FALSE)

observe({
      req(!is.null(caf_effect_size_result()), !is.null(caf_effect_size_result()$result))

      if (!is.null(caf_effect_size_result())) {
        output$caf_effect_size_feedback <- renderUI({
          tagList(
            div(class = "success-box", i18n$t("🙌 Great!")),
          )
        })
        } else {
        output$caf_effect_size_feedback <- renderUI({
          div(class = "error-box", i18n$t("🤔 Not quite - try again!"))
        })
        }
      })

observeEvent(input$back_page_raw_data, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Raw_Data")
      })
      observeEvent(input$next_page_writing_up, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Writing_Up")
      })

}
  )
}