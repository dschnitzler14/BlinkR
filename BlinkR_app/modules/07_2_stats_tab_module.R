analysis_stats_module_ui <- function(id) {
  ns <- NS(id)

   tabItem(
    tabName = "Statistical_Analysis",
    fluidPage(
        uiOutput(ns("testing_assumptions")),
        uiOutput(ns("normal_output")),
        uiOutput(ns("not_normal_output")),
        uiOutput(ns("not_normal_unpaired_ui")),
        uiOutput(ns("not_normal_paired_ui")),
        uiOutput(ns("normal_unpaired_ui")),
        uiOutput(ns("normal_paired_ui")),
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
          
        )
   )
}

analysis_stats_module_server <- function(id, results_data, parent.session, saved_results, session_folder_id) {
  moduleServer(id, function(input, output, session) {
    
    average_trs <- reactive({
      NULL
    })
    
    average_trs_data <- results_data %>%
      select(-"Group", -"Initials", -"Submission_ID") %>%
      dplyr::group_by(ID, Stress_Status) %>%
      dplyr::summarise(
        Average_Blinks_Per_Minute = mean(Blinks_Per_Minute, na.rm = TRUE),
        .groups = 'drop'
      )
    
    average_trs <- reactive({
      average_trs_data
    })

output$testing_assumptions <- renderUI({
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "testing_assumptions",
              title = "Testing Assumptions",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_hist_plot_explainer.Rmd"),
                  actionButton(session$ns("run_hist_Plot"), "Generate Histogram to check for Normality", class = "fun-submit-button"),
                  plotOutput(session$ns("hist_plot")),
                  ),
                  column(6,
                  uiOutput(session$ns("hist_explainer_ui")),

                  )
              ),
              
            )
          ),
          
        ),
        
      )
    })

observeEvent(input$run_hist_Plot, {
      req(average_trs())
      
      output$hist_plot <- renderPlot({
        hist(
          average_trs()$Average_Blinks_Per_Minute,
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
      tagList(
      includeMarkdown(
          "markdown/07_analysis/analysis_hist_plot_explainer.Rmd"
        ),
        actionButton(session$ns("normal"), "The Data is Normal", class = "fun-submit-button"),
        actionButton(session$ns("not_normal"), "The Data is Not Normal", class = "fun-submit-button")
      )
      })

  })

observeEvent(input$normal, {
    output$not_normal_output <- renderUI({NULL})
    output$not_normal_unpaired_ui <- renderUI({NULL})
    output$not_normal_paired_ui <- renderUI({NULL})
    output$normal_unpaired_ui <- renderUI({NULL})
    output$normal_paired_ui <- renderUI({NULL})

    output$normal_output <- renderUI({
      tagList(
        actionButton(session$ns("unpaired_normal"), "The Data is Not Paired", class = "fun-submit-button"),
        actionButton(session$ns("paired_normal"), "The Data is Paired", class = "fun-submit-button")
        )
    })
})

observeEvent(input$not_normal, {
  output$normal_output <- renderUI({NULL})
  output$not_normal_unpaired_ui <- renderUI({NULL})
  output$not_normal_paired_ui <- renderUI({NULL})
  output$normal_unpaired_ui <- renderUI({NULL})
  output$normal_paired_ui <- renderUI({NULL})

    output$not_normal_output <- renderUI({

      tagList(
        actionButton(session$ns("unpaired_not_normal"), "The Data is Not Paired", class = "fun-submit-button"),
        actionButton(session$ns("paired_not_normal"), "The Data is Paired", class = "fun-submit-button")
      )
    })
})

# not normal unpaired
  predefined_code_not_normal_unpaired = read_file("markdown/07_analysis/predefined_code_wilcoxon_test_unpaired.txt")
  not_normal_unpaired_result <- editor_module_server("not_normal_unpaired", average_trs, "average_trs", predefined_code = predefined_code_not_normal_unpaired, return_type = "result", session_folder_id, save_header = "Statistical Analysis: Not Normal Unpaired")

observeEvent(input$unpaired_not_normal,{
  output$not_normal_paired_ui <- renderUI({NULL})
  output$normal_unpaired_ui <- renderUI({NULL})
  output$normal_paired_ui <- renderUI({NULL})

  output$not_normal_unpaired_ui <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "not_normal_unpaired",
              title = "Not Normal Unpaired: Wilcoxon Test",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_wilcoxon_test_unpaired.Rmd"),
                  uiOutput(session$ns("not_normal_unpaired_feedback"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("not_normal_unpaired"))
                  )
              ),
              
            )
          ),
          
        ),
        
      )
  })
})


observe({
      req(!is.null(not_normal_unpaired_result()), !is.null(not_normal_unpaired_result()$result))

      if (inherits(not_normal_unpaired_result()$result, "htest")) {
        output$not_normal_unpaired_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
          )
        })
        } else {
        output$not_normal_unpaired_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
      })

# not normal paired

  predefined_code_not_normal_paired = read_file("markdown/07_analysis/predefined_code_wilcoxon_test_paired.txt")
  not_normal_paired_result <- editor_module_server("not_normal_paired", average_trs, "average_trs", predefined_code = predefined_code_not_normal_paired, return_type = "result", session_folder_id, save_header = "Statistical Analysis: Not Normal Paired")

observeEvent(input$paired_not_normal,{
  output$not_normal_unpaired_ui <- renderUI({NULL})
  output$normal_unpaired_ui <- renderUI({NULL})
  output$normal_paired_ui <- renderUI({NULL})

  output$not_normal_paired_ui <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "not_normal_paired",
              title = "Not Normal Paired: Wilcoxon Test",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_wilcoxon_test_paired.Rmd"),
                  uiOutput(session$ns("not_normal_paired_feedback"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("not_normal_paired"))
                  )
              ),
              
            )
          ),
          
        ),
        
      )
  })
})

observe({
      req(!is.null(not_normal_paired_result()), !is.null(not_normal_paired_result()$result))

      if (!is.null(not_normal_paired_result()$result)) {
        output$not_normal_paired_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
          )
        })
        } else {
        output$not_normal_paired_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
      })

# normal unpaired

  predefined_code_normal_unpaired = read_file("markdown/07_analysis/predefined_code_two_sided_t_test.txt")
  normal_unpaired_result <- editor_module_server("normal_unpaired", average_trs, "average_trs", predefined_code = predefined_code_normal_unpaired, return_type = "result", session_folder_id, save_header = "Statistical Analysis: Normal Unpaired")

observeEvent(input$unpaired_normal,{
  output$not_normal_unpaired_ui <- renderUI({NULL})
  output$not_normal_paired_ui <- renderUI({NULL})
  output$normal_paired_ui <- renderUI({NULL})

  output$normal_unpaired_ui <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "normal_unpaired",
              title = "Normal Unpaired: T-Test",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_two_sided_t_test.Rmd"),
                  uiOutput(session$ns("normal_unpaired_feedback"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("normal_unpaired"))
                  )
              ),
              
            )
          ),
          
        ),
        
      )
  })
})

observe({
      req(!is.null(normal_unpaired_result()), !is.null(normal_unpaired_result()$result))

      if (!is.null(normal_unpaired_result()$result)) {
        output$normal_unpaired_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
          )
        })
        } else {
        output$normal_unpaired_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
      })

# normal paired

  predefined_code_normal_paired = read_file("markdown/07_analysis/predefined_code_paired_t_test.txt")
  normal_paired_result <- editor_module_server("normal_paired", average_trs, "average_trs", predefined_code = predefined_code_normal_paired, return_type = "result", session_folder_id, save_header = "Statistical Analysis: Normal Paired")

observeEvent(input$paired_normal,{
  output$not_normal_unpaired_ui <- renderUI({NULL})
  output$not_normal_paired_ui <- renderUI({NULL})
  output$normal_unpaired_ui <- renderUI({NULL})

  output$normal_paired_ui <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "normal_paired",
              title = "Normal Paired: T-Test",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_paired_t_test.Rmd"),
                  uiOutput(session$ns("normal_paired_feedback"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("normal_paired"))
                  )
              ),
              
            )
          ),
          
        ),
        
      )
  })
})

observe({
      req(!is.null(normal_paired_result()), !is.null(normal_paired_result()$result))

      if (!is.null(normal_paired_result()$result)) {
        output$normal_paired_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
          )
        })
        } else {
        output$normal_paired_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
      })


})}