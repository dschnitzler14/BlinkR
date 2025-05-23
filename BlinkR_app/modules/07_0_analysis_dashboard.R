analysis_dashboard_module_ui <- function(id, i18n) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Analysis_Dashboard",
    fluidPage(
      fluidRow(
            column(
              width = 12,
              div(
                class = "page-title-box",
                tags$h2(
                  tagList(shiny::icon("dashboard"), i18n$t("Analysis Dashboard"))
                )
      )
    )),
      fluidRow(
        column(
          12,
          box(
            title = tagList(shiny::icon("magnifying-glass"), i18n$t("Step 1: Prepare Your Data")),
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            solidHeader = TRUE,
            column(12,
            div(
                style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
                   actionButton(ns("prepare_data"), 
                                label = tagList(icon("rectangle-list"), i18n$t("Prepare the Data")), 
                                class = "action-button custom-action",
                                `data-id` = "prepare_data")
                   ))
          ),
          box(
            title = tagList(shiny::icon("rectangle-list"), i18n$t("Step 2: Summarise Data")),
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            solidHeader = TRUE,
            fluidRow(
            column(12,
            div(
                style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
            actionButton(ns("summarise"), 
                                label = tagList(icon("rectangle-list"), i18n$t("Summarise the Data")), 
                                class = "action-button custom-action",
                                `data-id` = "summarise_data")
            )
            )
            ),
            column(6,
                   
                   ),
            column(6,
                   uiOutput(ns("saved_summary_results"))
                   )
          ),
          box(
            title = tagList(shiny::icon("chart-simple"), i18n$t("Step 3: Figure")),
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            solidHeader = TRUE,
            fluidRow(
            column(12,
            div(
                style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
            actionButton(ns("figure"),
                                label = tagList(icon("chart-simple"), i18n$t("Create a Figure")),
                                class = "action-button custom-action",
                                `data-id` = "create_figure")
            )
            )
            ),
            column(6,
                               
                   ),
            column(6,
                   uiOutput(ns("saved_plot_results"))
            )
          ),
          box(
            title = tagList(shiny::icon("equals"), i18n$t("Step 4: Statistical Analysis")),
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            solidHeader = TRUE,
            fluidRow(
            column(12,
            div(
                style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
            actionButton(ns("statistics"),
                                label = tagList(icon("equals"), i18n$t("Run Statistical Analysis")),
                                class = "action-button custom-action",
                                `data-id` = "stats")
            )
            )
            ),
            column(6,
                   
                    uiOutput(ns("stats_interpretation"))
                   ),
            column(6,
            fluidRow(
              column(12,
                     uiOutput(ns("saved_stats_results")),
                     uiOutput(ns("saved_effect_size_results")),
                     uiOutput(ns("saved_q_q_plot_result")),
                     uiOutput(ns("saved_box_plot_result")),
                     uiOutput(ns("saved_hist_plot_result"))
              )
            ),
            )
            )
        ),
          fluidRow(
            column(
              width = 12,
              div(
                style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
                actionButton(ns("link_to_drive"),
                             label = tagList(icon("google-drive"), i18n$t("View on Google Drive")),
                             class = "btn-primary"
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
        ns("back_page_analysis"),
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_analysis"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
      )
    )
  )
}


analysis_dashboard_module_server <- function(id, i18n, parent.session, saved_results, session_folder_id) {
  moduleServer(id, function(input, output, session) {
       vars <- get_experiment_vars()

        observeEvent(input$back_page_analysis, {
      updateTabItems(parent.session, "sidebar", "Playground")
    })
      observeEvent(input$next_page_analysis, {
      updateTabItems(parent.session, "sidebar", "Prepare_Data")
    })
      
    
    # Summary Results
    
    summary_content_reactive <- reactive({
      req(saved_results$scripts[["summary"]])
      paste(capture.output(saved_results$scripts[["summary"]]$result), collapse = "\n")
    })
    
    output$saved_summary_results <- renderUI({
      req(summary_content_reactive())
      
      tagList(
        verbatimTextOutput(session$ns("saved_summary_script"))
      )
    })
    
    output$saved_summary_script <- renderText({
      summary_content_reactive()
    })
    
   
    # Stats Results
    #assumption plots
    output$saved_q_q_plot_result <- renderUI({
      req(saved_results$recorded_plots[["q_q_plot"]])
      tagList(
        plotOutput(session$ns("saved_q_q_plot"))
      )
    })

    output$saved_q_q_plot <- renderPlot({
      req(saved_results$recorded_plots[["q_q_plot"]])
      replayPlot(saved_results$recorded_plots[["q_q_plot"]])
    })

    output$saved_hist_plot_result <- renderUI({
      req(saved_results$recorded_plots[["hist_plot"]])
      tagList(
        plotOutput(session$ns("saved_hist_plot"))
      )
    })

    output$saved_hist_plot <- renderPlot({
      req(saved_results$recorded_plots[["hist_plot"]])
      replayPlot(saved_results$recorded_plots[["hist_plot"]])
    })

    output$saved_box_plot_result <- renderUI({
      req(saved_results$recorded_plots[["box_plot"]])
      tagList(
        plotOutput(session$ns("saved_box_plot"))
      )
    })

    output$saved_box_plot <- renderPlot({
      req(saved_results$recorded_plots[["box_plot"]])
      replayPlot(saved_results$recorded_plots[["box_plot"]])
    })
    
    
    stats_content_reactive <- reactive({
      req(!is.null(saved_results$scripts[["stats_not_normal_unpaired"]]) || 
        !is.null(saved_results$scripts[["stats_not_normal_paired"]]) ||
        !is.null(saved_results$scripts[["stats_normal_unpaired"]]) ||
        !is.null(saved_results$scripts[["stats_normal_paired"]]) ||
        !is.null(saved_results$scripts[["stats_normal_paired_effect_size"]]) ||
        !is.null(saved_results$scripts[["stats_normal_unpaired_effect_size"]]) ||
        !is.null(saved_results$scripts[["stats_not_normal_paired_effect_size"]]) ||
        !is.null(saved_results$scripts[["stats_not_normal_unpaired_effect_size"]])

      )
      
      if (!is.null(saved_results$scripts[["stats_not_normal_unpaired"]])) {
        paste(capture.output(saved_results$scripts[["stats_not_normal_unpaired"]]$result), collapse = "\n")
      } else if (!is.null(saved_results$scripts[["stats_not_normal_paired"]])) {
        paste(capture.output(saved_results$scripts[["stats_not_normal_paired"]]$result), collapse = "\n")
        } else if (!is.null(saved_results$scripts[["stats_normal_unpaired"]])) {
            paste(capture.output(saved_results$scripts[["stats_normal_unpaired"]]$result), collapse = "\n")
            } else if (!is.null(saved_results$scripts[["stats_normal_paired"]])) {
            paste(capture.output(saved_results$scripts[["stats_normal_paired"]]$result), collapse = "\n")
            } else if (!is.null(saved_results$scripts[["stats_normal_paired_effect_size"]])) {
            paste(capture.output(saved_results$scripts[["stats_normal_paired_effect_size"]]$result), collapse = "\n")
            } else if (!is.null(saved_results$scripts[["stats_normal_unpaired_effect_size"]])) {
            paste(capture.output(saved_results$scripts[["stats_normal_unpaired_effect_size"]]$result), collapse = "\n")
            } else if (!is.null(saved_results$scripts[["stats_not_normal_paired_effect_size"]])) {
            paste(capture.output(saved_results$scripts[["stats_not_normal_paired_effect_size"]]$result), collapse = "\n")
            } else if (!is.null(saved_results$scripts[["stats_not_normal_unpaired_effect_size"]])) {
            paste(capture.output(saved_results$scripts[["stats_not_normal_unpaired_effect_size"]]$result), collapse = "\n")
            } else {
            "No statistical scripts found."
            }
})

stats_content_reactive <- reactive({
      req(!is.null(saved_results$scripts[["stats_not_normal_unpaired"]]) || 
        !is.null(saved_results$scripts[["stats_not_normal_paired"]]) ||
        !is.null(saved_results$scripts[["stats_normal_unpaired"]]) ||
        !is.null(saved_results$scripts[["stats_normal_paired"]])
      )
      
      if (!is.null(saved_results$scripts[["stats_not_normal_unpaired"]])) {
        paste(capture.output(saved_results$scripts[["stats_not_normal_unpaired"]]$result), collapse = "\n")
      } else if (!is.null(saved_results$scripts[["stats_not_normal_paired"]])) {
        paste(capture.output(saved_results$scripts[["stats_not_normal_paired"]]$result), collapse = "\n")
        } else if (!is.null(saved_results$scripts[["stats_normal_unpaired"]])) {
            paste(capture.output(saved_results$scripts[["stats_normal_unpaired"]]$result), collapse = "\n")
            } else if (!is.null(saved_results$scripts[["stats_normal_paired"]])) {
            paste(capture.output(saved_results$scripts[["stats_normal_paired"]]$result), collapse = "\n")
            } else {
            "No statistical scripts found."
            }
})


effect_size_content_reactive <- reactive({
req(!is.null(saved_results$scripts[["stats_normal_paired_effect_size"]]) ||
        !is.null(saved_results$scripts[["stats_normal_unpaired_effect_size"]]) ||
        !is.null(saved_results$scripts[["stats_not_normal_paired_effect_size"]]) ||
        !is.null(saved_results$scripts[["stats_not_normal_unpaired_effect_size"]])

      )
      
      if (!is.null(saved_results$scripts[["stats_normal_paired_effect_size"]])) {
            paste(capture.output(saved_results$scripts[["stats_normal_paired_effect_size"]]$result), collapse = "\n")
            } else if (!is.null(saved_results$scripts[["stats_normal_unpaired_effect_size"]])) {
            paste(capture.output(saved_results$scripts[["stats_normal_unpaired_effect_size"]]$result), collapse = "\n")
            } else if (!is.null(saved_results$scripts[["stats_not_normal_paired_effect_size"]])) {
            paste(capture.output(saved_results$scripts[["stats_not_normal_paired_effect_size"]]$result), collapse = "\n")
            } else if (!is.null(saved_results$scripts[["stats_not_normal_unpaired_effect_size"]])) {
            paste(capture.output(saved_results$scripts[["stats_not_normal_unpaired_effect_size"]]$result), collapse = "\n")
            } else {
            "No effect size scripts found."
            }


})

stats_interpretation_content_reactive <- reactive({
  req(!is.null(saved_results$user_writing[["stats_interpretation_text"]]))

  if(!is.null(saved_results$user_writing[["stats_interpretation_text"]])) {
    paste(capture.output(saved_results$user_writing[["stats_interpretation_text"]]), collapse = "\n")
  } else {
    "No interpretation text found."
  }
})
    
    
    output$saved_stats_results <- renderUI({
      req(stats_content_reactive())
      
      tagList(
        verbatimTextOutput(session$ns("saved_stats_script"))
      )
    })
    
    output$saved_stats_script <- renderText({
      stats_content_reactive()
    })


    output$saved_effect_size_results <- renderUI({
      req(effect_size_content_reactive())
      
      tagList(
        verbatimTextOutput(session$ns("saved_effect_size_script"))
      )
    })
    
    output$saved_effect_size_script <- renderText({
      effect_size_content_reactive()
    })


output$stats_interpretation <- renderUI({
      req(stats_interpretation_content_reactive())
      
      tagList(
        textOutput(session$ns("saved_stats_interpretation"))
      )
    })
    
    output$saved_stats_interpretation <- renderText({
      stats_interpretation_content_reactive()
    })
    
    # Figure
    
    output$saved_plot_results <- renderUI({
      req(saved_results$plots)
      
      plot_keys <- names(saved_results$plots)
      
      tagList(
        lapply(plot_keys, function(key) {
          ns_key <- session$ns(key) 
          box(
            title = paste("Your Plot:"),
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            plotOutput(session$ns(paste0("plot_", key)))
          )
        })
      )
    })
    
    observe({
      req(saved_results$plots)
      
      plot_keys <- names(saved_results$plots)
      
      lapply(plot_keys, function(key) {
        output[[paste0("plot_", key)]] <- renderPlot({
          req(saved_results$plots[[key]])
          plot <- saved_results$plots[[key]]
          if (inherits(plot, "ggplot")) {
            print(plot)
          } else if (inherits(plot, "recordedplot")) {
            replayPlot(plot)
          } else {
            stop("The saved plot is neither a ggplot nor a base plot.")
          }
        })
        
      })
    })
    
    

  observeEvent(input$link_to_drive, {
    req(session_folder_id)
        
    showModal(modalDialog(
      title = "Your Google Drive",
      your_google_drive_module_ui(session$ns("your_drive_module_dashboard"), i18n),
      
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l" 
    ))
    
    your_google_drive_module_server("your_drive_module_dashboard", i18n, session_folder_id)

  })
  
   

    
    observeEvent(input$start, {
      updateTabItems(parent.session, "sidebar", "Prepare_Data")
    })
    observeEvent(input$prepare_data, {
      updateTabItems(parent.session, "sidebar", "Prepare_Data")
    })
    observeEvent(input$summarise, {
      updateTabItems(parent.session, "sidebar", "Summarise_Data")
    })
    observeEvent(input$statistics, {
      updateTabItems(parent.session, "sidebar", "Statistical_Analysis")
    })
    observeEvent(input$figure, {
      updateTabItems(parent.session, "sidebar", "Create_Figure")
    })
    
  })
}