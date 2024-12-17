analysis_dashboard_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Analysis_Dashboard",
    fluidPage(
      fluidRow(
        column(
          12,
            fluidRow(
              column(
                width = 12,
                div(
                  style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
                  actionButton(
                    ns("start"),
                    label = tagList(icon("magnifying-glass"), "Click Here To Get Started!"),
                    class = "action-button custom-dark-yellow"
                  )
              )
            ),
          ),
        
          box(
            title = "Your Results: Summarise Data",
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            solidHeader = TRUE,
            column(6,
                   actionButton(ns("summarise"), 
                                label = tagList(icon("rectangle-list"), "Summarise the Data"), 
                                class = "action-button custom-action",
                                `data-id` = "summarise_data")
                   ),
            column(6,
                   uiOutput(ns("saved_summary_results"))
                   )
          ),
          box(
            title = "Your Results: Statistical Analysis",
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            solidHeader = TRUE,
            column(6,
                   actionButton(ns("statistics"),
                                label = tagList(icon("equals"), "Run Statistical Analysis"),
                                class = "action-button custom-action",
                                `data-id` = "stats"),
                   ),
            column(6,
            fluidRow(
              column(12,
                     # uiOutput(ns("saved_q_q_plot_result")),
                     # uiOutput(ns("saved_box_plot_result")),
                     # uiOutput(ns("saved_hist_plot_result"))
              )
            ),
            fluidRow(
              column(12,
                     uiOutput(ns("saved_stats_results"))
              )
            )
            )
            ),
          box(
            title = "Your Results: Figure",
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            solidHeader = TRUE,
            column(6,
                   actionButton(ns("figure"),
                                label = tagList(icon("chart-simple"), "Create a Figure"),
                                class = "action-button custom-action",
                                `data-id` = "create_figure")            
                   ),
            column(6,
                   uiOutput(ns("saved_plot_results"))
            )
          )
        ),
          fluidRow(
            column(
              width = 12,
              div(
                style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
                download_handler_ui("download_script", "Download R Script")
              )
            ),
          ),
      )
    )
  )
}


analysis_dashboard_module_server <- function(id, parent.session, saved_results) {
  moduleServer(id, function(input, output, session) {
 
    
    source("data/test_analysis.R")
    
    # Summary Results
    
    summary_content_reactive <- reactive({
      req(saved_results$scripts[["summary"]])
      paste(capture.output(saved_results$scripts[["summary"]]), collapse = "\n")
    })
    
    output$saved_summary_results <- renderUI({
      req(summary_content_reactive())
      
      tagList(
        verbatimTextOutput(session$ns("saved_summary_script")),
        download_handler_ui(session$ns("download_summary"), "Download Summary Results")
      )
    })
    
    output$saved_summary_script <- renderText({
      summary_content_reactive()
    })
    
    download_handler_server(
      "download_summary",
      content_reactive = summary_content_reactive,
      filename_generator = function() {
        paste0("summary-results-", Sys.Date(), ".txt")
      },
      type = "text"
    )
    
    
    # Stats Results
    #assumption plots
    # output$saved_q_q_plot_result <- renderUI({
    #   req(saved_results$recorded_plots[["q_q_plot"]])
    #   tagList(
    #     plotOutput(session$ns("saved_q_q_plot"))
    #   )
    # })
    # 
    # output$saved_q_q_plot <- renderPlot({
    #   req(saved_results$recorded_plots[["q_q_plot"]]) 
    #   replayPlot(saved_results$recorded_plots[["q_q_plot"]])  
    # })
    # 
    # output$saved_hist_plot_result <- renderUI({
    #   req(saved_results$recorded_plots[["hist_plot"]])
    #   tagList(
    #     plotOutput(session$ns("saved_hist_plot"))
    #   )
    # })
    # 
    # output$saved_hist_plot <- renderPlot({
    #   req(saved_results$recorded_plots[["hist_plot"]]) 
    #   replayPlot(saved_results$recorded_plots[["hist_plot"]])  
    # })
    # 
    # output$saved_box_plot_result <- renderUI({
    #   req(saved_results$recorded_plots[["box_plot"]])
    #   tagList(
    #     plotOutput(session$ns("saved_box_plot"))
    #   )
    # })
    # 
    # output$saved_box_plot <- renderPlot({
    #   req(saved_results$recorded_plots[["box_plot"]])
    #   replayPlot(saved_results$recorded_plots[["box_plot"]])  
    # })
    # 
   
    #stats results
    output$saved_stats_results <- renderUI({
      req(!is.null(saved_results$scripts[["stats_two_sample"]]) || !is.null(saved_results$scripts[["stats_paired"]]))
      
      tagList(
        verbatimTextOutput(session$ns("saved_stats_script")),
        download_handler_ui(session$ns("download_stats"), "Download Stats Results")
      )
    })
    
    
    stats_content_reactive <- reactive({
      req(!is.null(saved_results$scripts[["stats_two_sample"]]) || !is.null(saved_results$scripts[["stats_paired"]]))
      
      if (!is.null(saved_results$scripts[["stats_two_sample"]])) {
        paste(capture.output(saved_results$scripts[["stats_two_sample"]]), collapse = "\n")
      } else if (!is.null(saved_results$scripts[["stats_paired"]])) {
        paste(capture.output(saved_results$scripts[["stats_paired"]]), collapse = "\n")
      } else {
        "No statistical scripts found."
      }
    })
    
    
    output$saved_stats_results <- renderUI({
      req(stats_content_reactive())
      
      tagList(
        verbatimTextOutput(session$ns("saved_stats_script")),
        download_handler_ui(session$ns("download_stats"), "Download Stats Results")
      )
    })
    
    output$saved_stats_script <- renderText({
      stats_content_reactive()
    })
    
    download_handler_server(
      "download_stats",
      content_reactive = stats_content_reactive,
      filename_generator = function() {
        paste0("stats-results-", Sys.Date(), ".txt")
      },
      type = "text"
    )
    
    
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
            plotOutput(session$ns(paste0("plot_", key))),
            download_handler_ui(ns_key, label = "Download This Plot")
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
        
        download_handler_server(
          key,
          content_reactive = reactive({
            req(saved_results$plots[[key]])
            saved_results$plots[[key]]
          }),
          filename_generator = function() {
            paste0(key, "-plot-", Sys.Date(), ".png")
          },
          type = "plot"
        )
      })
    })
    
    
    download_handler_server(
      "download_script",
      content_reactive = reactive({
        "data/test_analysis.R"
      }),
      filename_generator = function() {
        "test_analysis.R"
      },
      type = "file"
    )
    
    
    observeEvent(input$start, {
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