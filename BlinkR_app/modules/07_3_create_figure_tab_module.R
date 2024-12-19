analysis_create_figure_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Create_Figure",
    fluidPage(
      fluidRow(
        column(
          12,
          
          box(
            title = "Creating a Figure",
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            solidHeader = TRUE,
            fluidRow(
              column(
                4,
                markdown(
                  "Time to make a figure!"
                ),
                radioButtons(ns("figure_type_selector"),
                             label = "What type of figure would be best here?",
                             choices = c("bar chart" = "bar",
                                         "box plot" = "box"
                                         ),
                             selected = character(0)),
                uiOutput(ns("figure_type_selector_output")),
                uiOutput(ns("figure_editor_feedback"))
              ),
              column(
                8,
                uiOutput(ns("editor_ui")),
                uiOutput(ns("save_plot"))
                
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
                ns("summarise"),
                label = tagList(icon("rectangle-list"), "Summarise the Data"),
                class = "action-button custom-action",
                `data-id` = "summarise_data"
              ),
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
                label = tagList(icon("dashboard"), "Go to Dashboard"),
                class = "action-button custom-dark-yellow"
              )
            )
          )

))))

}

analysis_create_figure_module_server <- function(id, results_data, parent.session, saved_results) {
  moduleServer(id, function(input, output, session) {
    # Load data
  data_read <- read.csv(here("BlinkR_app", "data","dummy_blinking_data.csv"))
  
  data <- reactive({ data_read })

  average_trs <- data_read %>%
    dplyr::group_by(id, stress_status) %>%
    dplyr::summarise(
      average_blinks_per_minute = mean(blinks_per_minute, na.rm = TRUE),
      .groups = 'drop'
    )
  #Make Figure

  observeEvent(input$figure_type_selector, {
    figure_type_selector_output <- if (input$figure_type_selector == "bar") {
      includeMarkdown(here("BlinkR_app", "markdown","analysis_create_figure_barplot.Rmd"))
    } else {
      includeMarkdown(here("BlinkR_app", "markdown","analysis_create_figure_boxplot.Rmd"))
    }
    
    output$figure_type_selector_output <- renderUI({
      figure_type_selector_output
    })
    
    output$figure_editor_feedback <- renderUI({
      NULL
    })
    
    output$save_plot <- renderUI({
      NULL
    })
    
    output$editor_ui <- renderUI({
      if (input$figure_type_selector == "bar") {
        editor_module_ui(session$ns("figure_editor_bar_plot"))
        
      } else {
        editor_module_ui(session$ns("figure_editor_box_plot"))
    
      }
    })
})


  figure_editor_bar_plot <- editor_module_server("figure_editor_bar_plot", data = average_trs)
  figure_editor_box_plot <- editor_module_server("figure_editor_box_plot", data = average_trs)
  

#bar plot
  observe({
    req(!is.null(figure_editor_bar_plot()))
    if (inherits(barplot, "ggplot")) {
      output$figure_editor_feedback <- renderUI({
        tagList(
          div(class = "success-box", "\U1F64C Great Job!"),
          includeMarkdown(here("BlinkR_app", "markdown","analysis_figure_editing_colours.Rmd")),
          box(title = "Open me for a hint",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 12,
              status = "info",
              markdown("Look for existing colour names in the code!")
              )
        )
      })
      
      output$save_plot <- renderUI({
        actionButton(
          session$ns("save_bar_plot"),
          label = tagList(icon("save"), "Save Plot to Dashboard"),
          class = "action-button custom-action"
        )
      })
    } else {
      output$figure_editor_feedback <- renderUI({
        div(class = "error-box", "\U1F914 Not quite - try again!")
    })
      output$save_plot <- renderUI({
        NULL
      })
  }
  })
  
#boxplot
  observe({
    req(!is.null(figure_editor_box_plot()))
    if (inherits(boxplot, "ggplot")) {
      output$figure_editor_feedback <- renderUI({
        tagList(
          div(class = "success-box", "\U1F64C Great Job!"),
          includeMarkdown(here("BlinkR_app", "markdown","analysis_figure_editing_colours.Rmd")),
          box(title = "Open me for a hint",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 12,
              status = "info",
              markdown("Look for existing colour names in the code!")
          ),
        )
      })
  
      output$save_plot <- renderUI({
        actionButton(
          session$ns("save_box_plot"),
          label = tagList(icon("save"), "Save Plot to Dashboard"),
          class = "action-button custom-action"
        )
      })
    } else {
      output$figure_editor_feedback <- renderUI({
        div(class = "error-box", "\U1F914 Not quite - try again!")
      })
      output$save_plot <- renderUI({
        NULL
      })
    }
  })
  
  #bar plot save
  observeEvent(input$save_bar_plot, {
    if (!is.null(figure_editor_bar_plot())) {
      if (inherits(figure_editor_bar_plot(), "ggplot")) {
        key <- "bar_plot"
        saved_results$plots[["bar_plot"]] <- NULL
        saved_results$plots[["box_plot"]] <- NULL
        
        saved_results$plots[[key]] <- figure_editor_bar_plot()
        
      } else {
        saved_results$plots[[key]] <- recordPlot()
      }

      showNotification("Plot saved successfully. Previous save has been overwritten.", type = "message")
    } else {
      showNotification("No plot to save. Please create a plot first.", type = "error")
    }
  })
  
  # box plot save
  observeEvent(input$save_box_plot, {
 
    if (!is.null(figure_editor_box_plot())) {
      if (inherits(figure_editor_box_plot(), "ggplot")) {
        key <- "box_plot"
        saved_results$plots[["bar_plot"]] <- NULL
        saved_results$plots[["box_plot"]] <- NULL
        
        saved_results$plots[[key]] <- figure_editor_box_plot()
        
      } else {
        saved_results$plots[[key]] <- recordPlot()
      }
      
      showNotification("Plot saved successfully. Previous save has been overwritten.", type = "message")
    } else {
      showNotification("No plot to save. Please create a plot first.", type = "error")
    }
  })
  
  
  
  
  observeEvent(input$summarise, {
    updateTabItems(parent.session, "sidebar", "Summarise_Data")
  })
  observeEvent(input$statistics, {
    updateTabItems(parent.session, "sidebar", "Statistical_Analysis")
  })
  
# go to dashboard button
observeEvent(input$dashboard, {
  updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
})


  }
  )
}