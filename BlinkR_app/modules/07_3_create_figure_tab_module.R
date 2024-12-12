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
                uiOutput(ns("editor_ui"))
              )
            )
          ),
          box(
            title = "What do you want to do next?",
            collapsible = FALSE,
            width = 12,
            class = "custom-box",
            actionButton(ns("summarise"), "Summarise the Data", class = "action-button custom-action"),
            actionButton(ns("statistics"), "Run Statistical Analysis", class = "action-button custom-action"),
          ),
          box(
            title = " ",
            collapsible = FALSE,
            width = 12,
            class = "custom-box",
            markdown("View all your results in the Analysis Dashboard"),
            actionButton(ns("dashboard"), "Go to Dashboard", class = "action-button custom-action")
          )
))))

}

analysis_create_figure_module_server <- function(id, results_data, parent.session) {
  moduleServer(id, function(input, output, session) {
    # Load data
  data_read <- read.csv("/Users/Danny_1/GitHub/BlinkR/BlinkR_app/data/dummy_blinking_data.csv")
  
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
      includeMarkdown("markdown/07_analysis/analysis_create_figure_barplot.Rmd")
    } else {
      includeMarkdown("markdown/07_analysis/analysis_create_figure_boxplot.Rmd")
    }
    
    output$figure_type_selector_output <- renderUI({
      figure_type_selector_output
    })
    
    output$figure_editor_feedback <- renderUI({
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
          includeMarkdown("markdown/07_analysis/analysis_figure_editing_colours.Rmd"),
          box(title = "Open me for a hint",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 12,
              status = "info",
              markdown("Look for existing colour names in the code!")
              )
        )
      })
    } else {
      output$figure_editor_feedback <- renderUI({
        div(class = "error-box", "\U1F914 Not quite - try again!")
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
          includeMarkdown("markdown/07_analysis/analysis_figure_editing_colours.Rmd"),
          box(title = "Open me for a hint",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 12,
              status = "info",
              markdown("Look for existing colour names in the code!")
          )
        )
      })
    } else {
      output$figure_editor_feedback <- renderUI({
        div(class = "error-box", "\U1F914 Not quite - try again!")
      })
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