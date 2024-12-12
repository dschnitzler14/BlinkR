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
              ),
              column(
                8,
                uiOutput(ns("editor_ui"))
              )
            )
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

    # output$editor_ui <- renderUI({
    #   NULL
    # })
    
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
  

  
  
  
# go to dashboard button
observeEvent(input$dashboard, {
  updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
})


  }
  )
}