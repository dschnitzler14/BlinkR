analysis_create_figure_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Create_Figure",
    fluidPage(
      fluidRow(
        column(
          12,
          
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
                                         "box plot" = "box",
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
          )
))))

}

analysis_create_figure_module_server <- function(id, results_data, parent.session) {
  moduleServer(id, function(input, output, session) {
    # Load data
    data_read <- read.csv("/Users/Danny_1/GitHub/BlinkR/BlinkR_app/data/dummy_blinking_data.csv")
    
    data <- reactive({ data_read })

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



  }
  )
}