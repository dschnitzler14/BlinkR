analysis_prepare_data_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Prepare_Data",
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
                uiOutput(ns("view_data_code_feedback")),
                uiOutput(ns("view_data_quiz_feedback"))
              ),
              column(
                8,
                editor_module_ui(ns("view_data_editor"))
              )
            )
          ),
          shinydashboard::box(
            title = "Step 2: Prepare Data",
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            fluidRow(
              column(
                4,
                markdown(
                  "Time to average out the technical replicates for each student. Try the following code:"
                ),
                wellPanel(
                  markdown("
                  ```
                  average_trs <- data %>%
                    group_by(id, stress_status) %>%
                    summarise(average_blinks_per_minute = mean(blinks_per_minute, na.rm = TRUE), .groups = 'drop')
                  ```")
                ),
                uiOutput(ns("average_technical_replicates_code_result")),
                #uiOutput(ns("average_technical_replicates_quiz_feedback"))
              ),
              column(
                8,
                editor_module_ui(ns("average_trs_editor"))
              )
            )
          ),
          box(
            title = "What do you want to do next?",
            collapsible = FALSE,
            width = 12,
            class = "custom-box",
            actionButton(ns("summarise"),
                         label = tagList(icon("rectangle-list"), "Summarise the Data"),
                         class = "action-button custom-action"),
            actionButton(ns("statistics"),
                         label = tagList(icon("equals"), "Run Statistical Analysis"),
                         class = "action-button custom-action"),
            actionButton(ns("figure"),
                         label = tagList(icon("chart-simple"), "Create a Figure"),
                         class = "action-button custom-action")
          ),
          box(
            title = "",
            collapsible = FALSE,
            width = 12,
            class = "custom-box",
            actionButton(ns("dashboard"),
                         label = tagList(icon("dashboard"), "Go to Dashboard"),
                         class = "action-button custom-action")
            ),
        )
      )
    )
  )
}


analysis_prepare_data_module_server <- function(id, results_data, parent.session) {
  moduleServer(id, function(input, output, session) {
    # Load data
    data_read <- read.csv("/Users/Danny_1/GitHub/BlinkR/BlinkR_app/data/dummy_blinking_data.csv")

    data <- reactive({ data_read })
    
    # Step 1: View Data
    view_data_result <- editor_module_server("view_data_editor", data = data)
    
    observe({
      feedback <- if (is.data.frame(view_data_result()) && nrow(view_data_result()) > 0) {
          tagList(
            div(class = "success-box", "\U1F64C That's our data! Looks good!"),
            markdown("
        Well done! You just ran your first bit of code!
        The `head()` command returns the first part of our table, so we can confirm that our data has loaded.
        Take a look at the column names - they will be useful for the next step!
        **The next step is turning this data into something that we can use for analysis.**
        "),
            radioButtons(
              session$ns("analysis_step2_quiz"), 
              label = "What do you think we need to do to our data?", 
              choices = list(
                "Get average blinks/minute for each condition?" = "option1", 
                "Get average from technical replicates for each subject?" = "option2", 
                "Do statistical analysis?" = "option3"
              ),
              selected = character(0)
            )
          )
      
      } else if (!is.null(view_data_result())) {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      } else {
        NULL
      }
      
      output$view_data_code_feedback <- renderUI({
        feedback
    })
    
    })
    
    observeEvent(input$analysis_step2_quiz, {
      feedback <- if (input$analysis_step2_quiz == "option2") {
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
      
      output$view_data_quiz_feedback <- renderUI({
        feedback
      })
    })
    
    # Step 2: Pre-Process Data
    average_trs_result <- editor_module_server("average_trs_editor", data = data)
    
    observe({
      feedback <- if (is.data.frame(average_trs_result()) && nrow(average_trs_result()) > 0) {
        tagList(
          div(class = "success-box", "\U1F64C Good Job!"),
          includeMarkdown("markdown/07_analysis/analysis_home_prepare_data.Rmd")
        )
      } else if (!is.null(average_trs_result())) {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      } else {
        NULL
      }
      
      output$average_technical_replicates_code_result <- renderUI({
        feedback
      })
    })
    
    
    ## next step buttons
    observeEvent(input$summarise, {
      updateTabItems(parent.session, "sidebar", "Summarise_Data")
    })
    observeEvent(input$statistics, {
      updateTabItems(parent.session, "sidebar", "Statistical_Analysis")
    })
    observeEvent(input$figure, {
      updateTabItems(parent.session, "sidebar", "Create_Figure")
    })
    
    observeEvent(input$dashboard, {
      updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
    })  
    
    # observeEvent(input$trs_interpreting_R_quiz, {
    #   feedback <- if (input$trs_interpreting_R_quiz == "option1") {
    #     div(class = "success-box", "\U1F64C Correct!")
    #   } else {
    #     div(class = "error-box", "\U1F914 Not quite - try again!")
    #   }
    #   
    #   output$average_technical_replicates_quiz_feedback <- renderUI({
    #     feedback
    #   })
    # })

  })
}



