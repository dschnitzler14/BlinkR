analysis_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Analysis_Home",
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
            title = "What do you want to do next?",
            collapsible = FALSE,
            width = 12,
            status = "info",
            actionButton(ns("summarise"), "Summarise the Data", class = "action-button custom-action"),
            actionButton(ns("statistics"), "Run Statistical Analysis", class = "action-button custom-action"),
            actionButton(ns("figure"), "Create a Figure", class = "action-button custom-action"),
            align = "center"
          )
        )
      )
    )
  )
}


analysis_module_server <- function(id, results_data, parent.session) {
  moduleServer(id, function(input, output, session) {
    # Load data
    data_read <- read.csv("/Users/Danny_1/GitHub/BlinkR/BlinkR_app/data/dummy_blinking_data.csv")

    data <- reactive({ data_read })
    
    # Step 1: View Data
    step1_result <- editor_module_server("step1_editor", data = data)
    

    observeEvent(input$summarise, {
      updateTabItems(parent.session, "sidebar", "Summarise_Data")
    })
    observeEvent(input$statistics, {
      updateTabItems(parent.session, "sidebar", "Statistical_Analysis")
    })
    observeEvent(input$figure, {
      updateTabItems(parent.session, "sidebar", "Create_Figure")
    })
    
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

  })
}



