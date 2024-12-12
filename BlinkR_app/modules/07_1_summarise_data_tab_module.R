analysis_summarise_data_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Summarise_Data",
    fluidPage(
      fluidRow(
        column(
          12,
          box(
            title = "Summarising The Data",
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            fluidRow(
              column(
                4,
                markdown("In order to summarise the data, enter the code below into the code editor:"),
                wellPanel(
                  markdown(
                    "
                    ```
                    data_summary <- average_trs %>%
                      group_by(stress_status) %>%
                      summarise(
                        n = n(),
                        mean = mean(average_blinks_per_minute, na.rm = TRUE),
                        sd = sd(average_blinks_per_minute, na.rm = TRUE),
                        sem = sd / sqrt(n)
                      )
                    ```
                    "
                  )
                ),
                uiOutput(ns("summary_code_feedback"))
              
              ),
              column(8, editor_module_ui(ns("summarise_editor")))
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
          
        ))))}


analysis_summarise_data_module_server <- function(id, results_data, parent.session) {
  moduleServer(id, function(input, output, session) {
    # Load data
    data_read <- read.csv("/Users/Danny_1/GitHub/BlinkR/BlinkR_app/data/dummy_blinking_data.csv")
    
    data <- reactive({ data_read })
    
    average_trs_assumptions <- data_read %>%
      dplyr::group_by(id, stress_status) %>%
      dplyr::summarise(
        average_blinks_per_minute = mean(blinks_per_minute, na.rm = TRUE),
        .groups = 'drop'
      )
    
    data_summary <- average_trs %>%
      group_by(stress_status) %>%
      summarise(
        n = n(),
        mean = mean(average_blinks_per_minute, na.rm = TRUE),
        sd = sd(average_blinks_per_minute, na.rm = TRUE),
        sem = sd / sqrt(n)
      )
    
    average_trs <- reactive({
      as.data.frame(average_trs_assumptions)
    })
    
    summarise_result <- editor_module_server("summarise_editor", data = average_trs)
    
    observe({
      req(summarise_result())
      if (tibble::is_tibble(summarise_result())) {
        output$summary_code_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
            markdown("
        Let's first take a look at the code. Just like in first step, we are using the `dplyr` recipe book.
        "),
          radioButtons(
            session$ns("summary_code_quiz"), 
            label = "Do you remember what `summarise` does?", 
            choices = list(
              "Creates a new dataset" = "option1", 
              "Creates new columns" = "option2", 
              "Uses meta-data to summarise" = "option3"
            ),
            selected = character(0)
          ),
          uiOutput(session$ns("summary_code_quiz_feedback")),
          markdown("Next let's take a look at the result."),
          numericInput(
            inputId = session$ns("mean_unstressed_group_quiz"),
            label = "What is the mean of the unstressed group?",
            value = 0,
            min = 5,
            max = 60
          ),
          actionButton(
            session$ns("submit_mean_unstressed_group_quiz_answer"),
            label = "Submit"
          ),
          uiOutput(session$ns("mean_unstressed_group_quiz_feedback")),
          numericInput(
            inputId = session$ns("sem_stressed_group_quiz"),
            label = "What is the standard error of the mean (sem) of the stressed group?",
            value = 0,
            min = 5,
            max = 60
          ),
          actionButton(
            session$ns("submit_sem_stressed_group_quiz_answer"),
            label = "Submit"
          ),
          uiOutput(session$ns("submit_sem_stressed_group_quiz_feedback")),
          radioButtons(
            session$ns("summary_result_interpretation_quiz"), 
            label = "Can we tell from this if this is statistically significant?", 
            choices = list(
              "Yes" = "option1", 
              "No" = "option2", 
              "I don't know" = "option3"
            ),
            selected = character(0)
          ),
          uiOutput(session$ns("summary_result_interpretation_quiz_feedback")),
          
          )
        })
      } else {
        output$summary_code_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
      }
    })
    
    observeEvent(input$summary_code_quiz, {
      feedback <- if (input$summary_code_quiz == "option2") {
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
      
      output$summary_code_quiz_feedback <- renderUI({
        feedback
      })
    })
    
    unstressed_mean <- data_summary$mean[1]
    unstressed_mean_round_up <- round(unstressed_mean,0)
    unstressed_mean_round_down <- floor(unstressed_mean)
    
    
    observeEvent(input$submit_mean_unstressed_group_quiz_answer, {
      user_answer_mean_unstressed <- as.numeric(input$mean_unstressed_group_quiz)
      feedback <- 
        if (!is.na(user_answer_mean_unstressed) && user_answer_mean_unstressed != "" &&
                      user_answer_mean_unstressed >= unstressed_mean_round_down && user_answer_mean_unstressed <= unstressed_mean_round_up) {        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }

      output$mean_unstressed_group_quiz_feedback <- renderUI({
        feedback
      })
    })
    
    ##for stressed sem
    stressed_sem <- data_summary$sem[2]
    stressed_sem_round_up <- round(stressed_sem,2)
    stressed_sem_round_down <- floor(stressed_sem)
    

    observeEvent(input$submit_sem_stressed_group_quiz_answer, {
      user_answer_sem_stressed <- as.numeric(input$sem_stressed_group_quiz)
      feedback <- 
        if (!is.na(user_answer_sem_stressed) && user_answer_sem_stressed != "" &&
            user_answer_sem_stressed >= stressed_sem_round_down && user_answer_sem_stressed <= stressed_sem_round_up) {
          div(class = "success-box", "\U1F64C Correct!")
        } else {
          div(class = "error-box", "\U1F914 Not quite - try again!")
        }
      
      output$submit_sem_stressed_group_quiz_feedback <- renderUI({
        feedback
      })
    })
    
    #for significance radio quiz
    observeEvent(input$summary_result_interpretation_quiz, {
      feedback <- if (input$summary_result_interpretation_quiz == "option2") {
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
      
      output$summary_result_interpretation_quiz_feedback <- renderUI({
        feedback
      })
    })
    
    #home button
    observeEvent(input$dashboard, {
      updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
    }) 
    
  })
}
