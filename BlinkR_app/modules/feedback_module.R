feedback_module_ui <- function(id) {
  ns <- NS(id)
  feedback_tab <- tabItem(tabName = "feedback",
          fluidPage(
            fluidRow(
              box(
                title = "Feedback",
                id = "feedback",
                collapsible = FALSE,
                width = 12,
                solidHeader = TRUE,
              
      sliderInput(ns("overall_experience"), 
      "Overall, how would you rate your experience using this app? (1 is high, 5 is low)", 
      min = 1, max = 5, value = 3, step = 1, ticks = FALSE),

      sliderInput(ns("clarity"), 
      "How easy was it to navigate the app and understand what to do at each step? (1 is high, 5 is low)", 
      min = 1, max = 5, value = 3, step = 1, ticks = FALSE),

      conditionalPanel(
      condition = sprintf("input['%s'] <= 3", ns("clarity")),
      textAreaInput(ns("clarity_issues"), 
        "What part(s) of the app were confusing or difficult to use?", 
        placeholder = "Describe any difficulties you faced...")
      ),

      radioButtons(ns("bugs"), 
      "Did you experience any technical issues or bugs while using the app?", 
      choices = c("Yes" = "yes", "No" = "no"), 
      inline = TRUE),

      conditionalPanel(
      condition = sprintf("input['%s'] == 'yes'", ns("bugs")),
      textAreaInput(ns("bug_details"), 
        "Please describe the issue(s) you encountered", 
        placeholder = "Provide details on what happened...")
      ),

      radioButtons(ns("experiment_tools"), 
      "Did the app provide all the necessary tools and guidance for you to complete the experiment?", 
      choices = c("Yes" = "yes", "No" = "no"), 
      inline = TRUE),

      conditionalPanel(
      condition = sprintf("input['%s'] == 'no'", ns("experiment_tools")),
      textAreaInput(ns("missing_features"), 
        "What additional features or improvements would have made the experiment easier?", 
        placeholder = "Share your suggestions...")
      ),

      textAreaInput(ns("useful_features"), 
      "Which feature(s) did you find the most helpful? Why?", 
      placeholder = "Describe what worked well..."),
      textAreaInput(ns("least_useful_features"), 
      "Which feature(s) did you find the least useful or frustrating? Why?", 
      placeholder = "Describe what could be improved..."),
      textAreaInput(ns("general_feedback"), 
      "Do you have any other suggestions for improving the app?", 
      placeholder = "Share any additional thoughts..."),
      div(
          style = "text-align: center;",
      actionButton(ns("submit_feedback"), "Send Feedback", class = "fun-submit-button")
      ),
      uiOutput(ns("submission_feedback"))
          )
            )
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
        ns("back_page_feedback"),
        label = tagList(icon("arrow-left"), " Back"),
        class = "fun-nav-button"
      )
    )
  )
)
  )
}

feedback_module_server <- function(id, feedback_data, parent.session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns 
    
    feedback_sheet <- drive_get("Feedback")$id

    observeEvent(input$submit_feedback, {
      new_feedback <- data.frame(
        timestamp = Sys.time(),
        overall_experience = input$overall_experience,
        clarity = input$clarity,
        clarity_issues = ifelse(input$clarity <= 3, input$clarity_issues, NA),
        bugs = input$bugs,
        bug_details = ifelse(input$bugs == "yes", input$bug_details, NA),
        experiment_tools = input$experiment_tools,
        missing_features = ifelse(input$experiment_tools == "no", input$missing_features, NA),
        useful_features = input$useful_features,
        least_useful_features = input$least_useful_features,
        general_feedback = input$general_feedback,
        stringsAsFactors = FALSE
      )
      
      updated_feedback <- rbind(feedback_data(), new_feedback)
      feedback_data(updated_feedback)

      output$submission_feedback <- renderUI({
        div(style = "color: green;", "Thank you for your feedback!")
      })

      sheet_append(updated_feedback, ss = feedback_sheet, sheet = 1)
    })
    
    observeEvent(input$back_page_feedback, {
        updateTabItems(parent.session, "sidebar", "Upload_Report")
      })
      

  })
}
