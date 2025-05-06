feedback_module_ui <- function(id, i18n) {
  ns <- NS(id)
  feedback_tab <- tabItem(tabName = "feedback",
          fluidPage(
            fluidRow(
              box(
                title = i18n$t("Feedback"),
                id = "feedback",
                collapsible = FALSE,
                width = 12,
                solidHeader = TRUE,
              
      sliderInput(ns("overall_experience"), 
      i18n$t("Overall, how would you rate your experience using this app? (1 is high, 5 is low)"), 
      min = 1, max = 5, value = 3, step = 1, ticks = FALSE),

      sliderInput(ns("clarity"), 
      i18n$t("How easy was it to navigate the app and understand what to do at each step? (1 is high, 5 is low)"), 
      min = 1, max = 5, value = 3, step = 1, ticks = FALSE),

      conditionalPanel(
      condition = sprintf("input['%s'] <= 3", ns("clarity")),
      textAreaInput(ns("clarity_issues"), 
        i18n$t("What part(s) of the app were confusing or difficult to use?"), 
        placeholder = " ")
      ),
      tags$i(
        tagList(
          shiny::icon("info-circle", class = "me-1 text-info"),
          i18n$t("Describe any difficulties you faced.")
        ),
        style = "margin-top:-6px; display:block;",
      ),
      tags$br(),

      radioButtons(ns("bugs"), 
      i18n$t("Did you experience any technical issues or bugs while using the app?"), 
      choiceNames = list(
            i18n$t("Yes"),
            i18n$t("No")
          ),
          choiceValues = list(
            "yes", 
            "no"
          ),
      inline = TRUE),

      conditionalPanel(
      condition = sprintf("input['%s'] == 'yes'", ns("bugs")),
      textAreaInput(ns("bug_details"), 
         i18n$t("Please describe the issue(s) you encountered"), 
        placeholder = " ")
      ),
      tags$i(
        tagList(
          shiny::icon("info-circle", class = "me-1 text-info"),
          i18n$t("Provide details on what happened")
        ),
        style = "margin-top:-6px; display:block;",
      ),
      tags$br(),

      radioButtons(ns("experiment_tools"), 
      i18n$t("Did the app provide all the necessary tools and guidance for you to complete the experiment?"), 
      choiceNames = list(
              i18n$t("Yes"),
              i18n$t("No")
            ),
            choiceValues = list(
              "yes", 
              "no"
            ),
      inline = TRUE),

      conditionalPanel(
      condition = sprintf("input['%s'] == 'no'", ns("experiment_tools")),
      textAreaInput(ns("missing_features"), 
        i18n$t("What additional features or improvements would have made the experiment easier?"), 
        placeholder = " ")
      ),
      tags$i(
        tagList(
          shiny::icon("info-circle", class = "me-1 text-info"),
          i18n$t("Share your suggestions.")
        ),
        style = "margin-top:-6px; display:block;",
      ),
      tags$br(),

      textAreaInput(ns("useful_features"), 
      i18n$t("Which feature(s) did you find the most helpful? Why?"), 
      placeholder = " "),
       tags$i(
        tagList(
          shiny::icon("info-circle", class = "me-1 text-info"),
          i18n$t("Describe what worked well.")
        ),
        style = "margin-top:-6px; display:block;",
      ),
      tags$br(),

      textAreaInput(ns("least_useful_features"), 
      i18n$t("Which feature(s) did you find the least useful or frustrating? Why?"), 
      placeholder = " "),
       tags$i(
        tagList(
          shiny::icon("info-circle", class = "me-1 text-info"),
          i18n$t("Describe what could be improved.")
        ),
        style = "margin-top:-6px; display:block;",
      ),
      tags$br(),
      textAreaInput(ns("general_feedback"), 
      i18n$t("Do you have any other suggestions for improving the app?"), 
      placeholder = " "),
      tags$i(
        tagList(
          shiny::icon("info-circle", class = "me-1 text-info"),
          i18n$t("Share any additional thoughts.")
        ),
        style = "margin-top:-6px; display:block;",
      ),
      tags$br(),
      div(
          style = "text-align: center;",
      actionButton(ns("submit_feedback"), i18n$t("Send Feedback"), class = "fun-submit-button")
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
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      )
    )
  )
)
  )
}

feedback_module_server <- function(id, i18n, feedback_data, parent.session) {
  moduleServer(id, function(input, output, session) {
          vars <- get_experiment_vars()

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
