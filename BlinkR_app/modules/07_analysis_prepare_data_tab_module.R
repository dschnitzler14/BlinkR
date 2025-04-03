analysis_prepare_data_module_ui <- function(id, i18n) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Prepare_Data",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          div(
            class = "page-title-box",
            tags$h2(
              tagList(shiny::icon("magnifying-glass"), i18n$t("Analyis: Prepare Your Data"))
            )
  )
)

      ),

      fluidRow(
        column(
          12,
          shinydashboard::box(
            title = i18n$t("1️⃣ View Data"),
            collapsible = TRUE,
            width = 12,
            solidHeader = TRUE,
            fluidRow(
              column(
                4,
                markdown(
                  i18n$t("First, let's have a look at the data. Try running `head(data)` and see what happens.")
                ),
                uiOutput(ns("view_data_code_feedback")),
                uiOutput(ns("view_data_quiz_feedback"))
              ),
              column(
                8,
                editor_module_ui(ns("view_data_editor"), i18n)
              )
            )
          ),
          shinydashboard::box(
            title = i18n$t("2️⃣ Prepare Data"),
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            solidHeader = TRUE,
            fluidRow(
              column(
                4,
                uiOutput(ns("average_technical_replicates_code")),
                uiOutput(ns("average_technical_replicates_code_result")),
              ),
              column(
                8,
                editor_module_ui(ns("average_trs_editor"), i18n)
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
                label = tagList(icon("rectangle-list"), i18n$t("Summarise the Data")),
                class = "action-button custom-action",
                `data-id` = "summarise_data"
              ),
              actionButton(
                ns("statistics"),
                label = tagList(icon("equals"), i18n$t("Run Statistical Analysis")),
                class = "action-button custom-action",
                `data-id` = "stats"
              ),
              actionButton(
                ns("figure"),
                label = tagList(icon("chart-simple"), i18n$t("Create a Figure")),
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
                label = tagList(icon("dashboard"), i18n$t("Go to Analysis Dashboard")),
                class = "action-button custom-dark-yellow"
              )
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
        ns("back_page_prepare"),
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_prepare"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
      )
    )
  )
}


analysis_prepare_data_module_server <- function(id, i18n, results_data, parent.session, session_folder_id, process_markdown) {
  moduleServer(id, function(input, output, session) {

      vars <- get_experiment_vars()

    # Load data
    view_data <- reactive({ NULL })
    
    view_data_read <- results_data() %>%
      select(-"group", -"initials", -"submission_id")
    
    view_data <- reactive({ view_data_read })
    
    # Step 1: View Data
    
    predefined_code_view_data <- "head(data)"
    
    view_data_result <- editor_module_server("view_data_editor", data = view_data, variable_name = "data", predefined_code = predefined_code_view_data, return_type = "result", session_folder_id, save_header = "View Data Code")
    
    
    observe({
      req(!is.null(view_data_result()), !is.null(view_data_result()$result))
      feedback <- if (is.data.frame(view_data_result()$result) && nrow(view_data_result()$result) > 0) {
          tagList(
            div(class = "success-box", i18n$t("\U1F64C That's our data! Looks good!")),
            markdown(
        i18n$t("Well done! You just ran your first bit of code!
        The `head()` command returns the first 6 lines of our table, so we can confirm that our data has loaded.
        Take a look at the column names - they will be useful for the next step!
        **The next step is turning this data into something that we can use for analysis.**
        ")),
            textInput(
              session$ns("interpret_head_results"),
              label = i18n$t("How many subjects (students) can you see in this slice of data?"),
              placeholder = "Type your answer here",
              ),
              div(
                style = "text-align: center;",
                actionButton(
                  session$ns("interpret_head_results_submit"),
                  label = i18n$t("Submit"),
                  class = "fun-submit-button"
                )
              ),
              uiOutput(session$ns("feedback_head")),
            radioButtons(
              session$ns("analysis_step2_quiz"), 
              label = i18n$t("What do you think we need to do to our data?"), 
              choices = list(
                "Get the average for each condition?" = "option1", 
                "Get average from technical replicates for each subject?" = "option2", 
                "Do statistical analysis?" = "option3"
              ),
              selected = character(0)
            )
          )
      
      } else if (!is.null(view_data_result())) {
        div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
      } else {
        NULL
      }
      
      output$view_data_code_feedback <- renderUI({
        feedback
    })
    
    })


  observeEvent(input$interpret_head_results_submit, {
      feedback_head <- if (input$interpret_head_results == 1) {
        div(class = "success-box", i18n$t("\U1F64C Correct!"))
      } else {
        div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
      }
      
      output$feedback_head <- renderUI({
        feedback_head
      })
    })
    
    observeEvent(input$analysis_step2_quiz, {
      feedback <- if (input$analysis_step2_quiz == "option2") {
        div(class = "success-box", i18n$t("\U1F64C Correct!"))
      } else {
        div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
      }
      
      output$view_data_quiz_feedback <- renderUI({
        feedback
      })
    })
    
        # Step 2: Pre-Process Data

    output$average_technical_replicates_code <- renderUI({
      process_markdown("07_analysis/analysis_pre_process_data_code.Rmd")
    })

    predefined_code_pre_process_data <- whisker.render(
    read_file("markdown/07_analysis/predefined_code_pre_process_data.txt"),
    vars
    )
      
    average_trs_result <- editor_module_server("average_trs_editor", data = view_data, variable_name = "data", predefined_code = predefined_code_pre_process_data, return_type = "result", session_folder_id, save_header = "Pre-Process Data Code")
    
output$home_prepare_data <- renderUI({
  process_markdown("07_analysis/analysis_home_prepare_data.Rmd")
})


    observe({
      req(!is.null(average_trs_result()), !is.null(average_trs_result()$result))

      feedback <- if (is.data.frame(average_trs_result()$result) && nrow(average_trs_result()$result) > 0) {
        tagList(
          div(class = "success-box", i18n$t("\U1F64C Good Job!")),
          uiOutput(session$ns("home_prepare_data")),
        )
      } else if (!is.null(average_trs_result())) {
        div(class = "error-box", i18n$t("\U1F914 Not quite - try again!"))
      } else {
        NULL
      }
      
      output$average_technical_replicates_code_result <- renderUI({
        feedback
      })
    })
    
    observeEvent(input$back_page_prepare, {
        updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
      })
      observeEvent(input$next_page_prepare, {
        updateTabItems(parent.session, "sidebar", "Summarise_Data")
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
    

  })
}



