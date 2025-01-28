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
            solidHeader = TRUE,
            fluidRow(
              column(
                4,
                uiOutput(ns("step_feedback")),
              ),
              column(8,
                uiOutput(ns("editor_ui")) 
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
                label = tagList(icon("dashboard"), "Go to Analysis Dashboard"),
                class = "action-button custom-dark-yellow"
              )
            )
          )
          
        ))))}


analysis_summarise_data_module_server <- function(id, results_data, parent.session, saved_results, session_folder_id) {
  moduleServer(id, function(input, output, session) {
    
    average_trs <- reactive({ NULL })
    
    average_trs_results <- results_data %>%
      select(-"Group", -"Initials", -"Submission_ID") %>%
      dplyr::group_by(ID, Stress_Status) %>%
      dplyr::summarise(
        Average_Blinks_Per_Minute = mean(Blinks_Per_Minute, na.rm = TRUE),
        .groups = 'drop'
      )
    
    average_trs <- reactive({ average_trs_results })
 
tutorial_steps <- list(
  step1 = list(
    predefined_code = read_file("markdown/07_analysis/predefined_code_summarise_filter_unstressed.txt"),
    markdown = includeMarkdown("markdown/07_analysis/analysis_summarise_data_filter_unstressed.Rmd"),
    evaluate = function(result) {
      tibble::is_tibble(result) && nrow(result) > 0
    },
    feedback_success = "\U1F64C Great! You've filtered the data successfully.",
    feedback_failure = "\U1F914 Not quite - ensure you're filtering the data correctly!",
    next_step = "step2"
  ),
  step2 = list(
    predefined_code = read_file("markdown/07_analysis/predefined_code_calculate_mean.txt"),
    markdown = includeMarkdown("markdown/07_analysis/analysis_summarise_data_mean_unstressed.Rmd"),
    evaluate = function(result) {
      tibble::is_tibble(result) && "mean_value" %in% colnames(result)
    },
    feedback_success = "\U1F44D Good job! You've calculated the mean.",
    feedback_failure = "\U1F62F Hmm, looks like something's missing. Try again!",
    next_step = NULL 
  )
)

current_step <- reactiveVal("step1")     
completed_tutorial_steps <- reactiveVal(list())  

observe({
  if (is.null(completed_tutorial_steps()[["step1"]])) {
    completed_tutorial_steps_list <- list(
      step1 = list(
        feedback = NULL, 
        markdown = tutorial_steps[["step1"]]$markdown
      )
    )
    completed_tutorial_steps(completed_tutorial_steps_list)
  }
})


output$step_feedback <- renderUI({
  all_tutorial_steps <- completed_tutorial_steps()
  req(all_tutorial_steps)
  
  tagList(
    lapply(names(all_tutorial_steps), function(s) {
      feedback <- all_tutorial_steps[[s]]$feedback
      markdown <- all_tutorial_steps[[s]]$markdown
      
      tagList(
        h4(paste("Step:", gsub("step", "", s))),
        markdown,
        if (!is.null(feedback)) feedback,
        hr()
      )
    })
  )
})

output$editor_ui <- renderUI({
  editor_module_ui(session$ns("dynamic_editor"))
})

summarise_result <- reactive({
  step <- current_step()
  req(step)
  
  editor_module_server(
    "dynamic_editor",
    data = average_trs,
    variable_name = "average_trs",
    predefined_code = tutorial_steps[[step]]$predefined_code,
    return_type = "result",
    session_folder_id = session_folder_id,
    save_header = paste("Code for", step)
  )
})

observe({
  req(summarise_result())            
  req(summarise_result()$result) 

  step <- current_step()
  req(step)

  result <- summarise_result()$result
  
  if (tutorial_steps[[step]]$evaluate(result)) {
    showNotification(tutorial_steps[[step]]$feedback_success, type = "message")
    
    feedback_ui <- div(class = "success-box", "\U1F64C Great!")
    completed_tutorial_steps_list <- completed_tutorial_steps()
    completed_tutorial_steps_list[[step]] <- list(
      feedback = feedback_ui,
      markdown = tutorial_steps[[step]]$markdown
    )
    completed_tutorial_steps(completed_tutorial_steps_list)
    
    next_step <- tutorial_steps[[step]]$next_step
    if (!is.null(next_step)) {
      completed_tutorial_steps_list[[next_step]] <- list(
        feedback = NULL,
        markdown = tutorial_steps[[next_step]]$markdown
      )
      completed_tutorial_steps(completed_tutorial_steps_list)
      
      current_step(next_step)
    } else {
      showNotification("\U1F389 All tutorial_steps complete! Great work!", type = "success")
    }

  } else {
    showNotification(tutorial_steps[[step]]$feedback_failure, type = "error")
    
    feedback_ui <- div(class = "error-box", "\U1F914 Not quite - try again!")
    completed_tutorial_steps_list <- completed_tutorial_steps()
    completed_tutorial_steps_list[[step]] <- list(
      feedback = feedback_ui,
      markdown = tutorial_steps[[step]]$markdown
    )
    completed_tutorial_steps(completed_tutorial_steps_list)
  }
})

        
#   output$step1_filter_data <- renderUI({
#   tagList(
#     includeMarkdown("markdown/07_analysis/analysis_summarise_data_filter_unstressed.Rmd"),
#     uiOutput(session$ns("editor_ui"))
#   )
# })

#     output$editor_ui <- renderUI({
#       editor_module_ui(session$ns("summarise_editor_step1"))
#     })


#     predefined_code_summarise_step1 <- read_file("markdown/07_analysis/predefined_code_summarise_filter_unstressed.txt")
    
#     summarise_editor_step1 <- editor_module_server("summarise_editor_step1", data = average_trs, variable_name = "average_trs", 
#                                              predefined_code = predefined_code_summarise_step1, return_type = "result", 
#                                              session_folder_id, save_header = "Summarise Result Code")
    
#     observe({
#       req(!is.null(summarise_editor_step1()), !is.null(summarise_editor_step1()$result))

#       if (tibble::is_tibble(summarise_editor_step1()$result)) {
#         output$step1_filter_data_feedback <- renderUI({
#           tagList(
#             div(class = "success-box", "\U1F64C Great!"),
#             uiOutput(session$ns("step2_calculate_mean")),

#           )
#         })
        
#       } else {
#         output$step1_filter_data_feedback <- renderUI({
#           div(class = "error-box", "\U1F914 Not quite - try again!")
#         })
        
#       }
#     })
    
    
    observeEvent(input$statistics, {
      updateTabItems(parent.session, "sidebar", "Statistical_Analysis")
    })
    observeEvent(input$figure, {
      updateTabItems(parent.session, "sidebar", "Create_Figure")
    })
    
    #home button
    observeEvent(input$dashboard, {
      updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
    }) 
    
  })
}