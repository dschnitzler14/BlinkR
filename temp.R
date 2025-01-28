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
                uiOutput(ns("step1_filter_data"))
                uiOutput(ns("step1_filter_data_feedback")),
                uiOutput(ns("step2_calculate_mean")),             
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
    

    # uiOutput(ns("step1_filter_data"))
    # uiOutput(ns("step1_filter_data_feedback")),
    # uiOutput(ns("step2_calculate_mean")),
        
    ouput$step1_filter_data <- renderUI({
        includeMarkdown("markdown/07_analysis/analysis_summarise_data_filter_unstressed.Rmd")
        output$editor_ui <- renderUI({
          editor_module_ui("summarise_editor_step1")
        })
    })


    predefined_code_summarise_step1 <- readLines("markdown/07_analysis/summarise_predefined_code_filter.txt")
    
    summarise_editor_step1 <- editor_module_server("summarise_editor_step1", data = average_trs, variable_name = "average_trs", 
                                             predefined_code = predefined_code_summarise_step1, return_type = "result", 
                                             session_folder_id, save_header = "Summarise Result Code")
    
    observe({
      req(!is.null(summarise_editor_step1()), !is.null(summarise_editor_step1()$result))

      if (tibble::is_tibble(summarise_editor_step1()$result)) {
        output$step1_filter_data_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
            uiOutput(session$ns("step2_calculate_mean")),

          )
        })
        
      } else {
        output$step1_filter_data_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        
      }
    })
    
    
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