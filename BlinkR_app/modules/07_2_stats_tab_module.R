analysis_stats_module_ui <- function(id) {
  ns <- NS(id)

   tabItem(
    tabName = "Statistical_Analysis",
    fluidPage(
      fluidRow(
            column(
              width = 12,
              div(
                class = "page-title-box",
                tags$h2(
                  tagList(shiny::icon("equals"), "Analysis: Statistical Analysis")
                )
      )
    )),
      fluidRow(
        uiOutput(ns("testing_assumptions")),
        uiOutput(ns("normal_output")),
        uiOutput(ns("not_normal_output")),
        uiOutput(ns("not_normal_unpaired_ui")),
        uiOutput(ns("not_normal_paired_ui")),
        uiOutput(ns("normal_unpaired_ui")),
        uiOutput(ns("normal_paired_ui")),
        uiOutput(ns("effect_size_t_test_paired")),
        uiOutput(ns("effect_size_t_test_unpaired")),
        uiOutput(ns("effect_size_wilcoxon_paired")),
        uiOutput(ns("effect_size_wilcoxon_unpaired")),
        uiOutput(ns("interpretation_quiz")),


    ),
    fluidRow(
          column(
            width = 12,
            div(
              style = "display: flex; justify-content: center; align-items: center; gap: 20px; height: 100px;",
              actionButton(
                ns("summarise"),
                label = tagList(icon("rectangle-list"), "Summarise the Data"),
                class = "action-button custom-action",
                `data-id` = "summarise_data"
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
        ns("back_page_stats"),
        label = tagList(icon("arrow-left"), " Back"),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_stats"), 
        label = tagList("Next ", icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
    )
   )
}

analysis_stats_module_server <- function(id, results_data, parent.session, saved_results, session_folder_id) {
  moduleServer(id, function(input, output, session) {
    vars <- get_experiment_vars()

    average_trs <- reactive({
      NULL
    })
    
    average_trs_results <- results_data() %>%
      select(-"group", -"initials", -"submission_id") %>%
      dplyr::group_by(id, !!sym(vars$levels_variable_name)) %>%
      dplyr::summarise(
        average_measurement = mean(!!sym(vars$measurement_variable_name), na.rm = TRUE),
        .groups = 'drop'
      )
    
    average_trs <- reactive({
      average_trs_results
    })

average_trs_paired_wide <- reactive({
      NULL
    })
    

    average_trs_paired_wide_data <- average_trs()%>%
      pivot_wider(names_from = vars$levels_variable_name, values_from = average_measurement)
    
    average_trs_paired_wide <- reactive({
      average_trs_paired_wide_data
    })


    save_result <- function(name, key, result_obj, saved_results, session_folder_id) {
  saved_results$scripts[[key]] <- result_obj

  result_as_char <- capture.output(print(result_obj))

  temp_file <- tempfile(fileext = ".txt")
  writeLines(result_as_char, con = temp_file)

  path <- drive_get(as_id(session_folder_id))
  drive_upload(
    media = temp_file,
    path = path,
    name = paste0(key, ".txt"),
    overwrite = TRUE
  )
    unlink(temp_file)

  showNotification(paste0(name, " result saved successfully."), type = "message", duration = 3)
}

### 
rmd_content_analysis_hist_plot_explainer <- readLines("markdown/07_analysis/analysis_hist_plot_explainer.Rmd")
processed_rmd_analysis_hist_plot_explainer <- whisker.render(paste(rmd_content_analysis_hist_plot_explainer, collapse = "\n"), vars)

output$testing_assumptions <- renderUI({
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "testing_assumptions",
              title = "1️⃣ Testing Assumptions",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  HTML(markdownToHTML(text = processed_rmd_analysis_hist_plot_explainer, fragment.only = TRUE)),
                  div(
                  style = "text-align: center;",
                  actionButton(session$ns("run_hist_Plot"), 
                    tagList(shiny::icon("circle-plus"), "Generate Histogram to check for Normality"),
                    class = "fun-generate-button")
                  ),
                  uiOutput(session$ns("hist_explainer_ui")),
                  ),
                  column(6,
                  plotOutput(session$ns("hist_plot"))
                  )
              ),
              
            )
          ),
          
        ),
        
      )
    })

  
  hist_plot_reactive <- eventReactive(input$run_hist_Plot, {
  req(average_trs())
  
  hist(
    average_trs()$average_measurement,
    main = sprintf("Distribution of %s", vars$measurement_text_name),
    xlab = sprintf("Average of Technical Replicates for %s", vars$measurement_text_name),
    ylab = "Frequency",
    col = "grey49", border = "black"
  )
  
  recorded <- recordPlot()
  saved_results$recorded_plots[["hist_plot"]] <- recorded
  
  recorded
})


observeEvent(input$run_hist_Plot, {
  output$not_normal_unpaired_ui <- renderUI({NULL})
  output$not_normal_paired_ui <- renderUI({NULL})
  output$normal_unpaired_ui <- renderUI({NULL})
  output$normal_paired_ui <- renderUI({NULL})

  output$effect_size_t_test_paired <- renderUI({NULL})
  output$effect_size_t_test_unpaired <- renderUI({NULL})
  output$effect_size_wilcoxon_paired <- renderUI({NULL})
  output$effect_size_wilcoxon_unpaired <- renderUI({NULL})
  output$interpretation_quiz <- renderUI({NULL})
  output$enter_effect_size_feedback <- renderUI({NULL})
  output$enter_p_value_feedback <- renderUI({NULL})

  p_value_reactive <- reactive({NULL})
  effect_size_reactive <- reactive({NULL})

if(!is.null(normal_unpaired_result()$result)){
    normal_unpaired_result <- NULL
  } else if (!is.null(normal_paired_result()$result)) {
     normal_paired_result <- NULL
  } else if (!is.null(not_normal_unpaired_result()$result)) {
     not_normal_unpaired_result <- NULL
  } else if (!is.null(not_normal_paired_result()$result)) {
     not_normal_paired_result <- NULL
  }

      output$hist_plot <- renderPlot({
      replayPlot(hist_plot_reactive())

      temp_file <- tempfile(fileext = ".png")
      png(temp_file, width = 800, height = 600)
      replayPlot(saved_results$recorded_plots[["hist_plot"]])
      dev.off()
      
      path <- drive_get(as_id(session_folder_id))
      
      drive_upload(
        media = temp_file,
        path = path,
        name = paste0("hist_plot.png"),
        overwrite = TRUE
      )
      
      #recordPlot(NULL)
      #saved_results$recorded_plots[["hist_plot"]] <- NULL
      unlink(temp_file)
      showNotification("Plot saved successfully.", type = "message", duration = 3)
    })

rmd_content_analysis_hist_plot_explainer_output <- readLines("markdown/07_analysis/analysis_hist_plot_explainer_output.Rmd")
processed_rmd_analysis_hist_plot_explainer_output <- whisker.render(paste(rmd_content_analysis_hist_plot_explainer_output, collapse = "\n"), vars)
  
    output$hist_explainer_ui <- renderUI({
      tagList(
       HTML(markdownToHTML(text = processed_rmd_analysis_hist_plot_explainer_output, fragment.only = TRUE)),
      div(
          style = "text-align: center;",
      actionButton(session$ns("normal"), "👍 The Data is Normal", class = "fun-submit-button"),
      actionButton(session$ns("not_normal"), "👎 The Data is Not Normal", class = "fun-submit-button")
      )
      )
      })

  })

observeEvent(input$normal, {
  req(input$normal)
    output$not_normal_output <- renderUI({NULL})
    output$not_normal_unpaired_ui <- renderUI({NULL})
    output$not_normal_paired_ui <- renderUI({NULL})
    output$normal_unpaired_ui <- renderUI({NULL})
    output$normal_paired_ui <- renderUI({NULL})

    output$effect_size_t_test_paired <- renderUI({NULL})
    output$effect_size_t_test_unpaired <- renderUI({NULL})
    output$effect_size_wilcoxon_paired <- renderUI({NULL})
    output$effect_size_wilcoxon_unpaired <- renderUI({NULL})
    output$interpretation_quiz <- renderUI({NULL})

  p_value_reactive <- reactive({NULL})
  effect_size_reactive <- reactive({NULL})

  if(!is.null(normal_unpaired_result()$result)){
    normal_unpaired_result <- NULL
  } else if (!is.null(normal_paired_result()$result)) {
     normal_paired_result <- NULL
  } else if (!is.null(not_normal_unpaired_result()$result)) {
     not_normal_unpaired_result <- NULL
  } else if (!is.null(not_normal_paired_result()$result)) {
     not_normal_paired_result <- NULL
  }

rmd_content_analysis_paired_unpaired_explainer_output <- readLines("markdown/07_analysis/analysis_paired_unpaired_explainer_output.Rmd")
processed_rmd_analysis_paired_unpaired_explainer_output <- whisker.render(paste(rmd_content_analysis_paired_unpaired_explainer_output, collapse = "\n"), vars)

    output$normal_output <- renderUI({
      tagList(
        box(
        id = "paired_unpaired_explainer_normal",
              title = "2️⃣ Is the Data Paired or Unpaired?",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
        HTML(markdownToHTML(text = processed_rmd_analysis_paired_unpaired_explainer_output, fragment.only = TRUE)),
      div(
        style = "text-align: center;",
        actionButton(session$ns("unpaired_normal"), "☝️ The Data is Not Paired", class = "fun-submit-button"),
        actionButton(session$ns("paired_normal"), "✌️ The Data is Paired", class = "fun-submit-button")
        )
      )
      )
    })
})

observeEvent(input$not_normal, {
  req(input$not_normal)
  output$normal_output <- renderUI({NULL})
  output$not_normal_unpaired_ui <- renderUI({NULL})
  output$not_normal_paired_ui <- renderUI({NULL})
  output$normal_unpaired_ui <- renderUI({NULL})
  output$normal_paired_ui <- renderUI({NULL})

  output$effect_size_t_test_paired <- renderUI({NULL})
  output$effect_size_t_test_unpaired <- renderUI({NULL})
  output$effect_size_wilcoxon_paired <- renderUI({NULL})
  output$effect_size_wilcoxon_unpaired <- renderUI({NULL})
  output$interpretation_quiz <- renderUI({NULL})

  p_value_reactive <- reactive({NULL})
  effect_size_reactive <- reactive({NULL})

  if(!is.null(normal_unpaired_result()$result)){
    normal_unpaired_result <- NULL
  } else if (!is.null(normal_paired_result()$result)) {
     normal_paired_result <- NULL
  } else if (!is.null(not_normal_unpaired_result()$result)) {
     not_normal_unpaired_result <- NULL
  } else if (!is.null(not_normal_paired_result()$result)) {
     not_normal_paired_result <- NULL
  }
  
rmd_content_analysis_paired_unpaired_explainer_output <- readLines("markdown/07_analysis/analysis_paired_unpaired_explainer_output.Rmd")
processed_rmd_analysis_paired_unpaired_explainer_output <- whisker.render(paste(rmd_content_analysis_paired_unpaired_explainer_output, collapse = "\n"), vars)

    output$not_normal_output <- renderUI({

      tagList(
        box(
        id = "paired_unpaired_explainer",
              title = "2️⃣ Is the Data Paired or Unpaired?",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
        HTML(markdownToHTML(text = processed_rmd_analysis_paired_unpaired_explainer_output, fragment.only = TRUE)),

        div(
              style = "display: flex; justify-content: center; align-items: center; gap: 20px; height: 100px;",
        actionButton(session$ns("unpaired_not_normal"), "☝️ The Data is Not Paired", class = "fun-submit-button"),
        actionButton(session$ns("paired_not_normal"), "✌️ The Data is Paired", class = "fun-submit-button")
      )
        )
      )
    
    })
})

# 1. not normal unpaired

  predefined_code_not_normal_unpaired = whisker.render(
    read_file("markdown/07_analysis/predefined_code_wilcoxon_test_unpaired.txt"),
    vars)
  not_normal_unpaired_result <- editor_module_server("not_normal_unpaired", average_trs, "average_trs", predefined_code = predefined_code_not_normal_unpaired, return_type = "result", session_folder_id, save_header = "Statistical Analysis: Not Normal Unpaired")

observeEvent(input$unpaired_not_normal,{
  #output$not_normal_unpaired_ui <- renderUI({NULL})
  output$not_normal_paired_ui <- renderUI({NULL})
  output$normal_unpaired_ui <- renderUI({NULL})
  output$normal_paired_ui <- renderUI({NULL})

  output$effect_size_t_test_paired <- renderUI({NULL})
  output$effect_size_t_test_unpaired <- renderUI({NULL})
  output$effect_size_wilcoxon_paired <- renderUI({NULL})
  output$effect_size_wilcoxon_unpaired <- renderUI({NULL})
  output$interpretation_quiz <- renderUI({NULL})
  output$enter_effect_size_feedback <- renderUI({NULL})
  output$enter_p_value_feedback <- renderUI({NULL})

  p_value_reactive <- reactive({NULL})
  effect_size_reactive <- reactive({NULL})

if(!is.null(normal_unpaired_result()$result)){
    normal_unpaired_result <- NULL
  } else if (!is.null(normal_paired_result()$result)) {
     normal_paired_result <- NULL
  } else if (!is.null(not_normal_unpaired_result()$result)) {
     not_normal_unpaired_result <- NULL
  } else if (!is.null(not_normal_paired_result()$result)) {
     not_normal_paired_result <- NULL
  }

rmd_content_analysis_wilcoxon_test_unpaired <- readLines("markdown/07_analysis/analysis_wilcoxon_test_unpaired.Rmd")
processed_rmd_analysis_wilcoxon_test_unpaired <- whisker.render(paste(rmd_content_analysis_wilcoxon_test_unpaired, collapse = "\n"), vars)

  output$not_normal_unpaired_ui <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "not_normal_unpaired",
              title = "3️⃣ Not Normal Unpaired: Wilcoxon Test",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  HTML(markdownToHTML(text = processed_rmd_analysis_wilcoxon_test_unpaired, fragment.only = TRUE)),
                  uiOutput(session$ns("not_normal_unpaired_feedback"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("not_normal_unpaired")),
                  uiOutput(session$ns("save_not_normal_unpaired"))
                  )
              ),
              
            )
          ),
          
        ),
        
      )
  })
})


observe({
      req(!is.null(not_normal_unpaired_result()), !is.null(not_normal_unpaired_result()$result))

      if (inherits(not_normal_unpaired_result()$result, "htest")) {
        output$not_normal_unpaired_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
          )
    })
      output$save_not_normal_unpaired <- renderUI({
        tagList(
            div(
      style = "display: flex; justify-content: center; align-items: center; width: 100%;",
          actionButton(
            session$ns("save_not_normal_unpaired_button"),
            label = tagList(icon("save"), "Save Results to Dashboard"),
            class = "action-button custom-action",
            `data-id` = "not_normal_unpaired_save"
          )
            )
        )

        })
        } else {
        output$not_normal_unpaired_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
      })


observeEvent(input$save_not_normal_unpaired_button, {
  save_result(
    name             = "Wilcoxon Test - Unpaired",
    key              = "stats_not_normal_unpaired",
    result_obj       = not_normal_unpaired_result(), 
    saved_results    = saved_results,
    session_folder_id = session_folder_id
  )
})

# 2. not normal paired

  predefined_code_not_normal_paired = whisker.render(
    read_file("markdown/07_analysis/predefined_code_wilcoxon_test_paired.txt"),
    vars)
  not_normal_paired_result <- editor_module_server("not_normal_paired", average_trs, "average_trs", predefined_code = predefined_code_not_normal_paired, return_type = "result", session_folder_id, save_header = "Statistical Analysis: Not Normal Paired")

observeEvent(input$paired_not_normal,{
  output$not_normal_unpaired_ui <- renderUI({NULL})
  #output$not_normal_paired_ui <- renderUI({NULL})
  output$normal_unpaired_ui <- renderUI({NULL})
  output$normal_paired_ui <- renderUI({NULL})

  output$effect_size_t_test_paired <- renderUI({NULL})
  output$effect_size_t_test_unpaired <- renderUI({NULL})
  output$effect_size_wilcoxon_paired <- renderUI({NULL})
  output$effect_size_wilcoxon_unpaired <- renderUI({NULL})
  output$interpretation_quiz <- renderUI({NULL})
  output$enter_effect_size_feedback <- renderUI({NULL})
  output$enter_p_value_feedback <- renderUI({NULL})

  p_value_reactive <- reactive({NULL})
  effect_size_reactive <- reactive({NULL})


if(!is.null(normal_unpaired_result()$result)){
    normal_unpaired_result <- NULL
  } else if (!is.null(normal_paired_result()$result)) {
     normal_paired_result <- NULL
  } else if (!is.null(not_normal_unpaired_result()$result)) {
     not_normal_unpaired_result <- NULL
  } else if (!is.null(not_normal_paired_result()$result)) {
     not_normal_paired_result <- NULL
  }

rmd_content_analysis_wilcoxon_test_paired <- readLines("markdown/07_analysis/analysis_wilcoxon_test_paired.Rmd")
processed_rmd_analysis_wilcoxon_test_paired <- whisker.render(paste(rmd_content_analysis_wilcoxon_test_paired, collapse = "\n"), vars)

  output$not_normal_paired_ui <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "not_normal_paired",
              title = "3️⃣ Not Normal Paired: Wilcoxon Test",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  HTML(markdownToHTML(text = processed_rmd_analysis_wilcoxon_test_paired, fragment.only = TRUE)),
                  uiOutput(session$ns("not_normal_paired_feedback"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("not_normal_paired")),
                  uiOutput(session$ns("save_not_normal_paired"))
                  )
              ),
              
            )
          ),
          
        ),
        
      )
  })
})

observe({
      req(!is.null(not_normal_paired_result()), !is.null(not_normal_paired_result()$result))

      if (inherits(not_normal_paired_result()$result, "htest")) {
        output$not_normal_paired_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
          )
        })
        output$save_not_normal_paired <- renderUI({
          tagList(
    div(
      style = "display: flex; justify-content: center; align-items: center; width: 100%;",
          actionButton(
            session$ns("save_not_normal_paired_button"),
            label = tagList(icon("save"), "Save Results to Dashboard"),
            class = "action-button custom-action",
            `data-id` = "not_normal_paired_save"

          )
    )
          )


        })
        } else {
        output$not_normal_paired_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
      })


observeEvent(input$save_not_normal_paired_button, {
  save_result(
    name             = "Wilcoxon Test - Paired",
    key              = "stats_not_normal_paired",
    result_obj       = not_normal_paired_result(), 
    saved_results    = saved_results,
    session_folder_id = session_folder_id
  )
})

# 3. normal unpaired

  predefined_code_normal_unpaired = whisker.render(
    read_file("markdown/07_analysis/predefined_code_two_sided_t_test.txt"),
    vars)
  normal_unpaired_result <- editor_module_server("normal_unpaired", average_trs, "average_trs", predefined_code = predefined_code_normal_unpaired, return_type = "result", session_folder_id, save_header = "Statistical Analysis: Normal Unpaired")

observeEvent(input$unpaired_normal,{
  output$not_normal_unpaired_ui <- renderUI({NULL})
  output$not_normal_paired_ui <- renderUI({NULL})
  #output$normal_unpaired_ui <- renderUI({NULL})
  output$normal_paired_ui <- renderUI({NULL})

  output$effect_size_t_test_paired <- renderUI({NULL})
  output$effect_size_t_test_unpaired <- renderUI({NULL})
  output$effect_size_wilcoxon_paired <- renderUI({NULL})
  output$effect_size_wilcoxon_unpaired <- renderUI({NULL})
  output$interpretation_quiz <- renderUI({NULL})
  output$enter_effect_size_feedback <- renderUI({NULL})
  output$enter_p_value_feedback <- renderUI({NULL})

  p_value_reactive <- reactive({NULL})
  effect_size_reactive <- reactive({NULL})

if(!is.null(normal_unpaired_result()$result)){
    normal_unpaired_result <- NULL
  } else if (!is.null(normal_paired_result()$result)) {
     normal_paired_result <- NULL
  } else if (!is.null(not_normal_unpaired_result()$result)) {
     not_normal_unpaired_result <- NULL
  } else if (!is.null(not_normal_paired_result()$result)) {
     not_normal_paired_result <- NULL
  }

rmd_content_analysis_two_sided_t_test <- readLines("markdown/07_analysis/analysis_two_sided_t_test.Rmd")
processed_rmd_analysis_two_sided_t_test <- whisker.render(paste(rmd_content_analysis_two_sided_t_test, collapse = "\n"), vars)

  output$normal_unpaired_ui <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "normal_unpaired",
              title = "3️⃣ Normal Unpaired: T-Test",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  HTML(markdownToHTML(text = processed_rmd_analysis_two_sided_t_test, fragment.only = TRUE)),
                  uiOutput(session$ns("normal_unpaired_feedback"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("normal_unpaired")),
                  uiOutput(session$ns("save_normal_unpaired"))
                  )
              ),
              
            )
          ),
          
        ),
        
      )
  })
})

observe({
      req(!is.null(normal_unpaired_result()), !is.null(normal_unpaired_result()$result))

      if (inherits(normal_unpaired_result()$result, "htest")) {
        output$normal_unpaired_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
          )
        })
        output$save_normal_unpaired <- renderUI({
          tagList(
            div(
              style = "display: flex; justify-content: center; align-items: center; width: 100%;",
          actionButton(
            session$ns("save_normal_unpaired_button"),
            label = tagList(icon("save"), "Save Results to Dashboard"),
            class = "action-button custom-action",
            `data-id` = "normal_unpaired_save"
          )
    )
          )

        })
        } else {
        output$normal_unpaired_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
      })

      observeEvent(input$save_normal_unpaired_button, {
  save_result(
    name             = "T-Test - Unpaired",
    key              = "stats_normal_unpaired",
    result_obj       = normal_unpaired_result(), 
    saved_results    = saved_results,
    session_folder_id = session_folder_id
  )
})

# 4. normal paired

  predefined_code_normal_paired = whisker.render(
    read_file("markdown/07_analysis/predefined_code_paired_t_test.txt"),
    vars)
  normal_paired_result <- editor_module_server("normal_paired", average_trs, "average_trs", predefined_code = predefined_code_normal_paired, return_type = "result", session_folder_id, save_header = "Statistical Analysis: Normal Paired")

observeEvent(input$paired_normal,{
  output$not_normal_unpaired_ui <- renderUI({NULL})
  output$not_normal_paired_ui <- renderUI({NULL})
  output$normal_unpaired_ui <- renderUI({NULL})
  #output$normal_paired_ui <- renderUI({NULL})

  output$effect_size_t_test_paired <- renderUI({NULL})
  output$effect_size_t_test_unpaired <- renderUI({NULL})
  output$effect_size_wilcoxon_paired <- renderUI({NULL})
  output$effect_size_wilcoxon_unpaired <- renderUI({NULL})
  output$interpretation_quiz <- renderUI({NULL})
  output$enter_effect_size_feedback <- renderUI({NULL})
  output$enter_p_value_feedback <- renderUI({NULL})

  p_value_reactive <- reactive({NULL})
  effect_size_reactive <- reactive({NULL})


  if(!is.null(normal_unpaired_result()$result)){
    normal_unpaired_result <- NULL
  } else if (!is.null(normal_paired_result()$result)) {
     normal_paired_result <- NULL
  } else if (!is.null(not_normal_unpaired_result()$result)) {
     not_normal_unpaired_result <- NULL
  } else if (!is.null(not_normal_paired_result()$result)) {
     not_normal_paired_result <- NULL
  }

rmd_content_analysis_paired_t_test <- readLines("markdown/07_analysis/analysis_paired_t_test.Rmd")
processed_rmd_analysis_paired_t_test <- whisker.render(paste(rmd_content_analysis_paired_t_test, collapse = "\n"), vars)

  output$normal_paired_ui <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "normal_paired",
              title = "3️⃣ Normal Paired: T-Test",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  HTML(markdownToHTML(text = processed_rmd_analysis_paired_t_test, fragment.only = TRUE)),
                  uiOutput(session$ns("normal_paired_feedback"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("normal_paired")),
                  uiOutput(session$ns("save_normal_paired"))
                  )
              ),
              
            )
          ),
          
        ),
        
      )
  })
})

observe({
      req(!is.null(normal_paired_result()), !is.null(normal_paired_result()$result))

      if (inherits(normal_paired_result()$result, "htest")) {
        output$normal_paired_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
          )
        })
        output$save_normal_paired <- renderUI({
          tagList(
            div(
              style = "display: flex; justify-content: center; align-items: center; width: 100%;",
          actionButton(
            session$ns("save_normal_paired_button"),
            label = tagList(icon("save"), "Save Results to Dashboard"),
            class = "action-button custom-action",
            `data-id` = "normal_paired_save"
          )
            )
          )

        })
        } else {
        output$normal_paired_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
      })

      observeEvent(input$save_normal_paired_button, {
  save_result(
    name             = "T-Test - Paired",
    key              = "stats_normal_paired",
    result_obj       = normal_paired_result(), 
    saved_results    = saved_results,
    session_folder_id = session_folder_id
  )
})

# 5. effect size t-test paired
  predefined_code_t_test_effect_size_paired = whisker.render(
    read_file("markdown/07_analysis/predefined_t_test_effect_size_paired.txt"),
    vars)
  t_test_effect_size_paired_result <- editor_module_server("t_test_effect_size_paired", average_trs, "average_trs", predefined_code = predefined_code_t_test_effect_size_paired, return_type = "result", session_folder_id, save_header = "Statistical Analysis: Effect Size for Paired T-Test")

observe({
  req(!is.null(normal_paired_result()), !is.null(normal_paired_result()$result),  p_value_reactive() < 0.05)

  output$not_normal_unpaired_ui <- renderUI({NULL})
  output$not_normal_paired_ui <- renderUI({NULL})
  output$normal_unpaired_ui <- renderUI({NULL})
  #output$normal_paired_ui <- renderUI({NULL})

  #output$effect_size_t_test_paired <- renderUI({NULL})
  output$effect_size_t_test_unpaired <- renderUI({NULL})
  output$effect_size_wilcoxon_paired <- renderUI({NULL})
  output$effect_size_wilcoxon_unpaired <- renderUI({NULL})
  output$interpretation_quiz <- renderUI({NULL})

  effect_size_reactive <- reactive({NULL})

rmd_content_analysis_effect_size_t_test_paired <- readLines("markdown/07_analysis/analysis_effect_size_t_test_paired.Rmd")
processed_rmd_analysis_effect_size_t_test_paired <- whisker.render(paste(rmd_content_analysis_effect_size_t_test_paired, collapse = "\n"), vars)

  output$effect_size_t_test_paired <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "effect_size_t_test_paired",
              title = "4️⃣ Effect Size for Paired T-Test",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  HTML(markdownToHTML(text = processed_rmd_analysis_effect_size_t_test_paired, fragment.only = TRUE)),
                  uiOutput(session$ns("t_test_effect_size_paired_feedback")),
                  ),
                  column(6,
                  editor_module_ui(session$ns("t_test_effect_size_paired")),
                  uiOutput(session$ns("save_normal_paired_effect_size"))

                  )
              ),
              
            )
          ),
          
        ),
        
      )
  })
})

observe({
      req(!is.null(t_test_effect_size_paired_result()), !is.null(t_test_effect_size_paired_result()$result))

      if (!is.null(t_test_effect_size_paired_result())) {
        output$t_test_effect_size_paired_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
          )
        })
        output$save_normal_paired_effect_size <- renderUI({
          tagList(
            div(
            style = "display: flex; justify-content: center; align-items: center; width: 100%;",
          actionButton(
            session$ns("save_normal_paired_effect_size_button"),
            label = tagList(icon("save"), "Save Results to Dashboard"),
            class = "action-button custom-action",
            `data-id` = "normal_paired_effect_save"
          )
            )
          )

        })

        } else {
        output$t_test_effect_size_paired_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
      })

observeEvent(input$save_normal_paired_effect_size_button, {
  save_result(
    name             = "Normal Effect Size - Paired",
    key              = "stats_normal_paired_effect_size",
    result_obj       = t_test_effect_size_paired_result(), 
    saved_results    = saved_results,
    session_folder_id = session_folder_id
  )
})


#6. effect size t-test unpaired
  predefined_code_t_test_effect_size_unpaired = whisker.render(
    read_file("markdown/07_analysis/predefined_t_test_effect_size_unpaired.txt"),
    vars)
  t_test_effect_size_unpaired_result <- editor_module_server("t_test_effect_size_unpaired", average_trs, "average_trs", predefined_code = predefined_code_t_test_effect_size_unpaired, return_type = "result", session_folder_id, save_header = "Statistical Analysis: Effect Size for Unpaired T-Test")

observe({
    req(!is.null(normal_unpaired_result()), !is.null(normal_unpaired_result()$result), p_value_reactive() < 0.05)

  output$not_normal_unpaired_ui <- renderUI({NULL})
  output$not_normal_paired_ui <- renderUI({NULL})
  #output$normal_unpaired_ui <- renderUI({NULL})
  output$normal_paired_ui <- renderUI({NULL})

  output$effect_size_t_test_paired <- renderUI({NULL})
  #output$effect_size_t_test_unpaired <- renderUI({NULL})
  output$effect_size_wilcoxon_paired <- renderUI({NULL})
  output$effect_size_wilcoxon_unpaired <- renderUI({NULL})
  output$interpretation_quiz <- renderUI({NULL})

  effect_size_reactive <- reactive({NULL})

rmd_content_analysis_effect_size_t_test_unpaired <- readLines("markdown/07_analysis/analysis_effect_size_t_test_unpaired.Rmd")
processed_rmd_analysis_effect_size_t_test_unpaired <- whisker.render(paste(rmd_content_analysis_effect_size_t_test_unpaired, collapse = "\n"), vars)

  output$effect_size_t_test_unpaired <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "effect_size_t_test_unpaired",
              title = "4️⃣ Effect Size for Unpaired T-Test",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  HTML(markdownToHTML(text = processed_rmd_analysis_effect_size_t_test_unpaired, fragment.only = TRUE)),
                  uiOutput(session$ns("t_test_effect_size_unpaired_feedback")),
                  ),
                  column(6,
                  editor_module_ui(session$ns("t_test_effect_size_unpaired")),
                  uiOutput(session$ns("save_normal_unpaired_effect_size"))
                  )
              ),
              
            )
          ),
          
        ),
        
      )
  })
})

observe({
      req(!is.null(t_test_effect_size_unpaired_result()), !is.null(t_test_effect_size_unpaired_result()$result))

      if (!is.null(t_test_effect_size_unpaired_result())) {
        output$t_test_effect_size_unpaired_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
          )
        })
        output$save_normal_unpaired_effect_size <- renderUI({
        tagList(
          div(
            style = "display: flex; justify-content: center; align-items: center; width: 100%;",
                  actionButton(
                    session$ns("save_normal_unpaired_effect_size_button"),
                    label = tagList(icon("save"), "Save Results to Dashboard"),
                    class = "action-button custom-action",
                    `data-id` = "normal_unpaired_effect_save"

                  )
          )
        )

                })
        } else {
        output$t_test_effect_size_unpaired_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
      })

      observeEvent(input$save_normal_unpaired_effect_size_button, {
  save_result(
    name             = "Normal Effect Size - Unpaired",
    key              = "stats_normal_unpaired_effect_size",
    result_obj       = t_test_effect_size_unpaired_result(), 
    saved_results    = saved_results,
    session_folder_id = session_folder_id
  )
})


#7. effect size wilcoxon paired
  predefined_code_wilcoxon_effect_size_paired = whisker.render(
    read_file("markdown/07_analysis/predefined_wilcoxon_effect_size_paired.txt"),
    vars)
  wilcoxon_effect_size_paired_result <- editor_module_server("wilcoxon_effect_size_paired", average_trs, "average_trs", predefined_code = predefined_code_wilcoxon_effect_size_paired, return_type = "result", session_folder_id, save_header = "Statistical Analysis: Effect Size for Paired T-Test")

observe({
  req(!is.null(not_normal_paired_result()), !is.null(not_normal_paired_result()$result), p_value_reactive() < 0.05)

  output$not_normal_unpaired_ui <- renderUI({NULL})
  #output$not_normal_paired_ui <- renderUI({NULL})
  output$normal_unpaired_ui <- renderUI({NULL})
  output$normal_paired_ui <- renderUI({NULL})

  output$effect_size_t_test_paired <- renderUI({NULL})
  output$effect_size_t_test_unpaired <- renderUI({NULL})
  #output$effect_size_wilcoxon_paired <- renderUI({NULL})
  output$effect_size_wilcoxon_unpaired <- renderUI({NULL})
  output$interpretation_quiz <- renderUI({NULL})

  effect_size_reactive <- reactive({NULL})

rmd_content_analysis_wilcoxon_test_paired_effect_size <- readLines("markdown/07_analysis/analysis_wilcoxon_test_paired_effect_size.Rmd")
processed_rmd_analysis_wilcoxon_test_paired_effect_size <- whisker.render(paste(rmd_content_analysis_wilcoxon_test_paired_effect_size, collapse = "\n"), vars)

  output$effect_size_wilcoxon_paired <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "effect_size_wilcoxon_paired",
              title = "4️⃣ Effect Size for Paired Wilcoxon Test",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  HTML(markdownToHTML(text = processed_rmd_analysis_wilcoxon_test_paired_effect_size, fragment.only = TRUE)),
                  uiOutput(session$ns("wilcoxon_effect_size_paired_feedback")),
                  ),
                  column(6,
                  editor_module_ui(session$ns("wilcoxon_effect_size_paired")),
                  uiOutput(session$ns("save_not_normal_paired_effect_size"))

                  )
              ),
              
            )
          ),
          
        ),
        
      )
  })
})

observe({
      req(!is.null(wilcoxon_effect_size_paired_result()), !is.null(wilcoxon_effect_size_paired_result()$result))

      if (!is.null(wilcoxon_effect_size_paired_result())) {
        output$wilcoxon_effect_size_paired_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
          )
        })
        output$save_not_normal_paired_effect_size <- renderUI({
            tagList(
                  div(
                    style = "display: flex; justify-content: center; align-items: center; width: 100%;",
                          actionButton(
                            session$ns("save_not_normal_paired_effect_size_button"),
                            label = tagList(icon("save"), "Save Results to Dashboard"),
                            class = "action-button custom-action",
                            `data-id` = "not_normal_paired_effect_save"

                          )
                  )
            )

                        })
        } else {
        output$wilcoxon_effect_size_paired_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
      })

  observeEvent(input$save_not_normal_paired_effect_size_button, {
  save_result(
    name             = "Not Normal Effect Size - Paired",
    key              = "stats_not_normal_paired_effect_size",
    result_obj       = wilcoxon_effect_size_paired_result(), 
    saved_results    = saved_results,
    session_folder_id = session_folder_id
  )
})

#8. effect size wilcoxon unpaired
  predefined_code_wilcoxon_effect_size_unpaired = whisker.render(
    read_file("markdown/07_analysis/predefined_wilcoxon_effect_size_unpaired.txt"),
    vars)
    
  wilcoxon_effect_size_unpaired_result <- editor_module_server("wilcoxon_effect_size_unpaired", average_trs, "average_trs", predefined_code = predefined_code_wilcoxon_effect_size_unpaired, return_type = "result", session_folder_id, save_header = "Statistical Analysis: Effect Size for Unpaired T-Test")

observe({
  req(!is.null(not_normal_unpaired_result()), !is.null(not_normal_unpaired_result()$result), p_value_reactive() < 0.05)

  #output$not_normal_unpaired_ui <- renderUI({NULL})
  output$not_normal_paired_ui <- renderUI({NULL})
  output$normal_unpaired_ui <- renderUI({NULL})
  output$normal_paired_ui <- renderUI({NULL})

  output$effect_size_t_test_paired <- renderUI({NULL})
  output$effect_size_t_test_unpaired <- renderUI({NULL})
  output$effect_size_wilcoxon_paired <- renderUI({NULL})
  #output$effect_size_wilcoxon_unpaired <- renderUI({NULL})
  output$interpretation_quiz <- renderUI({NULL})

  effect_size_reactive <- reactive({NULL})

rmd_content_analysis_wilcoxon_test_unpaired_effect_size <- readLines("markdown/07_analysis/analysis_wilcoxon_test_unpaired_effect_size.Rmd")
processed_rmd_analysis_wilcoxon_test_unpaired_effect_size <- whisker.render(paste(rmd_content_analysis_wilcoxon_test_unpaired_effect_size, collapse = "\n"), vars)

  output$effect_size_wilcoxon_paired <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "effect_size_wilcoxon_unpaired",
              title = "4️⃣ Effect Size for Unpaired Wilcoxon Test",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  HTML(markdownToHTML(text = processed_rmd_analysis_wilcoxon_test_unpaired_effect_size, fragment.only = TRUE)),
                  uiOutput(session$ns("wilcoxon_effect_size_unpaired_feedback")),
                  ),
                  column(6,
                  editor_module_ui(session$ns("wilcoxon_effect_size_unpaired")),
                  uiOutput(session$ns("save_not_normal_unpaired_effect_size"))

                  )
              ),
              
            )
          ),
          
        ),
        
      )
  })
})

observe({
      req(!is.null(wilcoxon_effect_size_unpaired_result()), !is.null(wilcoxon_effect_size_unpaired_result()$result))

      if (!is.null(wilcoxon_effect_size_unpaired_result())) {
        output$wilcoxon_effect_size_unpaired_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
          )
        })
        output$save_not_normal_unpaired_effect_size <- renderUI({
            tagList(
              div(
                style = "display: flex; justify-content: center; align-items: center; width: 100%;",
                                  actionButton(
                                    session$ns("save_not_normal_unpaired_effect_size_button"),
                                    label = tagList(icon("save"), "Save Results to Dashboard"),
                                    class = "action-button custom-action",
                                    `data-id` = "not_normal_unpaired_effect_save"

                                  )
              )
            )

                                })
        } else {
        output$wilcoxon_effect_size_unpaired_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
      })

      observeEvent(input$save_not_normal_unpaired_effect_size_button, {
        save_result(
          name             = "Not Normal Effect Size - Unpaired",
          key              = "stats_not_normal_unpaired_effect_size",
          result_obj       = wilcoxon_effect_size_unpaired_result(), 
          saved_results    = saved_results,
          session_folder_id = session_folder_id
        )
      })

### reactives for effect size

   effect_size_reactive <- reactive({
    sr_es <- NULL
    if (!is.null(t_test_effect_size_paired_result()) && 
      !is.null(t_test_effect_size_paired_result()$result)) {
    sr_es <- t_test_effect_size_paired_result()
  } else if (!is.null(t_test_effect_size_unpaired_result()) && 
             !is.null(t_test_effect_size_unpaired_result()$result)) {
    sr_es <- t_test_effect_size_unpaired_result()
  } else if (!is.null(wilcoxon_effect_size_unpaired_result()) && 
             !is.null(wilcoxon_effect_size_unpaired_result()$result)) {
    sr_es <- wilcoxon_effect_size_unpaired_result()
  } else if (!is.null(wilcoxon_effect_size_paired_result()) && 
             !is.null(wilcoxon_effect_size_paired_result()$result)) {
    sr_es <- wilcoxon_effect_size_paired_result()
  }

print(sr_es)

      if (is.null(sr_es) || is.null(sr_es$result)) {
        return(NULL)
      }
      if (!tibble::is_tibble(sr_es$result)) {
        return(NULL)
      }
      df_effect_size <- sr_es$result %>%
        dplyr::select("effsize")
      
      if (nrow(df_effect_size) == 0) {
        return(NULL)
      }
      as.numeric(df_effect_size$effsize[1] %>% unname())
    })


  effect_size_reactive_round <- reactive({
    effect_size_reactive_round_val <- effect_size_reactive()
    if (is.null(effect_size_reactive_round_val) || !is.numeric(effect_size_reactive_round_val)) {
    return(NULL)
  }
  round(effect_size_reactive_round_val, 2)
})

# reactives for p-value

p_value_reactive <- reactive({
    sr <- NULL
    if (!is.null(normal_unpaired_result()) && 
      !is.null(normal_unpaired_result()$result)) {
    sr <- normal_unpaired_result()

  } else if (!is.null(normal_paired_result()) && 
             !is.null(normal_paired_result()$result)) {
    sr <- normal_paired_result()
  } else if (!is.null(not_normal_paired_result()) && 
             !is.null(not_normal_paired_result()$result)) {
    sr <- not_normal_paired_result()
  } else if (!is.null(not_normal_unpaired_result()) && 
             !is.null(not_normal_unpaired_result()$result)) {
    sr <- not_normal_unpaired_result()
  }
      
      if (is.null(sr) || is.null(sr$result)) {
        return(NULL)
      }
      if (!("p.value" %in% names(sr$result))) {
        return(NULL)
      }

      p_value <- sr$result$p.value
      
      if (is.null(p_value)) {
        return(NULL)
      }
      as.numeric(p_value)
    })

  p_value_round <- reactive({
    p_value_round_val <- p_value_reactive()
    if (is.null(p_value_round_val) || !is.numeric(p_value_round_val)) {
    return(NULL)
  }
  round(p_value_round_val, 2)
})

# 9. interpretation quiz
observe({

req(!is.null(p_value_reactive()))

    if (p_value_reactive() < 0.05) {
    req(
      (!is.null(t_test_effect_size_paired_result()) && !is.null(t_test_effect_size_paired_result()$result)) ||
      (!is.null(t_test_effect_size_unpaired_result()) && !is.null(t_test_effect_size_unpaired_result()$result)) ||
      (!is.null(wilcoxon_effect_size_paired_result()) && !is.null(wilcoxon_effect_size_paired_result()$result)) ||
      (!is.null(wilcoxon_effect_size_unpaired_result()) && !is.null(wilcoxon_effect_size_unpaired_result()$result))
    )
  } else {
    req(
      (!is.null(normal_paired_result()) && !is.null(normal_paired_result()$result)) ||
      (!is.null(normal_unpaired_result()) && !is.null(normal_unpaired_result()$result)) ||
      (!is.null(not_normal_paired_result()) && !is.null(not_normal_paired_result()$result)) ||
      (!is.null(not_normal_unpaired_result()) && !is.null(not_normal_unpaired_result()$result))
    )
  }

  output$interpretation_quiz <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "interpretation_quiz",
              title = "5️⃣ Your Results",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(12,
                  uiOutput(session$ns("interpretation_quiz_feedback")),
                  )
              ),
              
            )
          ),
          
        ),
        
      )
  })
})

observe({

  req(!is.null(p_value_reactive()))

  effect_size_exists <- 
    !is.null(t_test_effect_size_paired_result()$result) ||
    !is.null(t_test_effect_size_unpaired_result()$result) ||
    !is.null(wilcoxon_effect_size_paired_result()$result) ||
    !is.null(wilcoxon_effect_size_unpaired_result()$result)

  normality_results_exist <- 
    !is.null(normal_paired_result()$result) ||
    !is.null(normal_unpaired_result()$result) ||
    !is.null(not_normal_paired_result()$result) ||
    !is.null(not_normal_unpaired_result()$result)

  if (p_value_reactive() < 0.05) {
    req(effect_size_exists)
  } else {
    req(normality_results_exist)
  }

  if (effect_size_exists) {
    output$interpretation_quiz_feedback <- renderUI({
      tagList(
        numericInput(session$ns("enter_p_value"), "What is the p-value?", value = 0),
        div(
            style = "text-align: center;",
            actionButton(session$ns("enter_p_value_submit"), "Submit", class = "fun-submit-button")
        ),
        uiOutput(session$ns("enter_p_value_feedback")),
        uiOutput(session$ns("null_hyp_display")),

        numericInput(session$ns("enter_effect_size"), "What is the effect size?", value = 0),
        div(
            style = "text-align: center;",
            actionButton(session$ns("enter_effect_size_submit"), "Submit", class = "fun-submit-button")
        ),
        uiOutput(session$ns("enter_effect_size_feedback")),
        uiOutput(session$ns("effect_size_display")),
        
        textInput(session$ns("interpretation_quiz_text_p_value"), 
                  "Interpret the p-value result in one sentence", 
                  value = "A p-value of [statisical test method + degrees of freedom], p=[p-value] suggests that ______.", 
                  width = "100%"),
        
        textInput(session$ns("interpretation_quiz_text_effect_size"), 
                  "Summarise these results in one sentence", 
                  value = "An effect size of [effect size method]=[effect size] suggests that ______.", 
                  width = "100%"),

        div(
            style = "text-align: center;",
            actionButton(session$ns("save_text_interpretation_button"), 
                         tagList(shiny::icon("save"), "Save Notes"), 
                         class = "fun-save-button")
        )
      )
    })
  
  } else if (normality_results_exist) {
    output$interpretation_quiz_feedback <- renderUI({
      tagList(
        numericInput(session$ns("enter_p_value"), "What is the p-value?", value = 0),
        div(
            style = "text-align: center;",
            actionButton(session$ns("enter_p_value_submit"), "Submit", class = "fun-submit-button")
        ),
        uiOutput(session$ns("enter_p_value_feedback")),
        uiOutput(session$ns("null_hyp_display")),
        
        textInput(session$ns("interpretation_quiz_text_p_value"), 
                  "Interpret the p-value result in one sentence", 
                  value = "A p-value of [statisical test method + degrees of freedom], p=[p-value] suggests that ______.", 
                  width = "100%"),

        div(
            style = "text-align: center;",
            actionButton(session$ns("save_text_interpretation_button"), 
                         tagList(shiny::icon("save"), "Save Notes"), 
                         class = "fun-save-button")
        )
      )
    })
  
  } else {
    output$interpretation_quiz_feedback <- renderUI({
      div(class = "error-box", "\U1F914 Not quite - try again!")
    })
  }

})

observeEvent(input$enter_effect_size_submit, {
     req(
    (!is.null(t_test_effect_size_paired_result()) && !is.null(t_test_effect_size_paired_result()$result)) ||
    (!is.null(t_test_effect_size_unpaired_result()) && !is.null(t_test_effect_size_unpaired_result()$result)) ||
    (!is.null(wilcoxon_effect_size_paired_result()) && !is.null(wilcoxon_effect_size_paired_result()$result)) ||
    (!is.null(wilcoxon_effect_size_unpaired_result()) && !is.null(wilcoxon_effect_size_unpaired_result()$result))
  )

  if (!is.null(t_test_effect_size_paired_result()$result) || !is.null(t_test_effect_size_unpaired_result()$result) || 
  !is.null(wilcoxon_effect_size_paired_result()$result) || !is.null(wilcoxon_effect_size_unpaired_result()$result) ){

    val_es <- effect_size_reactive_round()

  } else {
    val_es <- NULL
  }
  
  if (is.null(val_es)) {
    output$enter_effect_size_feedback <- renderUI({
      div(class = "error-box", "\U1F914 We do not know the effect size yet!")
    })
    return()
  }

  user_answer_enter_effect_size <- as.numeric(input$enter_effect_size)

rmd_content_analysis_what_is_an_effect_size <- readLines("markdown/07_analysis/analysis_what_is_an_effect_size.Rmd")
processed_rmd_analysis_what_is_an_effect_size <- whisker.render(paste(rmd_content_analysis_what_is_an_effect_size, collapse = "\n"), vars)

  if (is.na(user_answer_enter_effect_size)) {
    feedback <- div(class = "error-box", "\U1F914 Please enter a numeric value!")
  } else {
    tolerance <- 0.5
    
    if (abs(user_answer_enter_effect_size - val_es) <= tolerance) {
      feedback <- div(class = "success-box", "\U1F64C Correct!")

      output$effect_size_display <- renderUI({
        tagList(
          HTML(markdownToHTML(text = processed_rmd_analysis_what_is_an_effect_size, fragment.only = TRUE)),
          radioButtons(
              inputId = (session$ns("understand_effect_size")),
              label = "Do you think this effect size?",
              choices = c("Negligible" = "negligible", "Small" = "small", "Medium" = "medium", "Large" = "large"),
              selected = character(0)
            ),
          uiOutput(session$ns("understanding_effect_size_feedback"))
        )
       
      })
    } else {
      feedback <- div(class = "error-box", "\U1F914 Not quite - try again!")
    }
  }

  output$enter_effect_size_feedback <- renderUI({
    feedback
  })

  output$understanding_effect_size_feedback <- renderUI({
    req(input$understand_effect_size)
    
  correct_answer <- ifelse(
    user_answer_enter_effect_size < 0.2, "negligible",
    ifelse(user_answer_enter_effect_size < 0.5, "small",
           ifelse(user_answer_enter_effect_size < 0.8, "medium", "large")))
      
    if (input$understand_effect_size == correct_answer) {
      div(class = "success-box", "\U1F64C Correct!")
    } else {
      div(class = "error-box", "\U1F914 Not quite - try again!")
    }
  })

})


  files_in_folder <- drive_ls(as_id(session_folder_id))

  null_hyp_file <- files_in_folder[grepl("^Null Hypothesis", files_in_folder$name), ]

  if (nrow(null_hyp_file) > 0) {
    temp_file <- tempfile(fileext = ".txt")
    drive_download(as_id(null_hyp_file$id), path = temp_file, overwrite = TRUE)
    
    submitted_null_hyp <- readLines(temp_file, warn = FALSE) %>% paste(collapse = "\n")
  } else {
    submitted_null_hyp <- "Please submit your null hypothesis in the hypothesis section."
  }

alt_hyp_file <- files_in_folder[grepl("^Alternative Hypothesis", files_in_folder$name), ]

  if (nrow(alt_hyp_file) > 0) {
    temp_file <- tempfile(fileext = ".txt")
    drive_download(as_id(alt_hyp_file$id), path = temp_file, overwrite = TRUE)
    
    submitted_alt_hyp <- readLines(temp_file, warn = FALSE) %>% paste(collapse = "\n")
  } else {
    submitted_alt_hyp <- "Please submit your alternative hypothesis in the hypothesis section."
  }



observeEvent(input$enter_p_value_submit, {
    req(
    (!is.null(normal_paired_result()) && !is.null(normal_paired_result()$result)) ||
    (!is.null(normal_unpaired_result()) && !is.null(normal_unpaired_result()$result)) ||
    (!is.null(not_normal_paired_result()) && !is.null(not_normal_paired_result()$result)) ||
    (!is.null(not_normal_unpaired_result()) && !is.null(not_normal_unpaired_result()$result))
    
  )

  if (!is.null(normal_paired_result()$result) || !is.null(normal_unpaired_result()$result)
  || !is.null(not_normal_paired_result()$result) || !is.null(not_normal_unpaired_result()$result)){
    val_pv <- p_value_reactive()
  }
  else {
    val_pv <- NULL
  }
  
  if (is.null(val_pv)) {
    output$enter_p_value_feedback <- renderUI({
      div(class = "error-box", "\U1F914 We do not know the p-value yet!")
    })
    return()
  }

  user_answer_enter_p_value <- as.numeric(input$enter_p_value)

rmd_content_analysis_what_is_a_p_value <- readLines("markdown/07_analysis/analysis_what_is_a_p_value.Rmd")
processed_rmd_analysis_what_is_a_p_value <- whisker.render(paste(rmd_content_analysis_what_is_a_p_value, collapse = "\n"), vars)

  if (is.na(user_answer_enter_p_value)) {
    feedback <- div(class = "error-box", "\U1F914 Please enter a numeric value!")
  } else {
    tolerance <- 0.5
    
    if (abs(user_answer_enter_p_value - val_pv) <= tolerance) {
      feedback <- div(class = "success-box", "\U1F64C Correct!")

      output$null_hyp_display <- renderUI({
        tagList(
          HTML(markdownToHTML(text = processed_rmd_analysis_what_is_a_p_value, fragment.only = TRUE)),
          strong("For this experiment your null hypothesis was: "),
          tags$br(),
          submitted_null_hyp,
          tags$br(),
          strong("Your alternative hypothesis was: "),
          tags$br(),
          submitted_alt_hyp,
          tags$br(),
          tags$br(),
          radioButtons(
              inputId = (session$ns("reject_null")),
              label = "Do you think we can reject the null hypothesis and accept the alternative hypothesis with this p-value?",
              choices = c("Yes" = "yes", "No" = "no"),
              selected = character(0)
            ),
          uiOutput(session$ns("reject_null_feedback"))
        )
       
      })

    } else {
      feedback <- div(class = "error-box", "\U1F914 Not quite - try again!")
    }
  }

  output$enter_p_value_feedback <- renderUI({
    feedback
  })

  
  output$reject_null_feedback <- renderUI({
    req(input$reject_null)
    
    correct_answer <- ifelse(user_answer_enter_p_value < 0.05, "yes", "no")
    
    if (input$reject_null == correct_answer) {
      div(class = "success-box", "\U1F64C Correct!")
    } else {
      div(class = "error-box", "\U1F914 Not quite - try again!")
    }
  })

    
})



# observeEvent(input$save_text_interpretation_button, {
#   req(nzchar(input$interpretation_quiz_text_p_value) || nzchar(input$interpretation_quiz_text_effect_size))

#   if (nzchar(input$interpretation_quiz_text_p_value) || nzchar(input$interpretation_quiz_text_effect_size)) {

observeEvent(input$save_text_interpretation_button, {
  req(nzchar(input$interpretation_quiz_text_p_value) || nzchar(input$interpretation_quiz_text_effect_size))

  req(!is.null(p_value_reactive()))

  effect_size_exists <- 
    !is.null(t_test_effect_size_paired_result()$result) ||
    !is.null(t_test_effect_size_unpaired_result()$result) ||
    !is.null(wilcoxon_effect_size_paired_result()$result) ||
    !is.null(wilcoxon_effect_size_unpaired_result()$result)

  normality_results_exist <- 
    !is.null(normal_paired_result()$result) ||
    !is.null(normal_unpaired_result()$result) ||
    !is.null(not_normal_paired_result()$result) ||
    !is.null(not_normal_unpaired_result()$result)

  if (p_value_reactive() < 0.05) {
    req(effect_size_exists)
  } else {
    req(normality_results_exist)
  }

  if (effect_size_exists &&
  nzchar(input$interpretation_quiz_text_p_value) &&
  nzchar(input$interpretation_quiz_text_effect_size)
  ) {
      
  interpretation_text <- paste0(
    "Interpretation: ", input$interpretation_quiz_text_p_value, ". ", input$interpretation_quiz_text_effect_size, "."
  )

  saved_results$user_writing[["stats_interpretation_text"]] <- interpretation_text

  temp_file <- tempfile(fileext = ".txt")
  writeLines(interpretation_text, con = temp_file)

  path <- drive_get(as_id(session_folder_id))
  drive_upload(
    media = temp_file,
    path = path,
    name = "stats_interpretation_text.txt",
    overwrite = TRUE
  )

  unlink(temp_file)

  showNotification("Interpretation saved successfully.", type = "message", duration = 3)
  

    } else if (normality_results_exist &&
  nzchar(input$interpretation_quiz_text_p_value)) {
    
  interpretation_text <- paste0(
  "Interpretation: ", input$interpretation_quiz_text_p_value, ". "
  )

  saved_results$user_writing[["stats_interpretation_text"]] <- interpretation_text

  temp_file <- tempfile(fileext = ".txt")
  writeLines(interpretation_text, con = temp_file)

  path <- drive_get(as_id(session_folder_id))
  drive_upload(
    media = temp_file,
    path = path,
    name = "stats_interpretation_text.txt",
    overwrite = TRUE
  )

  unlink(temp_file)

  showNotification("Interpretation saved successfully.", type = "message", duration = 3)
  



    } else {
      output$interpretation_quiz_text_input_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
    }
    
    })

# observeEvent(input$save_text_interpretation_button, {
#   req(nzchar(input$interpretation_quiz_text_p_value) && nzchar(input$interpretation_quiz_text_effect_size))

  
# })

observeEvent(input$back_page_stats, {
        updateTabItems(parent.session, "sidebar", "Create_Figure")
      })
      observeEvent(input$next_page_stats, {
        updateTabItems(parent.session, "sidebar", "Writing_Up_Advice")
      })

observeEvent(input$summarise, {
      updateTabItems(parent.session, "sidebar", "Summarise_Data")
    })
   
    observeEvent(input$figure, {
      updateTabItems(parent.session, "sidebar", "Create_Figure")
    })
    
    observeEvent(input$dashboard, {
      updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
    })

})}