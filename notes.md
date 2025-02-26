  predefined_code_t_test_effect_size_unpaired = read_file("markdown/07_analysis/predefined_t_test_effect_size_unpaired.txt")
  t_test_effect_size_unpaired_result <- editor_module_server("t_test_effect_size_unpaired", average_trs, "average_trs", predefined_code = predefined_code_t_test_effect_size_unpaired, return_type = "result", session_folder_id, save_header = "Statistical Analysis: Effect Size for Unpaired T-Test")

observe({
    req(!is.null(normal_unpaired_result()), !is.null(normal_unpaired_result()$result))

  output$effect_size_t_test_unpaired <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "effect_size_t_test_unpaired",
              title = "Effect Size for Unpaired T-Test",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_effect_size_t_test_unpaired.Rmd"),
                  uiOutput(session$ns("t_test_effect_size_unpaired_feedback")),
                  ),
                  column(6,
                  editor_module_ui(session$ns("t_test_effect_size_unpaired"))
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

        } else {
        output$t_test_effect_size_unpaired_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
      })
