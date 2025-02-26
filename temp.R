observe({
req(!is.null(effect_size_t_test_paired()), !is.null(effect_size_t_test_paired()$result))

  output$interpretation_quiz <- renderUI({
    tagList(
    fluidRow(
        column(
            12,
          box(
              id = "interpretation_quiz",
              title = "Your Results",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(12,
                  #includeMarkdown("markdown/07_analysis/analysis_effect_size_t_test_paired.Rmd"),
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
      req(!is.null(t_test_effect_size_paired_result()), !is.null(t_test_effect_size_paired_result()$result))

      if (!is.null(t_test_effect_size_paired_result())) {
        output$interpretation_quiz_feedback <- renderUI({
          tagList(
            div(class = "success-box", "\U1F64C Great!"),
            numericInput(session$ns("enter_p_value"), "What is the p-value?", value = 0),
            actionButton(session$ns("enter_p_value_submit"), "Submit", class = "fun-submit-button"),
            uiOutput(session$ns("enter_p_value_feedback")),

            numericInput(session$ns("enter_effect_size"), "What is the effect size?", value = 0),
            actionButton(session$ns("enter_effect_size_submit"), "Submit", class = "fun-submit-button"),
            uiOutput(session$ns("enter_effect_size_feedback"))
          )
        })

        } else {
        output$interpretation_quiz_feedback <- renderUI({
          div(class = "error-box", "\U1F914 Not quite - try again!")
        })
        }
})

observeEvent(input$enter_effect_size_submit, {
    req(!is.null(t_test_effect_size_paired_result()), !is.null(t_test_effect_size_paired_result()$result))


  if (!is.null(t_test_effect_size_paired_result()), !is.null(t_test_effect_size_paired_result()$result)){
      val <- t_test_paired_effect_size_round()

  } else {
    val <- NULL
  }
  
  if (is.null(val)) {
    output$enter_effect_size_feedback <- renderUI({
      div(class = "error-box", "\U1F914 We do not know the effect size yet!")
    })
    return()
  }

  user_answer_enter_effect_size <- as.numeric(input$enter_effect_size)

  if (is.na(user_answer_enter_effect_size)) {
    feedback <- div(class = "error-box", "\U1F914 Please enter a numeric value!")
  } else {
    tolerance <- 0.5
    
    if (abs(user_answer_enter_effect_size - val) <= tolerance) {
      feedback <- div(class = "success-box", "\U1F64C Correct!")
    } else {
      feedback <- div(class = "error-box", "\U1F914 Not quite - try again!")
    }
  }

  output$enter_effect_size_feedback <- renderUI({
    feedback
  })
})




t_test_effect_size <- reactive({

    sr <- NULL

    if (!is.null(t_test_effect_size_paired_result()) && is.null(t_test_effect_size_unpaired_result())){
        sr <- t_test_effect_size_paired_result()

    } else if (!is.null(t_test_effect_size_unpaired_result()) && is.null(t_test_effect_size_paired_result())) {
        sr <- t_test_effect_size_unpaired_result()
    }
      
      if (is.null(sr) || is.null(sr$result)) {
        return(NULL)
      }
      if (!tibble::is_tibble(sr$result)) {
        return(NULL)
      }
     
      df_effect_size <- sr$result %>%
        dplyr::select("effsize")
      
      if (nrow(df_effect_size) == 0) {
        return(NULL)
      }
      
      as.numeric(df_effect_size$effsize[1] %>% unname())

    })


  t_test_effect_size_round <- reactive({
    t_test_effect_size_round_val <- t_test_effect_size()
    
    if (is.null(t_test_effect_size_round_val) || !is.numeric(t_test_effect_size_round_val)) {
    return(NULL)
  }
  
  round(t_test_effect_size_round_val, 2)
})


##old
t_test_paired_effect_size <- reactive({
      sr <- t_test_effect_size_paired_result()
      
      if (is.null(sr) || is.null(sr$result)) {
        return(NULL)
      }
      if (!tibble::is_tibble(sr$result)) {
        return(NULL)
      }
     
      df_effect_size <- sr$result %>%
        dplyr::select("effsize")
      
      if (nrow(df_effect_size) == 0) {
        return(NULL)
      }
      
      as.numeric(df_effect_size$effsize[1] %>% unname())

    })


  t_test_paired_effect_size_round <- reactive({
    t_test_paired_effect_size_round_val <- t_test_paired_effect_size()
    
    if (is.null(t_test_paired_effect_size_round_val) || !is.numeric(t_test_paired_effect_size_round_val)) {
    return(NULL)
  }
  
  round(t_test_paired_effect_size_round_val, 2)
})