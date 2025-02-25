##jjjjj
    stressed_mean_reactive <- reactive({
      sr <- summarise_result_step5()$result
      
      if (is.null(sr) || is.null(sr$result)) {
        return(NULL)
      }
      if (!tibble::is_tibble(sr$result)) {
        return(NULL)
      }
      if ( stressed_mean() == sr$result ) {
        return(NULL)
      }
      
    })


    stressed_mean_round <- reactive({
    stressed_mean_round_val <- stressed_mean()
    if (is.null(stressed_mean_round_val)) return(NULL)
    round(stressed_mean_round_val, 2)
  })

 
    
observeEvent(input$submit_mean_stressed_group_quiz_answer, {
  val <- stressed_mean_round()

  if (is.null(val)) {
    output$mean_stressed_group_quiz_feedback <- renderUI({
      div(class = "error-box", "\U1F914 We do not have a valid mean yet!")
    })
    return()
  }

  user_answer_mean_stressed <- as.numeric(input$mean_stressed_group_quiz)

  if (is.na(user_answer_mean_stressed)) {
    feedback <- div(class = "error-box", "\U1F914 Please enter a numeric value!")
  } else {
    tolerance <- 0.5
    
    if (abs(user_answer_mean_stressed - val) <= tolerance) {
      feedback <- div(class = "success-box", "\U1F64C Correct!")
    } else {
      feedback <- div(class = "error-box", "\U1F914 Not quite - try again!")
    }
  }

  output$mean_stressed_group_quiz_feedback <- renderUI({
    feedback
  })
})

    #for stressed sem
    stressed_sem <- reactive({
      sr <- summarise_result_step5()

  if (is.null(sr) || is.null(sr$result)) {
    return(NULL)
  }
  if (!tibble::is_tibble(sr$result)) {
    return(NULL)
  }
  if (!all(c("Stress_Status", "sem") %in% names(sr$result))) {
    return(NULL)
  }

  df_stressed <- sr$result %>%
    dplyr::filter(Stress_Status == "Stressed")

  if (nrow(df_stressed) == 0) {
    return(NULL)
  }

  df_stressed$sem[1]
})

stressed_sem_round <- reactive({
  val <- stressed_sem()
  if (is.null(val)) return(NULL)
  round(val, 2)
})

observeEvent(input$submit_sem_stressed_group_quiz_answer, {
  val <- stressed_sem_round()
  
  if (is.null(val)) {
    output$submit_sem_stressed_group_quiz_feedback <- renderUI({
      div(class = "error-box", "\U1F914 We do not have a valid SEM yet!")
    })
    return()
  }
  
  user_answer_sem_stressed <- as.numeric(input$sem_stressed_group_quiz)
  
  if (is.na(user_answer_sem_stressed)) {
    feedback <- div(class = "error-box", "\U1F914 Please enter a numeric value!")
  } else {
    tolerance <- 0.1
    
    if (abs(user_answer_sem_stressed - val) <= tolerance) {
      feedback <- div(class = "success-box", "\U1F64C Correct!")
    } else {
      feedback <- div(class = "error-box", "\U1F914 Not quite - try again!")
    }
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



    ####

    predefined_code_step6 = read_file("markdown/07_analysis/predefined_code_calculate_dplyr.txt")
summarise_result_step6 <- editor_module_server("step6_editor",average_trs, "average_trs", predefined_code_step6, "result", session_folder_id, "Step 6: Summarise Data with Dplyr")

output$step6_box <- renderUI({
      req(!is.null(summarise_result_step5()), !is.null(summarise_result_step5()$result))
  tagList(
    fluidRow(
        column(
            12,
          box(
              id = "step6_box",
              title = "Step 6: Use Dplyr to quickly summarise your data",
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                  column(6,
                  includeMarkdown("markdown/07_analysis/analysis_summarise_data_dplyr.Rmd"),
                  uiOutput(session$ns("summary_code_feedback_step6"))
                  ),
                  column(6,
                  editor_module_ui(session$ns("step6_editor"))
                  )
              )
            )
          ),
          
        )
      )
    })

observe({
  req(!is.null(summarise_result_step6()), !is.null(summarise_result_step6()$result))
  
  if (tibble::is_tibble(summarise_result()$result)) {
    
    output$summary_code_feedback_step6 <- renderUI({
      tagList(
        div(class = "success-box", "\U1F64C Great!")
      )
    })
    
  } else {
    
    output$summary_code_feedback_step6 <- renderUI({
      div(class = "error-box", "\U1F914 Not quite - try again!")
    })
    
  }
})