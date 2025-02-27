uiOutput(session$ns("save_not_normal_unpaired"))


output$save_not_normal_unpaired <- renderUI({
      actionButton(
        session$ns("save_not_normal_unpaired_button"),
        label = tagList(icon("save"), "Save Results to Dashboard"),
        class = "action-button custom-action"
      )
    })


observeEvent(input$save_not_normal_unpaired_button, {
     
        name <- "Wilcoxon Test - Unpaired"
        key <- "stats_not_normal_unpaired"
        saved_results$scripts[[key]] <- not_normal_unpaired_result()
        
        result_as_char <- capture.output(print(saved_results$scripts[[key]]))
        
        temp_file <- tempfile(fileext = ".txt")
        writeLines(result_as_char, con = temp_file)
        
        path <- drive_get(as_id(session_folder_id))
        
        drive_upload(
          media = temp_file,
          path = path,
          name = paste0(key, ".txt"),
          overwrite = TRUE,
        )
        
        unlink(temp_file)

        showNotification("Wilcoxon Test - Unpaired result saved successfully.", type = "message")
        showNotification("[Name] result saved successfully.", type = "message")
     
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

  showNotification(paste0(name, " result saved successfully."), type = "message")
}

observeEvent(input$save_not_normal_unpaired_button, {
  save_result(
    name             = "Wilcoxon Test - Unpaired",
    key              = "stats_not_normal_unpaired",
    result_obj       = not_normal_unpaired_result(), 
    saved_results    = saved_results,
    session_folder_id = session_folder_id
  )
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



output$save_normal_paired_effect_size <- renderUI({
          actionButton(
            session$ns("save_normal_paired_effect_size_button"),
            label = tagList(icon("save"), "Save Results to Dashboard"),
            class = "action-button custom-action"
          )

        })


observeEvent(input$save_normal_paired_effect_size_button, {
  save_result(
    name             = "Normal Effect Size - Unpaired",
    key              = "stats_normal_unpaired_effect_size",
    result_obj       = t_test_effect_size_paired_result(), 
    saved_results    = saved_results,
    session_folder_id = session_folder_id
  )
})


stats_not_normal_unpaired
stats_not_normal_paired
stats_normal_unpaired
stats_normal_paired
stats_normal_paired_effect_size
stats_normal_unpaired_effect_size
stats_not_normal_paired_effect_size
stats_not_normal_unpaired_effect_size


stats_content_reactive <- reactive({
      req(!is.null(saved_results$scripts[["stats_not_normal_unpaired"]]) || 
        !is.null(saved_results$scripts[["stats_not_normal_paired"]]) ||
        !is.null(saved_results$scripts[["stats_normal_unpaired"]]) ||
        !is.null(saved_results$scripts[["stats_normal_paired"]]) ||
        !is.null(saved_results$scripts[["stats_normal_paired_effect_size"]]) ||
        !is.null(saved_results$scripts[["stats_normal_unpaired_effect_size"]]) ||
        !is.null(saved_results$scripts[["stats_not_normal_paired_effect_size"]]) ||
        !is.null(saved_results$scripts[["stats_not_normal_unpaired_effect_size"]])

      )
      
      if (!is.null(saved_results$scripts[["stats_not_normal_unpaired"]])) {
        paste(capture.output(saved_results$scripts[["stats_not_normal_unpaired"]]$result), collapse = "\n")
      } else if (!is.null(saved_results$scripts[["stats_not_normal_paired"]])) {
        paste(capture.output(saved_results$scripts[["stats_not_normal_paired"]]$result), collapse = "\n")
        } else if (!is.null(saved_results$scripts[["stats_normal_unpaired"]])) {
            paste(capture.output(saved_results$scripts[["stats_normal_unpaired"]]$result), collapse = "\n")
            } else if (!is.null(saved_results$scripts[["stats_normal_paired"]])) {
            paste(capture.output(saved_results$scripts[["stats_normal_paired"]]$result), collapse = "\n")
            } else if (!is.null(saved_results$scripts[["stats_normal_paired_effect_size"]])) {
            paste(capture.output(saved_results$scripts[["stats_normal_paired_effect_size"]]$result), collapse = "\n")
            } else if (!is.null(saved_results$scripts[["stats_normal_unpaired_effect_size"]])) {
            paste(capture.output(saved_results$scripts[["stats_normal_unpaired_effect_size"]]$result), collapse = "\n")
            } else if (!is.null(saved_results$scripts[["stats_not_normal_paired_effect_size"]])) {
            paste(capture.output(saved_results$scripts[["stats_not_normal_paired_effect_size"]]$result), collapse = "\n")
            } else if (!is.null(saved_results$scripts[["stats_not_normal_unpaired_effect_size"]])) {
            paste(capture.output(saved_results$scripts[["stats_not_normal_unpaired_effect_size"]]$result), collapse = "\n")
            } else {
            "No statistical scripts found."
            }
})


t_test_effect_size_paired_result
t_test_effect_size_unpaired_result
wilcoxon_effect_size_paired_result
wilcoxon_effect_size_unpaired_result


normal_unpaired_result
normal_paired_result
not_normal_paired_result
not_normal_unpaired_result


    textInput(session$ns("interpretation_quiz_text_p_value"), "Interpret the p-value result in one sentence", value = "A p-value of [statisical test method + degrees of freedom], p=[p-value] suggests that ______.", width = "100%"),
            actionButton(session$ns("interpretation_quiz_p_value_submit"), "Submit", class = "fun-submit-button"),
            textInput(session$ns("interpretation_quiz_text_effect_size"), "Summarise these results in one sentence", value = "An effect size of [effect size method]=[effect size] suggests that ______.", width = "100%"),
            actionButton(session$ns("interpretation_quiz_effect_size_submit"), "Submit", class = "fun-submit-button")