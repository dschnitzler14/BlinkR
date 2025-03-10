editor_module_ui <- function(id) { 
  ns <- NS(id)
  tagList(
    aceEditor(
      ns("editor"),
      mode = "r",
      theme = "solarized_dark",
      debounce = 5,
      value = "#type your code in here!",
      height = "200px",
      fontSize = 14,
      showLineNumbers = TRUE,
      autoComplete = "disabled",
      #autoCompleters = c("rlang", "text", "snippet", "keyword"),
      showPrintMargin = FALSE,
      setBehavioursEnabled = TRUE,
      autoScrollEditorIntoView = TRUE
    ),
    actionButton(
      ns("send_code_to_editor"),
      label = tagList(icon("arrow-right"), "Code to Editor"),
      class = "custom-button custom-send-button"           
    ),
    actionButton(
      ns("run_code"),
      label = tagList(icon("code"), "Run Code"),
      class = "custom-button custom-run-button"           
    ),
    actionButton(
      ns("clear_console"),
      label = tagList(icon("trash"), "Clear Code"),
      class = "custom-button custom-clear-button"
    ),
    div(
      style = "margin-top: 20px;",
      withSpinner(uiOutput(ns("dynamic_console")), type = 8, color = "#28a745", size = 2)
      #uiOutput(ns("dynamic_console"))

    )
  )
}

editor_module_server <- function(id, data, variable_name = "ace_editor_data", predefined_code = "", return_type = "", session_folder_id, save_header = "Code Header") {
  moduleServer(id, function(input, output, session) {
    values <- reactiveValues(result = NULL, is_plot = FALSE)
    current_code <- reactiveVal(NULL)

    observeEvent(input$send_code_to_editor, {
      updateAceEditor(session, "editor", value = predefined_code)
      
    })
    
    observeEvent(input$run_code, {

      code <- input$editor
      temp_env <- new.env(parent = globalenv())

      if (is.list(data) && length(variable_name) == length(data)) {
        for (i in seq_along(data)) {
          assign(variable_name[i], data[[i]](), envir = temp_env)
        }
      } else {
        assign(variable_name, data(), envir = temp_env)
      }

      forbidden_packages <- c("googledrive", "googlesheets4")
      lapply(forbidden_packages, function(pkg) {
        if (pkg %in% loadedNamespaces()) {
          detach(paste0("package:", pkg), character.only = TRUE, unload = FALSE)
        }
      })

      eval_result <- tryCatch({
        eval(parse(text = code), envir = temp_env)
      }, error = function(e) {
        paste("Error:", e$message)
      })

      lapply(forbidden_packages, function(pkg) {
        if (pkg %in% installed.packages()) {
          library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
        }
      })

      values$result <- eval_result
      values$is_plot <- inherits(eval_result, "ggplot") || inherits(eval_result, "recordedplot")

      current_code(code)

    }, ignoreInit = TRUE)

output$dynamic_console <- renderUI({
    req(input$run_code)

     if (is.null(values$result)) {
    return(NULL)
  }
    if (values$is_plot) {
      plotOutput(session$ns("plot_output"))
    } else if (!is.null(values$result)) {
      verbatimTextOutput(session$ns("text_output"))
    } else {
      NULL
    }
  })


    output$plot_output <- renderPlot({
  req(values$is_plot, values$result)
  if (inherits(values$result, "ggplot")) {
    print(values$result)
  } else if (inherits(values$result, "recordedplot")) {
    replayPlot(values$result)
  } else {
    stop("Result is not a recognized plot type.")
  }
})


    output$text_output <- renderPrint({
      req(!values$is_plot, values$result)
      values$result
    })

    reactive_result <- reactive({
      if (return_type == "result") {
        list(
      result = values$result,
      is_plot = values$is_plot
    )
      } else if (return_type == "code_history") {
        current_code()
      } else {
        list(
          result = values$result,
          code_history = current_code()
        )
      }
    })
    
    observeEvent(current_code(), {
      req(session_folder_id)
      req(current_code())
      
      code_to_save <- current_code()
      current_code(NULL)
            
      future({
      file_name <- "code_history.txt"
      header <- paste0("#", save_header)
      new_content <- c(header, code_to_save, "")
      
      existing_files <- drive_ls(as_id(session_folder_id), pattern = file_name)
      
      if (nrow(existing_files) > 0) {
        drive_download(existing_files$id[1], path = file_name, overwrite = TRUE)
        current_content <- readLines(file_name, warn = FALSE)
        combined_content <- c(current_content, new_content)
        writeLines(combined_content, file_name)
        drive_update(existing_files$id[1], file_name)
      } else {
        writeLines(new_content, file_name)
        drive_upload(file_name, path = as_id(session_folder_id))
      }
      
      TRUE
      }) %...>% {
      showNotification("Saved to Code History", type = "message", duration = 2)
      } %...!% {
        err <- .
        showNotification(
          paste("Error uploading code to Google Drive:", err$message),
          type = "error", duration = 3
        )
      }
      #current_code(NULL)
    }, ignoreInit = TRUE)
    
    observeEvent(input$clear_console, {
      values$result <- NULL
      values$is_plot <- FALSE

      #output$dynamic_console <- renderUI({ NULL })

      updateAceEditor(session, "editor", value = "")
    })
    
    return(reactive_result)
    
  })



}



