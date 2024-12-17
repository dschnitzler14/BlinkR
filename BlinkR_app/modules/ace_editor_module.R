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
      autoComplete = "live",
      autoCompleters = c("rlang", "text", "snippet", "keyword"),
      showPrintMargin = FALSE,
      setBehavioursEnabled = TRUE,
      autoScrollEditorIntoView = TRUE
    ),
    actionButton(
      ns("run_code"),
      label = tagList("\U1F3C3", "Run Code"),
      class = "custom-run-button"           
    ),
    div(
      style = "margin-top: 20px;",
      uiOutput(ns("dynamic_console"))
    )
  )
}

editor_module_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    values <- reactiveValues(result = NULL, is_plot = FALSE)
    
    observeEvent(input$run_code, {
      code <- input$editor
      temp_env <- new.env()
      
      assign("data", data(), envir = temp_env)
      
      eval_result <- tryCatch({
        eval(parse(text = code), envir = temp_env)
      }, error = function(e) {
        paste("Error:", e$message)
      })
      
      values$result <- eval_result
      values$is_plot <- inherits(eval_result, "ggplot") || inherits(eval_result, "recordedplot")
    }, ignoreInit = TRUE)
    
    output$dynamic_console <- renderUI({
      if (values$is_plot) {
        plotOutput(session$ns("plot_output"))
      } else {
        verbatimTextOutput(session$ns("text_output"))
      }
    })
    
    output$plot_output <- renderPlot({
      req(values$is_plot, values$result)
      if (inherits(values$result, "ggplot")) {
        print(values$result)
      } else {
        replayPlot(values$result)
      }
    })
    
    output$text_output <- renderPrint({
      req(!values$is_plot, values$result)
      values$result
    })
    
    return(reactive({
      req(values$result)
      values$result
    }))
  })
}


