editor_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    aceEditor(
      ns("editor"),
      mode = "r",
      theme = "solarized_dark",
      value = "#type your code in here!",
      height = "200px",
      fontSize = 14,
      showLineNumbers = TRUE,
      autoComplete = "live",
      autoCompleters = c("rlang", "text", "snippet", "keyword"),
      showPrintMargin = FALSE,
      setBehavioursEnabled = TRUE
    ),
    actionButton(ns("run_code"), "Run Code"),
    div(
      style = "margin-top: 20px;",  # Add spacing here
      verbatimTextOutput(ns("console"))
    )
  )
}


editor_module_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    values <- reactiveValues(result = NULL, executed = FALSE)

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
      values$executed <- TRUE
    })

    output$console <- renderPrint({
      req(values$executed)
        values$result
    })

    return(reactive(values$result))
  })
}
# 
# editor_module_server <- function(id, data) {
#   moduleServer(id, function(input, output, session) {
#     values <- reactiveValues(result = NULL, executed = FALSE, plot = NULL)
#     
#     observeEvent(input$run_code, {
#       code <- input$editor
#       temp_env <- new.env()
#       
#       assign("data", data(), envir = temp_env)
#       
#       eval_result <- tryCatch({
#         eval(parse(text = code), envir = temp_env)
#       }, error = function(e) {
#         paste("Error:", e$message)
#       })
#       
#       # Check if the result is a ggplot or a base plot
#       if (inherits(eval_result, "ggplot") || is.function(eval_result)) {
#         values$plot <- eval_result
#       } else {
#         values$plot <- NULL
#       }
#       
#       values$result <- eval_result
#       values$executed <- TRUE
#     })
#     
#     output$console <- renderPrint({
#       req(values$executed)
#       values$result
#     })
#     
#     # Return the plot object
#     return(reactive(values$plot))
#   })
# }


