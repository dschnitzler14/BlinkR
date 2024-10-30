text_area_module_UI <- function(id, text_label = "Text Area", text_value = "", button_label = "Submit") {
  ns <- NS(id)
  tagList(
    textAreaInput(
      inputId = ns("text_input"),
      label = text_label,
      value = text_value
    ),
    actionButton(ns("inputTextButton"), label = button_label),
    verbatimTextOutput(ns("text"))
  )
}

text_area_module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$inputTextButton, {
        output$text <- renderText({
          "Text submitted"
        })
      })
    }
  )
}