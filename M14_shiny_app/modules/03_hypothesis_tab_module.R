# Protocol Design Module ----
# protocol_design_module_UI <- function(id, text_label = "Protocol Design", button_label = "Submit") {
#   ns <- NS(id)
#   tagList(
#     textAreaInput(
#       inputId = ns("text_input"),
#       label = text_label
#     ),
#     actionButton(ns("inputTextButton"), label = button_label),
#     verbatimTextOutput(ns("text"))
#   )
# }
# 
# protocol_design_module_server <- function(id) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       observeEvent(input$inputTextButton, {
#         output$text <- renderText({
#           "Protocol submitted"
#         })
#       })
#     }
#   )
# }

# Hypothesis Module ----
hypothesis_module_ui <- function(id) {
  ns <- NS(id)
  hypothesis_tab <- tabItem(
    tabName = "Hypothesis",
    fluidPage(
      fluidRow(
        box(
          title = "What is a hypothesis?",
          id = "hypothesis_box1",
          collapsible = FALSE,
          status = "info",
          width = 12,
          includeMarkdown("markdown/03_hypothesis/what_is_a_hypothesis.Rmd")
        ),
        box(
          title = "What is the background?",
          id = "hypothesis_box2",
          collapsible = FALSE,
          status = "info",
          width = 12,
          includeMarkdown("markdown/03_hypothesis/background_summary.Rmd")
        ),
        box(
          title = "Let's design this experiment",
          id = "hypothesis_box3",
          collapsible = FALSE,
          status = "info",
          width = 12,
          text_area_module_UI(ns("protocol1"), text_label = "Protocol", text_value = "What will we measure? How will we measure it? What are the limitations?")
        )
      )
    )
  )
}

# Hypothesis Module Server
hypothesis_module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      text_area_module_server("protocol1")
    }
  )
}
