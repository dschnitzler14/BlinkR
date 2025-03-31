simulated_experiment_measure_button_module_ui <- function(id, i18n) {
    ns <- NS(id)
    uiOutput(ns("measurement_button_ui"))
}

simulated_experiment_measure_button_module_server <- function(id, min = 75, max = 120) {
  moduleServer(
    id,
    function(input, output, session){

output$measurement_button_ui <- renderUI({
  tagList(
    div(style = "display: flex; align-items: center;",
        div(style = "border: 1px solid #ccc; padding: 6px 10px; margin-bottom: 20px; border-radius: 4px; width: 100px; background-color: #f8f9fa; text-align: center; font-size: 14pt;  min-height: 1em;",
            textOutput(session$ns("measure_display"))
        )
    ),
    actionButton(session$ns("measure_button"), tagList(shiny::icon("stethoscope"), "Take HR Measurement"), class = "fun-submit-button")
  )
})

observeEvent(input$measure_button, {
  random_value <- sample(min:max, 1)
  output$measure_display <- renderText({ paste(random_value, "bpm") })
})

})}

