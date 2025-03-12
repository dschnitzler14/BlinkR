simulated_experiment_measurements_module_ui <- function(id) { 
    ns <- NS(id)
    simulated_experiment_measurements <- tabItem(tabName = "Simulated_Experiment_Measurements",
                              fluidPage(
                                fluidRow(
                                          column(
                                            width = 12,
                                            div(
                                              class = "page-title-box",
                                              tags$h2(
                                                tagList(shiny::icon("ruler"), "Measurements")
                                              )
                                    )
                                  )),
                                fluidRow(
                                  box(
                                    title = tagList(shiny::icon("heart-pulse"), "Take Heart Rate Measurements"),
                                    id = "simulated_measurements",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/09_simulated_experiment/simulated_experiment_measurements1.Rmd")
                                  ),
                                  tabBox(
                                    width = 12,
                                    tabPanel(
                                      tabName = "Measurements_Day_1",
                                      title =  tagList(shiny::icon("stethoscope"), "Measurements Day 1"),
                                      fluidRow(
                                        column(12,
                                      actionButton(ns("give_gum"), tagList(shiny::icon("circle-plus"), "Particpant Start Chewing"), class = "fun-generate-button"),
                                      uiOutput(ns("timeprogress")),
                                        )
                                      ),
                                      fluidRow(
                                        column(3, simulated_experiment_measure_button_module_ui(ns("measurement1"))),
                                        column(3, simulated_experiment_measure_button_module_ui(ns("measurement2"))),
                                        column(3, simulated_experiment_measure_button_module_ui(ns("measurement3")))
                                      ),
                                    ),
                                    tabPanel(
                                      tabName = "Measurements_Day_2",
                                      title =  tagList(shiny::icon("stethoscope"), "Measurements Day 2"),
                                      fluidRow(
                                        column(12,
                                      actionButton(ns("give_gum2"), "Particpant Start Chewing", class = "fun-generate-button"),
                                      uiOutput(ns("timeprogress2")),
                                        )
                                      ),
                                      fluidRow(
                                        column(3, simulated_experiment_measure_button_module_ui(ns("measurement4"))),
                                        column(3, simulated_experiment_measure_button_module_ui(ns("measurement5"))),
                                        column(3, simulated_experiment_measure_button_module_ui(ns("measurement6")))
                                      ),
                                    )
                                  )
                              )
    )
    )
    }

simulated_experiment_measurements_module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session){
    
    observeEvent(input$give_gum, {
  output$timeprogress <- renderUI({
    })
  
  simulated_experiment_measure_button_module_server("measurement1", max = 80)
  simulated_experiment_measure_button_module_server("measurement2", max = 80)
  simulated_experiment_measure_button_module_server("measurement3", max = 80)

 
  })

  observeEvent(input$give_gum2, {
  output$timeprogress2 <- renderUI({
    })
  
  simulated_experiment_measure_button_module_server("measurement4", min = 98)
  simulated_experiment_measure_button_module_server("measurement5", min = 98)
  simulated_experiment_measure_button_module_server("measurement6", min = 98)

 
  })
})
}
