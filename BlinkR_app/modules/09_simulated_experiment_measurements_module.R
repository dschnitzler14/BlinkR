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
                                    ),
                                    div(
                                        class = "yellow-box",
                                          tagList("Remember, this is a simulated experiment. The data you see here is not real.")
                                      ),
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
                                      withSpinner(uiOutput(ns("timeprogress")), type = 7, color = "#78d94c", size = 1),
                                        )
                                      ),
                                      fluidRow(
                                        column(4, simulated_experiment_measure_button_module_ui(ns("measurement1"))),
                                        column(4, simulated_experiment_measure_button_module_ui(ns("measurement2"))),
                                        column(4, simulated_experiment_measure_button_module_ui(ns("measurement3")))
                                      ),
                                    ),
                                    tabPanel(
                                      tabName = "Measurements_Day_2",
                                      title =  tagList(shiny::icon("stethoscope"), "Measurements Day 2"),
                                      fluidRow(
                                        column(12,
                                      actionButton(ns("give_gum2"), tagList(shiny::icon("circle-plus"), "Particpant Start Chewing"), class = "fun-generate-button"),
                                      withSpinner(uiOutput(ns("timeprogress2")), type = 7, color = "#78d94c", size = 1),
                                        )
                                      ),
                                      fluidRow(
                                        column(4, simulated_experiment_measure_button_module_ui(ns("measurement4"))),
                                        column(4, simulated_experiment_measure_button_module_ui(ns("measurement5"))),
                                        column(4, simulated_experiment_measure_button_module_ui(ns("measurement6")))
                                      ),
                                    )
                                  )
                              ),
                              fluidRow(
  column(
    width = 12,
    div(
      style = "
        display: flex; 
        justify-content: center; 
        align-items: center; 
        gap: 10px;          
        margin: 0; 
        padding: 10px;
      ",
      actionButton(
        ns("back_page_protocol"),
        label = tagList(icon("arrow-left"), " Back"),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_raw_data"), 
        label = tagList("Next ", icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
    )
    )
    }

simulated_experiment_measurements_module_server <- function(id, parent.session) {
  moduleServer(
    id,
    function(input, output, session){
    
    output$timeprogress <- renderUI({ 
    NULL
  })

    observeEvent(input$give_gum, {
       output$timeprogress <- renderUI({
        Sys.sleep(5)
        tagList(
          h4("Done!")
        )
      })
      
  
  simulated_experiment_measure_button_module_server("measurement1", max = 80)
  simulated_experiment_measure_button_module_server("measurement2", max = 80)
  simulated_experiment_measure_button_module_server("measurement3", max = 80)

 
  })

   output$timeprogress2 <- renderUI({ 
    NULL
  })

  observeEvent(input$give_gum2, {
  output$timeprogress2 <- renderUI({
    Sys.sleep(5)
        tagList(
          h4("Done!")
        )
    })
  
  simulated_experiment_measure_button_module_server("measurement4", min = 98)
  simulated_experiment_measure_button_module_server("measurement5", min = 98)
  simulated_experiment_measure_button_module_server("measurement6", min = 98)

 
  })

  observeEvent(input$back_page_protocol, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Protocol")
      })
      observeEvent(input$next_page_raw_data, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Raw_Data")
      })
})
}
