simulated_experiment_measurements_module_ui <- function(id, i18n) { 
    ns <- NS(id)
    simulated_experiment_measurements <- tabItem(tabName = "Simulated_Experiment_Measurements",
                              fluidPage(
                                fluidRow(
                                          column(
                                            width = 12,
                                            div(
                                              class = "page-title-box",
                                              tags$h2(
                                                tagList(shiny::icon("ruler"), i18n$t("Simulated Experiment: Measurements"))
                                              )
                                    ),
                                    div(
                                        class = "yellow-box",
                                          tagList(i18n$t("Remember, this is a simulated experiment. The data you see here is not real."))
                                      ),
                                  )),
                                fluidRow(
                                  box(
                                    title = tagList(shiny::icon("heart-pulse"), i18n$t("Take Heart Rate Measurements")),
                                    id = "simulated_measurements",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("simulated_experiment_measurements1_markdown"))
                                  ),
                                  tabBox(
                                    width = 12,
                                    tabPanel(
                                      tabName = "Measurements_Day_1",
                                      title =  tagList(shiny::icon("stethoscope"), i18n$t("Measurements Day 1")),
                                      fluidRow(
                                        column(12,
                                      actionButton(ns("give_gum"), tagList(shiny::icon("circle-plus"), i18n$t("Particpant: Start chewing")), class = "fun-generate-button"),
                                      withSpinner(uiOutput(ns("timeprogress")), type = 7, color = "#78d94c", size = 1),
                                        )
                                      ),
                                      fluidRow(
                                        column(4, simulated_experiment_measure_button_module_ui(ns("measurement1"), i18n)),
                                        column(4, simulated_experiment_measure_button_module_ui(ns("measurement2"), i18n)),
                                        column(4, simulated_experiment_measure_button_module_ui(ns("measurement3"), i18n))
                                      ),
                                    ),
                                    tabPanel(
                                      tabName = "Measurements_Day_2",
                                      title =  tagList(shiny::icon("stethoscope"), i18n$t("Measurements Day 2")),
                                      fluidRow(
                                        column(12,
                                      actionButton(ns("give_gum2"), tagList(shiny::icon("circle-plus"), i18n$t("Particpant: Start chewing")), class = "fun-generate-button"),
                                      withSpinner(uiOutput(ns("timeprogress2")), type = 7, color = "#78d94c", size = 1),
                                        )
                                      ),
                                      fluidRow(
                                        column(4, simulated_experiment_measure_button_module_ui(ns("measurement4"), i18n)),
                                        column(4, simulated_experiment_measure_button_module_ui(ns("measurement5"), i18n)),
                                        column(4, simulated_experiment_measure_button_module_ui(ns("measurement6"), i18n))
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
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_raw_data"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
    )
    )
    }

simulated_experiment_measurements_module_server <- function(id, i18n, parent.session, include_markdown_language) {
  moduleServer(
    id,
    function(input, output, session){
    
    output$simulated_experiment_measurements1_markdown <- renderUI({
  include_markdown_language("09_simulated_experiment/simulated_experiment_measurements1.Rmd")
})

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
      
  
  simulated_experiment_measure_button_module_server("measurement1", i18n, max = 80)
  simulated_experiment_measure_button_module_server("measurement2", i18n, max = 80)
  simulated_experiment_measure_button_module_server("measurement3", i18n, max = 80)

 
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
  
  simulated_experiment_measure_button_module_server("measurement4", i18n, min = 98)
  simulated_experiment_measure_button_module_server("measurement5", i18n, min = 98)
  simulated_experiment_measure_button_module_server("measurement6", i18n, min = 98)

 
  })

  observeEvent(input$back_page_protocol, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Protocol")
      })
      observeEvent(input$next_page_raw_data, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Raw_Data")
      })
})
}
