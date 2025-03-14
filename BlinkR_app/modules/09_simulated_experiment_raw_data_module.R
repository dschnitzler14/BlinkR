simulated_experiment_raw_data_module_ui <- function(id) {
    ns <- NS(id)
    simulated_experiment_raw_data <- tabItem(tabName = "Simulated_Experiment_Raw_Data",
                              fluidPage(
                                      fluidRow(
                                          column(
                                            width = 12,
                                            div(
                                              class = "page-title-box",
                                              tags$h2(
                                                tagList(shiny::icon("database"), "Simulated Experiment: Raw Data")
                                              )
                                    )
                                  )),
                                fluidRow(
                                  box(
                                    title = tagList(shiny::icon("database"), "Raw Data"),
                                    id = "simulated_raw_data",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    DT::dataTableOutput(ns("hr_data")),
                                  ),
                                  )
                              )
    )
    }

 

simulated_experiment_raw_data_module_server <- function(id, caf_data_read) {
    moduleServer(
    id,
    function(input, output, session){


      output$hr_data <- renderDT({
        DT::datatable(caf_data_read, options = list(pageLength = 10))
      })
    })
}