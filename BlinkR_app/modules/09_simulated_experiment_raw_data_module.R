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

 

simulated_experiment_raw_data_module_server <- function(id) {
    moduleServer(
    id,
    function(input, output, session){

      file_path <- "data/Caf_Dummy_Data.csv"

      data <- read.csv(file_path, header = TRUE)

      output$hr_data <- renderDT({
        DT::datatable(data, options = list(pageLength = 10))
      })
    })
}