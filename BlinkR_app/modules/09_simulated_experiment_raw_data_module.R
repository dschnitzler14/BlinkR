simulated_experiment_raw_data_module_ui <- function(id) {
    ns <- NS(id)
    simulated_experiment_raw_data <- tabItem(tabName = "Simulated_Experiment_Raw_Data",
                              fluidPage(
                                fluidRow(
                                  box(
                                    title = "Raw Data",
                                    id = "simulated_raw_data",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    
                                  ),
                                  )
                              )
    )
    }

simulated_experiment_raw_data_module_server <- function(input, output, session) {
    #ns <- session$ns
}