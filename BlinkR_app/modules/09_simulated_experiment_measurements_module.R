simulated_experiment_measurements_module_ui <- function(id) {
    ns <- NS(id)
    simulated_experiment_measurements <- tabItem(tabName = "Simulated_Experiment_Measurements",
                              fluidPage(
                                fluidRow(
                                  box(
                                    title = "Measurements",
                                    id = "simulated_measurements",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    
                                  ),
                                  )
                              )
    )
    }

simulated_experiment_measurements_module_server <- function(input, output, session) {
    #ns <- session$ns
}