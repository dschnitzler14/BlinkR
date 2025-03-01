simulated_experiment_protocol_module_ui <- function(id) {
    ns <- NS(id)
    simulated_experiment_protocol <- tabItem(tabName = "Simulated_Experiment_Protocol",
                              fluidPage(
                                fluidRow(
                                  box(
                                    title = "Protocol",
                                    id = "simulated_protocol",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    
                                  ),
                                  )
                              )
    )
    }

simulated_experiment_protocol_module_server <- function(input, output, session) {
    #ns <- session$ns
}