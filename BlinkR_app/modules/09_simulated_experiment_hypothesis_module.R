simulated_experiment_hypothesis_module_ui <- function(id) {
    ns <- NS(id)
    simulated_experiment_hypothesis <- tabItem(tabName = "Simulated_Experiment_Hypothesis",
                              fluidPage(
                                fluidRow(
                                  box(
                                    title = "Hypothesis",
                                    id = "simulated_hypothesis",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    
                                  ),
                                  )
                              )
    )
    }

simulated_experiment_hypothesis_module_server <- function(input, output, session) {
    #ns <- session$ns
}