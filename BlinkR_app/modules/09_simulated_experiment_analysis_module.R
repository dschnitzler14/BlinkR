simulated_experiment_analysis_module_ui <- function(id) {
    ns <- NS(id)
    simulated_experiment_analysis <- tabItem(tabName = "Simulated_Experiment_Analysis",
                              fluidPage(
                                fluidRow(
                                  box(
                                    title = "Analysing the Data",
                                    id = "simulated_analysis",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    
                                  ),
                                  )
                              )
    )
    }

simulated_experiment_analysis_module_server <- function(input, output, session) {
    #ns <- session$ns
}