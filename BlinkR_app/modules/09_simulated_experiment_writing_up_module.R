simulated_experiment_writing_up_module_ui <- function(id) {
    ns <- NS(id)
    simulated_experiment_writing_up <- tabItem(tabName = "Simulated_Experiment_Writing_Up",
                              fluidPage(
                                fluidRow(
                                  box(
                                    title = "Writing Up",
                                    id = "simulated_writing_up",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    
                                  ),
                                  )
                              )
    )
    }

simulated_experiment_writing_up_module_server <- function(input, output, session) {
    #ns <- session$ns
}