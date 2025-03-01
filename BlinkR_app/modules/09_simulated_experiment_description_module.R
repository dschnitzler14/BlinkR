simulated_experiment_description_module_ui <- function(id) {
    ns <- NS(id)
    simulated_experiment_description <- tabItem(tabName = "Simulated_Experiment_Description",
                              fluidPage(
                                fluidRow(
                                    
                                  box(
                                    title = "Description of this Section",
                                    id = "simulated_description",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/09_simulated_experiment/simulated_experiment_description.Rmd")
                                  ),
                                  
                                )
                              )
    )
    }

simulated_experiment_description_module_server <- function(input, output, session) {
    #ns <- session$ns
}