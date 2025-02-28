simulated_experiment_module_ui <- function(id) {
    ns <- NS(id)
    simulated_experiment <- tabItem(tabName = "Simulated_Experiment",
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
                                  box(
                                    title = "Background",
                                    id = "simulated_background",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6,
                                    includeMarkdown("markdown/09_simulated_experiment/simulated_experiment_background.Rmd")
                                    ),
                                    column(6,
                                    includeMarkdown("markdown/09_simulated_experiment/simulated_experiment_written_introduction.Rmd")
                                    )
                                  ),
                                  box(
                                    title = "Planning & Carrying Out the Experiment",
                                    id = "simulated_planning",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6,
                                    includeMarkdown("markdown/09_simulated_experiment/simulated_experiment_planning.Rmd")
                                    )
                                  ),
                                  box(
                                    title = "Analysing the Data",
                                    id = "simulated_analysis",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6,
                                    includeMarkdown("markdown/09_simulated_experiment/simulated_experiment_analysis.Rmd")
                                    ),
                                    column(6,
                                    includeMarkdown("markdown/09_simulated_experiment/simulated_experiment_written_results.Rmd")
                                    )
                                  ),
                                )
                              )
    )
    }

simulated_experiment_module_server <- function(input, output, session) {
    #ns <- session$ns
}