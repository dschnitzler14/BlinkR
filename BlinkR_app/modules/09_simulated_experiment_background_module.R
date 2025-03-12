simulated_experiment_background_module_ui <- function(id) { 
    ns <- NS(id)
    simulated_experiment_background <- tabItem(tabName = "Simulated_Experiment_Background",
                              fluidPage(
                                fluidRow(
                                        column(
                                          width = 12,
                                          div(
                                            class = "page-title-box",
                                            tags$h2(
                                              tagList(shiny::icon("book-open"), "Simulated Experiment: Background")
                                            )
                                  )
                                )

                                      ),
                                fluidRow(
                                  box(
                                    title = tagList(shiny::icon("circle-question"),"What do we know?"),
                                    id = "simulated_background1",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/09_simulated_experiment/simulated_experiment_background1.Rmd")
                                    
                                  ),
                                  box(
                                    title = tagList(shiny::icon("circle-question"), "What don't we know?"),
                                    id = "simulated_background2",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/09_simulated_experiment/simulated_experiment_background2.Rmd")
                                    
                                  ),
                                  box(
                                    title = tagList(shiny::icon("circle-question"), "Why is this important?"),
                                    id = "simulated_background3",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/09_simulated_experiment/simulated_experiment_background3.Rmd")
                                    
                                  ),
                                  )
                              )
    )
    }

simulated_experiment_background_module_server <- function(input, output, session) {
    #ns <- session$ns
}