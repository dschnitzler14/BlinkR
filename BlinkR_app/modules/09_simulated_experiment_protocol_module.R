simulated_experiment_protocol_module_ui <- function(id) {
    ns <- NS(id)
    simulated_experiment_protocol <- tabItem(tabName = "Simulated_Experiment_Protocol",
                              fluidPage(
                                    fluidRow(
                                  column(
                                    width = 12,
                                    div(
                                      class = "page-title-box",
                                      tags$h2(
                                        tagList(shiny::icon("list"), "Simulated Experiment: Protocol")
                                      )
                            )
                          )

                                ),
                                fluidRow(
                                  box(
                                    title = tagList(shiny::icon("list"), "Protocol"),
                                    id = "simulated_protocol",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/09_simulated_experiment/simulated_experiment_protocol.Rmd")
                                  ),
                                  )
                              )
    )
    }

simulated_experiment_protocol_module_server <- function(input, output, session) {
    #ns <- session$ns
}