simulated_experiment_analysis_module_ui <- function(id) {
    ns <- NS(id)
    simulated_experiment_analysis <- tabItem(tabName = "Simulated_Experiment_Analysis",
                              fluidPage(
                                fluidRow(
                                        column(
                                          width = 12,
                                          div(
                                            class = "page-title-box",
                                            tags$h2(
                                              tagList(shiny::icon("dashboard"), "Simulated Experiment: Analysis")
                                            )
                                  )
                                )),
                                fluidRow(
                                  box(
                                    title = tagList(shiny::icon("dashboard"), "Analysing the Data"),
                                    id = "simulated_analysis1",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    
                                  ),
                                  box(
                                    title = tagList(shiny::icon("magnifying-glass"), "Prepare the Data"),
                                    id = "simulated_analysis2",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    
                                  ),
                                  box(
                                    title = tagList(shiny::icon("rectangle-list"),"Summarising the Data"),
                                    id = "simulated_analysis3",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    
                                  ),
                                  box(
                                    title = tagList(shiny::icon("chart-simple"),"Creating a Figure"),
                                    id = "simulated_analysis4",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    
                                  ),
                                  box(
                                    title = tagList(shiny::icon("equals"), "Statistical Analysis Data"),
                                    id = "simulated_analysis5",
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