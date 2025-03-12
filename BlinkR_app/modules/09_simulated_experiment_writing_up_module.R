simulated_experiment_writing_up_module_ui <- function(id) {
    ns <- NS(id)
    simulated_experiment_writing_up <- tabItem(tabName = "Simulated_Experiment_Writing_Up",
                              fluidPage(
                                fluidRow(
                                  fluidRow(
                                          column(
                                            width = 12,
                                            div(
                                              class = "page-title-box",
                                              tags$h2(
                                                tagList(shiny::icon("pen"), "Simulated Experiment: Writing Up")
                                              )
                                    )
                                  )),
                                  box(
                                    title = "Writing Up",
                                    id = "simulated_writing_up",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    
                                  ),
                                  box(
                                    title = "Introduction",
                                    id = "simulated_writing_up2",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6, 
                                    p("Text")
                                    ),
                                    column(6,
                                    p("Advice")
                                    )
                                  ),
                                  box(
                                    title = "Methods",
                                    id = "simulated_writing_up3",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6, 
                                    p("Text")
                                    ),
                                    column(6,
                                    p("Advice")
                                    )
                                  ),
                                  box(
                                    title = "Results",
                                    id = "simulated_writing_up3",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6, 
                                    p("Text")
                                    ),
                                    column(6,
                                    p("Advice")
                                    )
                                  ),
                                  box(
                                    title = "Discussion",
                                    id = "simulated_writing_up4",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6, 
                                    p("Text")
                                    ),
                                    column(6,
                                    p("Advice")
                                    )
                                  ),
                                  box(
                                    title = "Future Direction",
                                    id = "simulated_writing_up5",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6, 
                                    p("Text")
                                    ),
                                    column(6,
                                    p("Advice")
                                    )
                                  ),
                                  box(
                                    title = "References",
                                    id = "simulated_writing_up6",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6, 
                                    p("Text")
                                    ),
                                    column(6,
                                    p("Advice")
                                    )
                                  ),
                                  box(
                                    title = "AI",
                                    id = "simulated_writing_up7",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    fluidRow(
                                    column(6, 
                                    p("Intro AI")
                                    ),
                                    column(6,
                                    p("Critique")
                                    ),
                                    ),
                                    fluidRow(
                                    column(6, 
                                    p("Methods AI")
                                    ),
                                    column(6,
                                    p("Critique")
                                    ),
                                    ),
                                    fluidRow(
                                    column(6, 
                                    p("Results AI")
                                    ),
                                    column(6,
                                    p("Critique")
                                    ),
                                    ),
                                    fluidRow(
                                    column(6, 
                                    p("Discussion AI")
                                    ),
                                    column(6,
                                    p("Critique")
                                    ),
                                    ),
                                    fluidRow(
                                    column(6, 
                                    p("Future Direction AI")
                                    ),
                                    column(6,
                                    p("Critique")
                                    ),
                                    )
                                  ),

                                  )
                              )
    )
    }

simulated_experiment_writing_up_module_server <- function(input, output, session) {
    #ns <- session$ns
}