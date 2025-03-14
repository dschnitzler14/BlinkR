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
                                    includeMarkdown("markdown/09_simulated_experiment/som_exp_writing_up_first.Rmd")
                                  ),
                                  box(
                                    title = "Introduction",
                                    id = "simulated_writing_up2",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6, 
                                    includeMarkdown("markdown/09_simulated_experiment/sim_exp_writing_intro_body.Rmd")
                                    ),
                                    column(6,
                                    includeMarkdown("markdown/09_simulated_experiment/sim_exp_writing_intro_advice.Rmd")
                                    )
                                  ),
                                  box(
                                    title = "Methods",
                                    id = "simulated_writing_up3",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6, 
                                    includeMarkdown("markdown/09_simulated_experiment/sim_exp_writing_methods_body.Rmd"),
                                    tags$figure(
                                        class = "centerFigure",
                                        tags$img(
                                          src = "caf_hist.png",
                                          width = "80%",
                                          alt = "Histogram"
                                        ),
                                        tags$figcaption("Fig.1: Histogram of bpm measurements")
                                      )
                                    ),
                                    column(6,
                                    includeMarkdown("markdown/09_simulated_experiment/sim_exp_writing_methods_advice.Rmd")
                                    )
                                  ),
                                  box(
                                    title = "Results",
                                    id = "simulated_writing_up3",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6, 
                                    includeMarkdown("markdown/09_simulated_experiment/sim_exp_writing_results_body.Rmd"),
                                    tags$figure(
                                        class = "centerFigure",
                                        tags$img(
                                          src = "caf_box.png",
                                          width = "80%",
                                          alt = "Boxplot"
                                        ),
                                        tags$figcaption("Fig.2: Boxplot of bpm measurements for caffeine-free and caffeine-containing groups.")
                                      )
                                    ),
                                    column(6,
                                    includeMarkdown("markdown/09_simulated_experiment/sim_exp_writing_results_advice.Rmd")
                                    )
                                  ),
                                  box(
                                    title = "Discussion",
                                    id = "simulated_writing_up4",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6, 
                                    includeMarkdown("markdown/09_simulated_experiment/sim_exp_writing_discussion_body.Rmd")
                                    ),
                                    column(6,
                                    includeMarkdown("markdown/09_simulated_experiment/sim_exp_writing_discussion_advice.Rmd")
                                    )
                                  ),
                                  box(
                                    title = "Future Direction",
                                    id = "simulated_writing_up5",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6, 
                                    includeMarkdown("markdown/09_simulated_experiment/sim_exp_writing_future_body.Rmd")
                                    ),
                                    column(6,
                                    includeMarkdown("markdown/09_simulated_experiment/sim_exp_writing_future_advice.Rmd")
                                    )
                                  ),
                                  box(
                                    title = "References",
                                    id = "simulated_writing_up6",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    column(6, 
                                    includeMarkdown("markdown/09_simulated_experiment/sim_exp_writing_references_body.Rmd")
                                    ),
                                    column(6,
                                    includeMarkdown("markdown/09_simulated_experiment/sim_exp_writing_references_advice.Rmd")
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