simulated_experiment_writing_up_module_ui <- function(id, i18n) {
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
                                    ),
                                    div(
                                        class = "yellow-box",
                                          tagList("Remember, this is a simulated experiment. The data you see here is not real.")
                                      ),
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
                                  ),
                                  fluidRow(
  column(
    width = 12,
    div(
      style = "
        display: flex; 
        justify-content: center; 
        align-items: center; 
        gap: 10px;          
        margin: 0; 
        padding: 10px;
      ",
      actionButton(
        ns("back_page_analysis"),
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_feedback"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
                              )
    )
    }

simulated_experiment_writing_up_module_server <- function(id,parent.session) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$back_page_analysis, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Analysis")
      })
      observeEvent(input$next_page_feedback, {
        updateTabItems(parent.session, "sidebar", "Feedback")
      })
}
  )
}