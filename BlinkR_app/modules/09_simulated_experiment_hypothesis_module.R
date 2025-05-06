simulated_experiment_hypothesis_module_ui <- function(id, i18n) {
    ns <- NS(id) 
    simulated_experiment_hypothesis <- tabItem(tabName = "Simulated_Experiment_Hypothesis",
                              fluidPage(
                                fluidRow(
                                        column(
                                        width = 12,
                                        div(
                                            class = "page-title-box",
                                            tags$h2(
                                            tagList(shiny::icon("pen-to-square"), i18n$t("Simulated Experiment: Hypothesis"))
                                            )
                                ),
                                div(
                                        class = "yellow-box",
                                          tagList(i18n$t("Remember, this is a simulated experiment. The data you see here is not real."))
                                      ),
                                )

                                    ),
                                fluidRow(
                                  box(
                                        title = tagList(shiny::icon("pen-to-square"), i18n$t("Hypothesis")),
                                        id = "simulated_hypothesis",
                                        collapsible = TRUE,
                                        width = 12,
                                        solidHeader = TRUE,
                                        
                                        div(style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px; background-color: #f9f9f9;",
                                            h3(i18n$t("What is the hypothesis in plain language?")),
                                            p(i18n$t("Caffeine-containing gums affect heart rate"))
                                        ),
                                        
                                        div(style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px; background-color: #f9f9f9;",
                                            h3(i18n$t("What is the null hypothesis?")),
                                            p(i18n$t("Chewing caffeine-containing gum does not affect heart rate significantly more than chewing caffeine-free gum"))
                                        ),
                                        
                                        div(style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px; background-color: #f9f9f9;",
                                            h3(i18n$t("What is the alternative hypothesis?")),
                                            p(i18n$t("Chewing caffeine-containing gum has an effect on heart rate significantly more than chewing caffeine-free gum"))
                                        )
                                      )

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
        ns("back_page_background"),
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_protocol"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
                              )
    )
    }

simulated_experiment_hypothesis_module_server <- function(id, i18n, parent.session) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$back_page_background, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Background")
      })
      observeEvent(input$next_page_protocol, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Protocol")
      })
}
  )
}