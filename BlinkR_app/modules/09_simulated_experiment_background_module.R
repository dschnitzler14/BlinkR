simulated_experiment_background_module_ui <- function(id, i18n) { 
    ns <- NS(id)
    simulated_experiment_background <- tabItem(tabName = "Simulated_Experiment_Background",
                              fluidPage(
                                fluidRow(
                                        column(
                                          width = 12,
                                          div(
                                            class = "page-title-box",
                                            tags$h2(
                                              tagList(shiny::icon("book-open"), i18n$t("Simulated Experiment: Background"))
                                            )
                                  )
                                )

                                      ),
                                fluidRow(
                                  box(
                                    title = tagList(shiny::icon("circle-question"), i18n$t("What do we know?")),
                                    id = "simulated_background1",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("simulated_experiment_background1_markdown"))                                    
                                  ),
                                  box(
                                    title = tagList(shiny::icon("circle-question"), i18n$t("What don't we know?")),
                                    id = "simulated_background2",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("simulated_experiment_background2_markdown"))                                    
                                  ),
                                  box(
                                    title = tagList(shiny::icon("circle-question"), i18n$t("Why is this important?")),
                                    id = "simulated_background3",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("simulated_experiment_background3_markdown"))                                    
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
        ns("back_page_description"),
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_hypothesis"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
                              )
    )
    }

simulated_experiment_background_module_server <- function(id, i18n, parent.session, include_markdown_language) {
  moduleServer(id, function(input, output, session) {

output$simulated_experiment_background1_markdown <- renderUI({
  include_markdown_language("09_simulated_experiment/simulated_experiment_background1.Rmd")
})

output$simulated_experiment_background2_markdown <- renderUI({
  include_markdown_language("09_simulated_experiment/simulated_experiment_background2.Rmd")
})

output$simulated_experiment_background3_markdown <- renderUI({
  include_markdown_language("09_simulated_experiment/simulated_experiment_background3.Rmd")
})


    observeEvent(input$back_page_description, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Description")
      })
      observeEvent(input$next_page_hypothesis, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Hypothesis")
      })
}
  )
}