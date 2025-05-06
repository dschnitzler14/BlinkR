simulated_experiment_protocol_module_ui <- function(id, i18n) {
    ns <- NS(id)
    simulated_experiment_protocol <- tabItem(tabName = "Simulated_Experiment_Protocol",
                              fluidPage(
                                    fluidRow(
                                  column(
                                    width = 12,
                                    div(
                                      class = "page-title-box",
                                      tags$h2(
                                        tagList(shiny::icon("list"), i18n$t("Simulated Experiment: Protocol"))
                                      )
                            )
                          )

                                ),
                                fluidRow(
                                  box(
                                    title = tagList(shiny::icon("list"), i18n$t("Protocol")),
                                    id = "simulated_protocol",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("simulated_experiment_protocol_markdown"))
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
        ns("back_page_hypothesis"),
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_measurements"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
                              )
    )
    }

simulated_experiment_protocol_module_server <- function(id, i18n, parent.session, include_markdown_language) {
  moduleServer(id, function(input, output, session) {

output$simulated_experiment_protocol_markdown <- renderUI({
  include_markdown_language("09_simulated_experiment/simulated_experiment_protocol.Rmd")
})

    observeEvent(input$back_page_hypothesis, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Hypothesis")
      })
      observeEvent(input$next_page_measurements, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Measurements")
      })
}
  )
}