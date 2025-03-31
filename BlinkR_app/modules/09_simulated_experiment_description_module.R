simulated_experiment_description_module_ui <- function(id, i18n) {
    ns <- NS(id)
    simulated_experiment_description <- tabItem(tabName = "Simulated_Experiment_Description",
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
        ns("next_page_background"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
                              )
    )
    }

simulated_experiment_description_module_server <- function(id,parent.session) {
  moduleServer(id, function(input, output, session) {

       observeEvent(input$next_page_background, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Background")
      })
}
  )
}