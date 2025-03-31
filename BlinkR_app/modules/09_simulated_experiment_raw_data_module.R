simulated_experiment_raw_data_module_ui <- function(id, i18n) {
    ns <- NS(id)
    simulated_experiment_raw_data <- tabItem(tabName = "Simulated_Experiment_Raw_Data",
                              fluidPage(
                                      fluidRow(
                                          column(
                                            width = 12,
                                            div(
                                              class = "page-title-box",
                                              tags$h2(
                                                tagList(shiny::icon("database"), "Simulated Experiment: Raw Data")
                                              )
                                    ),
                                    div(
                                        class = "yellow-box",
                                          tagList("Remember, this is a simulated experiment. The data you see here is not real.")
                                      ),
                                  )),
                                fluidRow(
                                  box(
                                    title = tagList(shiny::icon("database"), "Raw Data"),
                                    id = "simulated_raw_data",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    DT::dataTableOutput(ns("hr_data")),
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
        ns("back_page_measurements"),
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_analysis"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
                              )
    )
    }

 

simulated_experiment_raw_data_module_server <- function(id, caf_data_read, parent.session) {
    moduleServer(
    id,
    function(input, output, session){


      output$hr_data <- renderDT({
        DT::datatable(caf_data_read, options = list(pageLength = 10))
      })

      observeEvent(input$back_page_measurements, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Measurements")
      })
      observeEvent(input$next_page_analysis, {
        updateTabItems(parent.session, "sidebar", "Simulated_Experiment_Analysis")
      })

    })
}