analysis_dashboard_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Analysis_Dashboard",
    fluidPage(
      fluidRow(
        column(
          12,
          box(
            title = "Your Analysis",
            collapsible = FALSE,
            width = 12,
            class = "custom-box",
            actionButton(ns("start"), "Let's Get Started!", class = "action-button custom-action"),
          ),
        
          box(
            title = "Your Results",
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            class = "custom-box",
            actionButton(ns("download_plots"), "Download Your Plots"),
            actionButton(ns("download_results"), "Download Your Analysis Results"),
            actionButton(ns("download_script"), "Download R Script")
          ),
          
        )
      )
    )
  )
}


analysis_dashboard_module_server <- function(id, parent.session) {
  moduleServer(id, function(input, output, session) {
 
    observeEvent(input$start, {
      updateTabItems(parent.session, "sidebar", "Prepare_Data")
    })   
  })
}