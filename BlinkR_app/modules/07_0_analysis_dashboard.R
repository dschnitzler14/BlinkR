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
            actionButton(ns("start"), "Click Here To Get Started!", class = "action-button custom-action"),
          ),
          box(
            title = "What do you want to do next?",
            collapsible = FALSE,
            width = 12,
            class = "custom-box",
            actionButton(ns("summarise"), "Summarise the Data", class = "action-button custom-action"),
            actionButton(ns("statistics"), "Run Statistical Analysis", class = "action-button custom-action"),
            actionButton(ns("figure"), "Create a Figure", class = "action-button custom-action")
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
    
    observeEvent(input$summarise, {
      updateTabItems(parent.session, "sidebar", "Summarise_Data")
    })
    observeEvent(input$statistics, {
      updateTabItems(parent.session, "sidebar", "Statistical_Analysis")
    })
    observeEvent(input$figure, {
      updateTabItems(parent.session, "sidebar", "Create_Figure")
    })
    
  })
}