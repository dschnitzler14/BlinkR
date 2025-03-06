view_groups_admin_module_ui <- function(id) {
  ns <- NS(id)
    tagList(
      DT::DTOutput(ns("user_table")),
      actionButton(ns("refresh_data"), "Refresh Groups Data")
    )
}


view_groups_admin_module_server <- function(id, user_base_google_sheet) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    user_base_data <- user_base_google_sheet()
  
    
    observe({
    output$user_table <- DT::renderDT({
      req(user_base_data())
      DT::datatable(
        user_base_data(),
        editable = FALSE,
        options = list(pageLength = 10)
      )
    })
    })
    
    
    
    }
  )
}