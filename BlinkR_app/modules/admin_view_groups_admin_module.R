view_groups_admin_module_ui <- function(id) {
  ns <- NS(id)
    tagList(
      DT::DTOutput(ns("user_table")),
    )
}


view_groups_admin_module_server <- function(id, user_base_data) {
  moduleServer(id, function(input, output, session) {
          vars <- get_experiment_vars()

    ns <- session$ns

    observe({
    output$user_table <- DT::renderDT({
      req(user_base_data)
      DT::datatable(
        user_base_data,
        editable = FALSE,
        options = list(pageLength = 10)
      )
    })
    })
    
    
    
    }
  )
}