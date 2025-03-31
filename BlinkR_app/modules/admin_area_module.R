admin_area_module_ui <- function(id, i18n) {
  ns <- NS(id)
  admin_area_tab <- tabItem(tabName = "admin_area",
                            fluidPage(
                              fluidRow(
                                    column(
                                      width = 12,
                                      div(
                                        style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
                                        actionButton(ns("open_google_drive"),
                                                    label = tagList(icon("google-drive"), "Open Google Drive"),
                                                    class = "btn-primary"
                                        )
                                      )
                                    ),
                                  ),
                              fluidRow(
                                column(
                                  12,
                                  
                                  box(title = tagList(shiny::icon("users-viewfinder"),"View All Groups"),
                                      collapsible = TRUE,
                                      collapsed = FALSE,
                                      width = 12,
                                      solidHeader = TRUE,
                                      view_groups_admin_module_ui(ns("view_groups"), i18n)
                                  ),
                                  box(title = tagList(shiny::icon("users"), "Share Class Protocol"),
                                      collapsible = TRUE,
                                      collapsed = FALSE,
                                      width = 12,
                                      solidHeader = TRUE,
                                      share_to_groups_admin_module_ui(ns("share_protocol"), i18n),
                                  ),
                                  box(title = tagList(shiny::icon("circle-plus"), "Share Class Data"),
                                      collapsible = TRUE,
                                      collapsed = FALSE,
                                      width = 12,
                                      solidHeader = TRUE,
                                      share_to_groups_admin_module_ui(ns("share_data"), i18n),
                                      combine_sheets_module_ui(ns("combine_data"), i18n),
                                  ),
                                  box(title = tagList(shiny::icon("upload"), "View Report Submission"),
                                      collapsible = TRUE,
                                      collapsed = FALSE,
                                      width = 12,
                                      solidHeader = TRUE,
                                      view_report_submission_admin_module_ui(ns("report_submission_viewer"), i18n)
                                  ),
                                      )
                                  ),
  )
  )
}


admin_area_module_server <- function(id, group_data_file_id, parent.session, user_base, final_reports_folder_id, user_base_sheet_id, session_folder_id) {
  moduleServer(id, function(input, output, session) {
          vars <- get_experiment_vars()

    ns <- session$ns

    observeEvent(input$open_google_drive, {
    req(session_folder_id)
        
    showModal(modalDialog(
      title = "View Google Drive",
      your_google_drive_module_ui(session$ns("admin_drive_module"), i18n),

      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l" 
    ))
    
    your_google_drive_module_server("admin_drive_module", session_folder_id)

  })
    
    user_base_data_reactive <- reactiveVal()

    observe({
      user_base_data_reactive(read_sheet(user_base_sheet_id))
    })
    
    user_base_static <- read_sheet(user_base_sheet_id)

    column_data_permissions <- paste("F")
    column_protocol_permissions <- paste("E")
    
    share_to_groups_admin_module_server("share_protocol", user_base_static, column = column_protocol_permissions, user_base_sheet_id)
    share_to_groups_admin_module_server("share_data", user_base_data = user_base_static, column = column_data_permissions, user_base_sheet_id)
    view_groups_admin_module_server("view_groups", user_base_data_reactive())
    combine_sheets_module_server("combine_data", group_data_file_id, parent.session)
    view_report_submission_admin_module_server("report_submission_viewer", final_reports_folder_id)
    
  })
}
