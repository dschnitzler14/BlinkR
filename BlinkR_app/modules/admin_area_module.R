admin_area_module_ui <- function(id) {
  ns <- NS(id)
  admin_area_tab <- tabItem(tabName = "admin_area",
                            fluidPage(
                              fluidRow(
                                column(
                                  12,
                                  box(title = "Google Drive",
                                      collapsible = FALSE,
                                      collapsed = FALSE,
                                      width = 12,
                                      solidHeader = TRUE,
                                      actionButton(ns("open_google_drive"),
                                                   label = tagList(icon("google-drive"), "Open Google Drive"),
                                                   class = "action-button custom-action"
                                      )
                                      ),
                                  box(title = "View Groups",
                                      collapsible = TRUE,
                                      collapsed = FALSE,
                                      width = 12,
                                      solidHeader = TRUE,
                                      view_groups_admin_module_ui(ns("view_groups"))
                                  ),
                                  box(title = "Class Protocol",
                                      collapsible = TRUE,
                                      collapsed = FALSE,
                                      width = 12,
                                      solidHeader = TRUE,
                                      share_to_groups_admin_module_ui(ns("share_protocol")),
                                  ),
                                  box(title = "Class Data",
                                      collapsible = TRUE,
                                      collapsed = FALSE,
                                      width = 12,
                                      solidHeader = TRUE,
                                      share_to_groups_admin_module_ui(ns("share_data")),
                                      fileInput(
                                        ns("class_data_file_upload"),
                                        label = "Choose a local file to upload",
                                        accept = c(".csv", ".xlsx")
                                      ),
                                      textOutput(ns("upload_status")),
                                      combine_sheets_module_ui(ns("combine_data")),
                                  ),
                                  box(title = "Report Submission",
                                      collapsible = TRUE,
                                      collapsed = FALSE,
                                      width = 12,
                                      solidHeader = TRUE,
                                      view_report_submission_admin_module_ui(ns("report_submission_viewer"))
                                  ),
                                      )
                                  ),
  )
  )
}


admin_area_module_server <- function(id, group_data_file_id, parent.session, user_base, user_base_google_sheet, final_reports_folder_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$open_google_drive, {
      runjs('window.open("https://drive.google.com/drive/u/0/folders/1uymxsry9EekJKAlCDVBtrZXUPou7Lu_H", "_blank");')
      
    })
    
    
    column_data_permissions <- paste("F")
    column_protocol_permissions <- paste("E")
    
    share_to_groups_admin_module_server("share_protocol", user_base, user_base_google_sheet, column = column_protocol_permissions)
    share_to_groups_admin_module_server("share_data", user_base, user_base_google_sheet, column = column_data_permissions)
    view_groups_admin_module_server("view_groups", user_base_google_sheet)
    combine_sheets_module_server("combine_data", group_data_file_id, parent.session)
    view_report_submission_admin_module_server("report_submission_viewer", final_reports_folder_id)
    
  })
}
