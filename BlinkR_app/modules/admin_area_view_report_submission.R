view_report_submission_admin_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("submitted_reports")),
    actionButton(ns("refresh_data"), "Refresh Submitted Reports Table", class = "fun-save-button"),
  )
  
}

view_report_submission_admin_module_server <- function(id, final_reports_folder_id) {
  moduleServer(id, function(input, output, session) {
    vars <- get_experiment_vars()


    ns <- session$ns
    table_data <- reactiveVal(NULL)
    load_data <- function() {
      df <- googledrive::drive_ls(as_id(final_reports_folder_id))
      df$Name <- str_extract(df$name, "(?<=^Final Report - )[^-]+")
      df$Group <- str_extract(df$name, "(?<=Group )\\d+(?=\\.)")
      df
    }
    observeEvent(input$refresh_data, {
      df <- load_data()
      table_data(df)
    }, ignoreNULL = FALSE)
    output$submitted_reports <- DT::renderDT({
      req(table_data())
      DT::datatable(
        table_data()[, c("name", "Name", "Group", "id")],
        options = list(scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
}
