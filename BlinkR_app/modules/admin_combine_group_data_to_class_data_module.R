combine_sheets_module_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    actionButton(ns("combine_sheets"),
                 label = tagList(icon("file-circle-plus"), "Combine Group Data"),
                 class = "fun-generate-button"
    ),
    withSpinner(uiOutput(ns("loader_ui")), type = 2, color = "orange", color.background = "white")
  )
}


combine_sheets_module_server <- function(id, group_data_file_id, parent.session) {
  moduleServer(id, function(input, output, session) {
          vars <- get_experiment_vars()

    ns <- session$ns

    output$loader_ui <- renderUI({
      NULL
    })

    observeEvent(input$combine_sheets, {
      req(input$combine_sheets)

        output$loader_ui <- renderUI({
        tags$div("Processing... Please wait.", style = "color: blue; font-weight: bold;")
      })


    future({
      existing_files <- googledrive::drive_find(pattern = "^BlinkR_Combined_Class_Data$")

      if (nrow(existing_files) > 0) {
        timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
        new_name <- paste0("BlinkR_Combined_Class_Data_old_version_", timestamp)

        googledrive::drive_rename(existing_files[1, ], new_name)
      }

      sheet_names <- googlesheets4::sheet_names(group_data_file_id)
      combined_data <- NULL

      for (i in 2:length(sheet_names)) {
        sheet_data <- googlesheets4::read_sheet(group_data_file_id, sheet = sheet_names[i])
        if (i == 2) {
          combined_data <- sheet_data
        } else {
          combined_data <- dplyr::bind_rows(combined_data, sheet_data[-1, ])
        }
      }

      googlesheets4::gs4_create("BlinkR_Combined_Class_Data", sheets = list(combined_class_data = combined_data))

    }) %...>% {
      showNotification("Data successfully combined and written to BlinkR_Combined_Class_Data", type = "message", duration = 3)
    } %...!% {
      showNotification("An error occurred while combining data.", type = "error", duration = 3)
    } %>% finally({
      output$loader_ui <- renderUI({ NULL })
    })

    })
  })
}
