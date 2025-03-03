custom_login_server <- function(id, user_base_sheet_id, user_data_reactive, base_group_files_url) {
  moduleServer(id, function(input, output, session) {

    credentials <- reactiveValues(
      user_auth = FALSE,
      info = list(Group = NULL, role = NULL, date = NULL, protocol = NULL, data = NULL),
      session_folder = NULL,
      session_folder_url = NULL,
      session_folder_id = NULL
    )

    # "Sign up" observer
    observeEvent(input$sign_up_button, {
      req(input$sign_up_group_name, input$name)

      shinyjs::disable("sign_up_button")

      # Get current user data from the reactiveVal
      current_users <- user_data_reactive()

      # Check if group already exists
      user <- current_users %>%
        filter(Group == input$sign_up_group_name) %>%
        slice(1)

      # If group not found, proceed with "sign-up"
      if (nrow(user) < 1) {
        credentials$user_auth <- TRUE
        credentials$info <- list(
          Group = input$sign_up_group_name,
          role = "group",
          date = format(Sys.Date(), "%d/%m/%y"),
          protocol = "FALSE",
          data = "FALSE"
        )
        output$error <- renderText("")

        # Data frame to append
        new_user <- data.frame(
          Group = as.character(input$sign_up_group_name),
          Role = as.character("group"),
          Name = as.character(input$name),
          Date = credentials$info$date,
          Protocol = "FALSE",
          Data = "FALSE",
          stringsAsFactors = FALSE
        )

        # Append new row to the Google Sheet
        tryCatch({
          googlesheets4::sheet_append(user_base_sheet_id, new_user)

          # ***IMPORTANT***
          # Now re-read the sheet so our reactiveVal has the updated rows
          updated_df <- googlesheets4::read_sheet(user_base_sheet_id)
          user_data_reactive(updated_df)

          output$sign_up_status <- renderUI("Group successfully signed up!")
        },
        error = function(e) {
          output$sign_up_status <- renderUI(paste("Error: ", e$message))
        })

        # Create folder, share, etc...
        if (credentials$info$role != "admin") {
          parent_folder_name <- "BlinkR_text_results"
          parent_folder <- googledrive::drive_get(parent_folder_name)

          if (nrow(parent_folder) == 0) {
            parent_folder <- googledrive::drive_mkdir(parent_folder_name)
          }

          group_name <- input$sign_up_group_name
          session_folder_name <- group_name

          new_folder <- googledrive::drive_mkdir(
            name = session_folder_name,
            path = googledrive::as_id(parent_folder$id)
          )

          folder_id <- new_folder$id

          drive_share_anyone(googledrive::as_id(folder_id))

          credentials$session_folder <- new_folder

          folder_url <- paste0(base_group_files_url, folder_id)

          credentials$session_folder_id <- folder_id
          credentials$session_folder_url <- folder_url
        } else {
          credentials$session_folder <- NULL
          credentials$session_folder_url <- NULL
          credentials$session_folder_id <- NULL
        }

      } else {
        output$sign_uperror <- renderUI("Group already exists.")
      }
    })

    # Return your usual reactive
    reactive({
      list(
        user_auth = credentials$user_auth,
        user_info = credentials$info,
        session_folder = credentials$session_folder,
        session_folder_url = credentials$session_folder_url,
        session_folder_id = credentials$session_folder_id
      )
    })

  })
}
