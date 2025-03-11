text_area_module_UI <- function(id, text_label = "Type Your Notes Here", text_value = "", button_label = "Save Notes") {
  ns <- NS(id)
  tagList(
    textAreaInput(
      inputId = ns("text_input"),
      label = text_label,
      value = text_value
    ),
    div(
      style = "display: flex; justify-content: center; align-items: center; width: 100%;",
    actionButton(ns("inputTextButton"), label = button_label, class = tagList(icon("save"), "fun-save-button")
    )
),
    uiOutput(ns("submission_feedback"))
  )
}

text_area_module_server <- function(id, auth, filename = "Filename") {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$inputTextButton, {
        
        output$submission_feedback <- renderUI({NULL})

        text_to_save <- input$text_input
        temp_file <- tempfile(fileext = ".txt")
        writeLines(text_to_save, temp_file)

        Group <- auth()$user_info$Group
        pathname <- paste0("BlinkR_text_results/", Group)

        name <- paste0(filename, ".txt")

        upload_and_overwrite <- function(temp_file, pathname, name) {
          
          existing_files <- drive_ls(path = pathname)

          existing_file <- existing_files %>%
            dplyr::filter(name == name)
          
          if (nrow(existing_file) > 0) {
            drive_rm(as_id(existing_file$id))
          }
          
          drive_upload(
            media = temp_file,
            path = pathname,
            name = name
          )
        }

        upload_and_overwrite(temp_file = temp_file, pathname = pathname, name = name)

        output$submission_feedback <- renderUI({
          div(class = "success-box", "\U1F64C Your notes have been saved!")
        })

      })
    }
  )
}
