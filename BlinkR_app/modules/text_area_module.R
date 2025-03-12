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

text_area_module_server <- function(id, auth, filename = "Filename", time = "") {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$inputTextButton, {
        

        text_to_save <- input$text_input

        temp_file <- tempfile(fileext = ".txt")
        writeLines(text_to_save, temp_file)

        Group = auth()$user_info$Group

        pathname <- paste0("BlinkR_text_results/", Group)

        name <- paste0(
          filename,
          time,
          ".txt"
        )

       
        drive_upload(
          media = temp_file,
          path = pathname,
          name = name,
          overwrite = TRUE
        )

        output$submission_feedback <- renderUI({
          div(class = "success-box", "\U1F64C Your notes have been saved!")
        })

      })
    }
  )
}

