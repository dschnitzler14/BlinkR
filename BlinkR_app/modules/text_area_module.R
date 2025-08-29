text_area_module_UI <- function(id, i18n, text_label = i18n$t("Type Your Notes Here"), text_value = i18n$t("Enter your Notes"), button_label = i18n$t("Save Notes")) {
  ns <- NS(id)
  tagList(
    textAreaInput(
      inputId = ns("text_input"),
      label = text_label,
      #value = text_value
    ),
    tags$i(
                    tagList(
                      shiny::icon("info-circle", class = "me-1 text-info"),
                      text_value
                    ),
                    style = "margin-top:-6px; display:block;",
                  ),
              tags$br(),

    div(
      style = "display: flex; justify-content: center; align-items: center; width: 100%;",
    actionButton(ns("inputTextButton"), label = button_label, class = tagList(icon("save"), "fun-save-button")
    )
),
    uiOutput(ns("submission_feedback"))
  )
}

text_area_module_server <- function(id, i18n, auth, filename = "Filename", time = "") {
  moduleServer(
    id,
    function(input, output, session) {
            vars <- get_experiment_vars()

      observeEvent(input$inputTextButton, {
        

        text_to_save <- input$text_input

        temp_file <- tempfile(fileext = ".txt")
        writeLines(text_to_save, temp_file)

        group = auth()$user_info$group

        pathname <- paste0("BlinkR_text_results/", group)

        dt <- lubridate::now(tzone = "")

        time <- format(dt, "_%Hh_%Mm_%Ss_")

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
          div(class = "success-box", i18n$t("🙌 Your notes have been saved!"))
        })

      cat("group name ", group, " saved to path", pathname, "as filename", name , "\n")


      })
    }
  )
}

