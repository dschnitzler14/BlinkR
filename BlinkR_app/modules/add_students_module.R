group_info_module_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    textInput(
      inputId = ns("student_initials"),
      label = i18n$t("Enter Student Initials"),
      placeholder = i18n$t("Type initials here")
    ),
    actionButton(
      inputId = ns("generate_id"),
      label = tagList(shiny::icon("circle-plus"), i18n$t("Generate ID")),
      class = "fun-generate-button"
    )
  )
}

group_info_module_server <- function(id, i18n, db_student_table, auth) {
  moduleServer(
    id,
    function(input, output, session) {
            vars <- get_experiment_vars()

      ns <- session$ns

      next_id <- reactiveVal(1)

      observeEvent(input$generate_id, {
        initials <- trimws(input$student_initials)

        if (initials == "") {
          showNotification(i18n$t("Please enter initials."), type = "error", duration = 3)
          return()
        }

        current_table <- db_student_table()

        new_entry <- data.frame(
          group = as.character(auth()$user_info$group),
          id = as.integer(next_id()),
          initials = as.character(initials),
          remove = as.character("Remove"),
          stringsAsFactors = FALSE
        )

        db_student_table(rbind(current_table, new_entry))

        next_id(next_id() + 1)

        updateTextInput(session, "student_initials", value = "")

        showNotification(i18n$t("Student added to the database."), type = "message", duration = 3)
      })


    }
  )
}




