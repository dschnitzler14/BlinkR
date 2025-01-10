group_info_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(
      inputId = ns("student_initials"),
      label = "Enter Student Initials",
      placeholder = "Type initials here"
    ),
    actionButton(
      inputId = ns("generate_id"),
      label = "Generate ID",
      class = "fun-submit-button"
    )
  )
}

group_info_module_server <- function(id, db_student_table, auth) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      next_id <- reactiveVal(1)

      observeEvent(input$generate_id, {
        initials <- trimws(input$student_initials)

        if (initials == "") {
          showNotification("Please enter initials.", type = "error")
          return()
        }

        current_table <- db_student_table()

        new_entry <- data.frame(
          Group = auth()$user_info$Group,
          ID = next_id(),
          Initials = initials,
          Remove = "Remove",
          stringsAsFactors = FALSE
        )

        db_student_table(rbind(current_table, new_entry))

        next_id(next_id() + 1)

        updateTextInput(session, "student_initials", value = "")

        showNotification("Student added to the database.", type = "message")
      })


    }
  )
}




