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
      class = "btn-primary"
    ),
    h3("Student Database"),
    DTOutput(ns("student_table"))
  )
}

group_info_module_server <- function(id, db) {
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
        
        if (initials %in% db()$Initials) {
          showNotification("These initials already exist in the database. Please enter different initials.", type = "error")
          return()
        }
                new_entry <- data.frame(
          ID = next_id(),
          Initials = initials,
          Unstressed_1 = NA, Unstressed_2 = NA, Unstressed_3 = NA,
          Stressed_1 = NA, Stressed_2 = NA, Stressed_3 = NA,
          Remove = "Remove",  # Text label instead of HTML button
          stringsAsFactors = FALSE
        )
        db(rbind(db(), new_entry))
        
        next_id(next_id() + 1)
        
        updateTextInput(session, "student_initials", value = "")
        
        showNotification("Student added to the database.", type = "message")
      })
      
      output$student_table <- renderDT({
        datatable(
          db(),
          options = list(
            autoWidth = TRUE
          ),
          rownames = FALSE,
          selection = "none"
        )
      }, server = TRUE)
      
      observeEvent(input$student_table_cell_clicked, {
        info <- input$student_table_cell_clicked
        if (!is.null(info) && !is.null(info$row) && info$col == (ncol(db()) - 1)) {
          student_id_to_remove <- db()$ID[info$row]
          updated_db <- db()[db()$ID != student_id_to_remove, ]
          db(updated_db)
          showNotification("Student removed from the database.", type = "warning")
        }
      })
    }
  )
}





