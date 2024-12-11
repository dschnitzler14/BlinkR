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
    DT::dataTableOutput(ns("student_table"))
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
        
        if (initials %in% current_table$Initials) {
          showNotification("These initials already exist in the database. Please enter different initials.", type = "error")
          return()
        }
        
        # Create the new entry
        new_entry <- data.frame(
          Group = auth()$user_info$user,
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
      
  
      output$student_table <- renderDT({

        datatable(
          db_student_table(),
          escape = FALSE,
          options = list(
            autoWidth = TRUE
          ),
          rownames = FALSE
        )
      }, server = TRUE)
      
      observeEvent(input$student_table_cell_clicked, {
        info <- input$student_table_cell_clicked
        if (!is.null(info) && !is.null(info$row) && info$col == (ncol(db_student_table()) - 1)) {
          student_id_to_remove <- db_student_table()$ID[info$row]
          updated_db <- db_student_table()[db_student_table()$ID != student_id_to_remove, ]
          db_student_table(updated_db)
          showNotification("Student removed from the database.", type = "warning")
        }
      })
    }
  )
}





