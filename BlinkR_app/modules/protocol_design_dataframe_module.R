experimental_design_module_ui <- function(id, label = "Protocol Planning Notes", placeholder = "Placeholder") {
  ns <- NS(id)
  tagList(
    textAreaInput(
      ns("experimental_design"),
      label = label,
      placeholder = placeholder
    ),
    actionButton(ns("submit_protocol_notes"), tagList(shiny::icon("save"), "Save Notes"), class = "fun-save-button")
    )
}

experimental_design_module_server <- function(id, auth, protocol_file_id, sheet_name = "Protocol_Notes", input_question = "Input Question") {
  moduleServer(id, function(input, output, session) {
    vars <- get_experiment_vars()

    ns <- session$ns
    
    protocol_notes_db <- reactiveVal(data.frame())
    
    observeEvent(input$submit_protocol_notes, {
      text_to_save <- input$experimental_design
      group_name <- auth()$user_info$Group
      
      new_entry <- data.frame(
        Group = group_name,
        InputQuestion = input_question,
        Experimental_Design = text_to_save,
        stringsAsFactors = FALSE
      )
      
      current_data <- protocol_notes_db()
      updated_data <- rbind(current_data, new_entry)
      protocol_notes_db(updated_data)
      
      tryCatch({
        print(new_entry)
        sheet_write(data = new_entry, ss = protocol_file_id, sheet = sheet_name)
        showNotification("Protocol notes submitted successfully!", type = "message", duration = 3)
      }, error = function(e) {
        showNotification("Error saving protocol notes: Please try again.", type = "error", duration = 3)
      })
    })
  })
}
