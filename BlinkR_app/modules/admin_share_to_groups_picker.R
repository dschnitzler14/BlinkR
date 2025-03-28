share_to_groups_admin_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("group_picker")),
    textOutput(ns("selection_output")),
    actionButton(ns("share_to_selected_groups"), 
                 label = tagList(icon("eye"), "Share to Selected Groups"),
                 class = "fun-save-button"
    ),
    actionButton(ns("hide_from_selected_groups"), 
                 label = tagList(icon("eye-slash"), "Hide from Selected Groups"),
                 class = "fun-save-button"
    ),
  )
}


share_to_groups_admin_module_server <- function(id, user_base_data, column, user_base_sheet_id) {
  moduleServer(id, function(input, output, session) {
          vars <- get_experiment_vars()


    ns <- session$ns
    
    user_base <- user_base_data
    
    date_group_choices <- user_base %>%
      filter(!is.na(Date)) %>%   
      group_by(Date) %>%         
      summarize(Groups = list(unlist(group))) %>% 
      deframe()  
    
    output$group_picker <- renderUI(
      virtualSelectInput(
        inputId = ns("group_input"),
        label = "Select Groups by Date:",
        choices = date_group_choices,  
        showValueAsTags = TRUE,
        search = TRUE,
        multiple = TRUE
      )
    )
    
    output$selection_output <- renderText({
      if (is.null(input$group_input) || length(input$group_input) == 0) {
        "No groups selected."
      } else {
        paste("Groups to Share To:", paste(input$group_input, collapse = ", "))
      }
    })
    
   
    
    observeEvent(input$share_to_selected_groups, {
      if (is.null(input$group_input) || length(input$group_input) == 0) {
        showNotification("No groups selected to share.", type = "warning", duration = 3)
        return()
      }
      
      for (group in input$group_input) {
        row_index <- which(user_base$group == group)
        
        cell_range <- paste0(column, row_index + 1)
        
        range_write(
          ss = user_base_google_sheet,
          data = data.frame("TRUE"),
          range = cell_range,
          col_names = FALSE
        )
      }
      
      showNotification("Data shared!", type = "message", duration = 3)
    })
    
    observeEvent(input$hide_from_selected_groups, {
      if (is.null(input$group_input) || length(input$group_input) == 0) {
        showNotification("No groups selected to share.", type = "warning", duration = 3)
        return()
      }
      
      for (group in input$group_input) {
        row_index <- which(user_base$group == group)
        
        cell_range <- paste0(column, row_index + 1)
        
        range_write(
          ss = user_base_google_sheet,
          data = data.frame("FALSE"),
          range = cell_range,
          col_names = FALSE
        )
      }
      
      showNotification("Data hidden!", type = "message", duration = 3)
    })
    
    
  }
  )
}