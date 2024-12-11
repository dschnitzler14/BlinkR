view_data_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
      DT::dataTableOutput(ns("data_table")) 
    )
  }

view_data_module_server <- function(id, database, enable_remove = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize reactive data
    reactive_data <- reactiveVal({
      if (!is.null(database)) {
        if (inherits(database, "reactive")) { # Check if database is reactive
          database()
        } else {
          database
        }
      } else {
        data.frame() # Fallback for NULL values
      }
    })
    
    # Render the data table
    output$data_table <- renderDT({
      data <- reactive_data()
      
      # Ensure data is a valid data frame
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
        data <- data.frame(Message = "No data available")
      }
      
      # Add "Remove" column if enabled
      if (enable_remove) {
        data <- data %>%
          mutate(
            Remove = sprintf(
              '<button class="btn btn-danger btn-sm" id="remove_%s">Remove</button>',
              row_number()
            )
          )
      }
      
      datatable(
        data,
        escape = FALSE,
        options = list(
          autoWidth = TRUE,
          paging = TRUE,
          searching = TRUE
        ),
        rownames = FALSE
      )
    }, server = TRUE)
    
    # Handle remove button clicks if enabled
    if (enable_remove) {
      observeEvent(input$data_table_cell_clicked, {
        info <- input$data_table_cell_clicked
        
        # Check if a valid "Remove" button is clicked
        if (!is.null(info) && info$col == ncol(reactive_data()) - 1) {
          current_data <- reactive_data()
          updated_data <- current_data[-info$row, ] # Remove the clicked row
          reactive_data(updated_data) # Update the reactive value
        }
      })
    }
    
    # Expose the reactive data for other modules to use
    return(reactive(reactive_data()))
  })
}



