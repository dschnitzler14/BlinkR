view_data_module_ui <- function(id, i18n) {
    ns <- NS(id)
    tagList(
      DT::dataTableOutput(ns("data_table")) 
    )
  }

view_data_module_server <- function(id, database, enable_remove = FALSE) {
  moduleServer(id, function(input, output, session) {
          vars <- get_experiment_vars()

    ns <- session$ns
    
    reactive_data <- reactiveVal({
      if (!is.null(database)) {
        if (inherits(database, "reactive")) {
          database()
        } else {
          database
        }
      } else {
        data.frame()
      }
    })
    
    output$data_table <- renderDT({
      data <- reactive_data()
      
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
        data <- data.frame(Message = "No data available")
      }
      
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
    
    if (enable_remove) {
      observeEvent(input$data_table_cell_clicked, {
        info <- input$data_table_cell_clicked
        
        if (!is.null(info) && info$col == ncol(reactive_data()) - 1) {
          current_data <- reactive_data()
          updated_data <- current_data[-info$row, ]
          reactive_data(updated_data)
        }
      })
    }
    
    return(reactive(reactive_data()))
  })
}



