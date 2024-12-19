# Class Data Module UI
class_data_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabBox(
      title = "Raw Data",
      id = ns("data_box"),
      width = 12,
      side = "left",
      tabItem(
        tabName = "Your Group Data",
        title = "Your Group Data",
        DT::dataTableOutput(ns("group_data"))
      ),
      tabItem(
        tabName = "Class Data",
        title = "Class Data",
        DT::dataTableOutput(ns("class_data"))
      )
    )
  )
}

class_data_module_server <- function(id, db_measurement, BlinkR_measurement_sheet) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      dummy_data <- read.csv(here("BlinkR_app", "data","dummy_blinking_data.csv")) 
      
      output$class_data <- renderDT({
        datatable(
          dummy_data,
          escape = FALSE,
          options = list(
            paging = FALSE,
            searching = FALSE,
            autoWidth = TRUE,
            lengthChange = FALSE,
            scrollY = FALSE
          ),
          rownames = FALSE
        )
      }, server = TRUE)
      
      
      group_data_trigger <- reactiveVal(0)

      observeEvent(input$refresh_group_data, {
        group_data_trigger(group_data_trigger() + 1)
        
      })

      output$group_data <- renderDT({
        req(group_data_trigger())
        measurement_data <- db_measurement()
        
        datatable(
          measurement_data,
          escape = FALSE,
          options = list(
            paging = FALSE,
            searching = FALSE,
            autoWidth = TRUE,
            lengthChange = FALSE,
            scrollY = FALSE
          ),
          rownames = FALSE
        )
      }, server = TRUE)
      
      observe({
        measurement_data <- db_measurement()
        req(!is.null(measurement_data), nrow(measurement_data) > 0)
        
        group_name <- unique(measurement_data$Group)[1]
        existing_sheets <- sheet_names(ss = BlinkR_measurement_sheet)
        sheet_name <- paste0(group_name, "_", Sys.Date())
        
        if (!(sheet_name %in% existing_sheets)) {
          sheet_add(ss = BlinkR_measurement_sheet, sheet = sheet_name)
          
          sheet_write(data = measurement_data,
                      ss = BlinkR_measurement_sheet,
                      sheet = sheet_name)
        } else {
          sheet_write(data = measurement_data,
                       ss = BlinkR_measurement_sheet,
                       sheet = sheet_name)
        }
      })
      
    }
  )
}
