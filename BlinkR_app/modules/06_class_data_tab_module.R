class_data_module_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
            column(
              width = 12,
              div(
                class = "page-title-box",
                tags$h2(
                  tagList(shiny::icon("database"), i18n$t("Raw Data"))
                )
      )
    )),
      fluidRow(
    tabBox(
      title = i18n$t("Raw Data"),
      id = ns("data_box"),
      width = 12,
      side = "left",
      tabItem(
        tabName = i18n$t("Your Group Data"),
        title = tagList(shiny::icon("user"), i18n$t("Your Group Data")),
        DT::dataTableOutput(ns("group_data"))
      ),
      tabItem(
        tabName = i18n$t("Class Data"),
        title = tagList(shiny::icon("users"), i18n$t("Class Data")),
        DT::dataTableOutput(ns("class_data")),
        textOutput(ns("no_data_text"))
      )
    ),
    ),
    fluidRow(
  column(
    width = 12,
    div(
      style = "
        display: flex; 
        justify-content: center; 
        align-items: center; 
        gap: 10px;          
        margin: 0; 
        padding: 10px;
      ",
      actionButton(
        ns("back_page_data"),
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_data"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
    
  )
  )
}

class_data_module_server <- function(
    id,
    i18n,
    db_measurement,
    BlinkR_measurement_sheet,
    parent.session,
    auth
) {
  moduleServer(
    id,
    function(input, output, session) {
      vars <- get_experiment_vars()

      ns <- session$ns
  
      observeEvent(input$back_page_data, {
        updateTabItems(parent.session, "sidebar", "Measurements")
      })
      observeEvent(input$next_page_data, {
        updateTabItems(parent.session, "sidebar", "Playground")
      })
      
      view_permission = auth()$user_info$Data
      
      if (is.null(view_permission) || length(view_permission) == 0) {
        view_permission <- "FALSE"
      }
      
      current_data <- reactiveVal()
      
      if (view_permission == "TRUE") {
        observe({
          combined_class_data_id <- tryCatch(
            drive_get("BlinkR_Combined_Class_Data")$id,
            error = function(e) NULL
          )
          
          if (!is.null(combined_class_data_id)) {
            combined_data <- tryCatch(
              read_sheet(combined_class_data_id),
              error = function(e) NULL
            )
            
            current_data <- reactiveVal(combined_data)
            
            output$class_data <- renderDT({
              req(current_data())
              DT::datatable(current_data(), options = list(pageLength = 10))
            })
          } else {
          }
        })
      } else {
        output$no_data_text <- renderText(i18n$t("Check back later for the Class Data"))
      }
      
# Group Data 
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
        
        group_name <- unique(measurement_data$group)[1]
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


