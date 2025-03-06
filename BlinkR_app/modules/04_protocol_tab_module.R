protocol_module_ui <- function(id){
  ns <- NS(id)
  protocol_tab <- tabItem(tabName = "Protocol",
                          fluidPage(
                            fluidRow(
                                  column(
                                    width = 12,
                                    div(
                                      class = "page-title-box",
                                      tags$h2(
                                        tagList(shiny::icon("list"), "Protocol")
                                      )
                            )
                          )

                                ),
                            fluidRow(
                              column(12,
                                tabBox(
                                  width = 12,
                                  tabPanel(
                                    tabName = "Your Protocol",
                                    title =  tagList(shiny::icon("user"), "Your Protocol"),
                                    experimental_design_module_ui(ns("experimental_design_protocol"), "Experimental Design", "Think about some general ideas for the experiment here"),
                                    experimental_design_module_ui(ns("measurement_protocol"), "Measurement", "How will you record measurements?"),
                                    experimental_design_module_ui(ns("analysis_protocol"), "Analysis", "How will you analyse your results?")
                                    ),
                                  tabPanel(
                                    tabName = "Class Protocol",
                                    title = tagList(shiny::icon("users"), "Class Protocol"),
                                    DT::dataTableOutput(ns("class_protocol")),
                                    textOutput(ns("no_protocol_text"))
                                  )
                                )
                              )
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
        ns("back_page_protocol"),
        label = tagList(icon("arrow-left"), " Back"),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_protocol"), 
        label = tagList("Next ", icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
)
  )
}

protocol_module_server <- function(id, auth, parent.session, protocol_file_id){
  moduleServer(
    id,
    function(input, output, server){
      
   
      experimental_design_module_server("experimental_design_protocol", auth, protocol_file_id, "Experimental Design", "What is your general design?")
      experimental_design_module_server("measurement_protocol", auth, protocol_file_id, "Measurement", "How will you record measurements?")
      experimental_design_module_server("analysis_protocol", auth, protocol_file_id, "Experimental Design", "How will you analyse your results?")
      
      view_permission_protocol = auth()$user_info$Protocol

      if (is.null(view_permission_protocol) || length(view_permission_protocol) == 0) {
        view_permission_protocol <- "FALSE"
      }
      
      current_protocol <- reactiveVal()
      
      if (view_permission_protocol == "TRUE") {
        observe({
          combined_class_protocol_id <- tryCatch(
            drive_get("BlinkR_Class_Protocol")$id,
            error = function(e) NULL
          )
          
          if (!is.null(combined_class_protocol_id)) {
            combined_protocol <- tryCatch(
              read_sheet(combined_class_protocol_id, sheet = 1),
              error = function(e) NULL
            )
            
            current_protocol(combined_protocol)
            
            output$class_protocol <- DT::renderDataTable({
              req(current_protocol())
              
              formatted_data <- current_protocol()
              formatted_data[] <- lapply(formatted_data, function(col) {
                if (is.character(col)) {
                  gsub("\\n", "<br>", col)
                } else {
                  col
                }
              })
              
              DT::datatable(
                formatted_data,
                options = list(
                  paging = FALSE,        
                  searching = FALSE,     
                  ordering = FALSE,      
                  dom = 't',             
                  scrollX = TRUE         
                ),
                rownames = FALSE,
                escape = FALSE
              )
            })
          }
        })
      } else {
        output$no_protocol_text <- renderText("Check back later for the Class Protocol")
      }
      
      
       observeEvent(input$back_page_protocol, {
      updateTabItems(parent.session, "sidebar", "Hypothesis")
    })
      observeEvent(input$next_page_protocol, {
      updateTabItems(parent.session, "sidebar", "Measurements")
    })

    }
  )
}