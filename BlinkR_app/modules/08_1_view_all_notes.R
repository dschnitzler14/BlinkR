view_all_notes_ui <- function(id) {
  ns <- NS(id)
  view_notes_tab <- 
      tabItem(tabName = "View_Notes",
      fluidPage(
        fluidRow(
          column(
            12,
              box(title = "Introduction",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 12,
                  solidHeader = TRUE,
                  fluidRow(
                  column(12, 
                    existing_data_module_ui(ns("existing_intro"))
                    ),
                  )
                ),
            box(title = "Methods",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(12, 
                         existing_data_module_ui(ns("existing_methods"))

                  )
                )
            ),
            box(title = "Results",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(12, 
                         existing_data_module_ui(ns("existing_results"))

                  ),
                  
                )
            ),
            box(title = "Discussion",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(6, 
                        existing_data_module_ui(ns("existing_discussion"))

                  ),
                  
                )
            ),
            box(title = "Future Work",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(6, 
                         existing_data_module_ui(ns("existing_future"))

                  ),
                  
                )
            )
          ), 
    ),
    fluidRow(
      column(
        width = 12,
        div(
          style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
          concat_notes_ui(ns("concat_write_up"))
        )
      ),
    ),
     fluidRow(
      column(
      width = 12,
      div(
        style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
        actionButton(ns("back_page"),
                     label = tagList(icon("arrow-left"), "Back")),
        actionButton(ns("next_page"), 
              label = tagList("Next", icon("arrow-right")))
          )
        ),
      ),
  )
)

}

view_all_notes_server <- function(id, parent.session, auth, reload_trigger){
  moduleServer(
    id,
    function(input, output, server){
      
      #ns <- session$ns
      
      req(auth()$user_auth)
      
      observeEvent(input$back_page, {
      updateTabItems(parent.session, "sidebar", "Introduction")
    })
      observeEvent(input$next_page, {
      updateTabItems(parent.session, "sidebar", "Feedback")
    })
      
      existing_data_module_server("existing_intro", auth, "Intro", reload_trigger)
      existing_data_module_server("existing_methods", auth, "Methods", reload_trigger)
      existing_data_module_server("existing_results", auth, "Results", reload_trigger)
      existing_data_module_server("existing_discussion", auth, "Discussion", reload_trigger)
      existing_data_module_server("existing_future", auth, "Future", reload_trigger)

      concat_notes_server("concat_write_up", auth)
      

    }
  )
}
