write_up_module_ui <- function(id) {
  ns <- NS(id)
  writing_up_tab <- 
    tabItem(tabName = "Writing-Up",
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
                  column(6, 
                    text_area_module_UI(ns("write_up_intro")),
                    existing_data_module_ui(ns("existing_intro"))
                    ),
                  column(6,
                         includeMarkdown(here("BlinkR_app", "markdown","writing_up_intro.Rmd")),
                         actionButton(
                           ns("background"),
                           label = tagList(icon("book-open"), "Go to Background"),
                           class = "action-button custom-action",
                           
                         ),
                         actionButton(
                           ns("hypothesis"),
                           label = tagList(icon("pen-to-square"), "Go to Hypothesis"),
                           class = "action-button custom-action",
                         ),
                        
                  )
                  )
                ),
            box(title = "Methods",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(6, 
                         text_area_module_UI(ns("write_up_methods"))
                  ),
                  column(6,
                         includeMarkdown(here("BlinkR_app", "markdown","writing_up_methods.Rmd")),
                         actionButton(
                           ns("protocol"),
                           label = tagList(icon("list"), "Go to Protocol"),
                           class = "action-button custom-action",
                         ),
                         actionButton(
                           ns("analysis_dashboard"),
                           label = tagList(icon("dashboard"), "Go to Analysis Dashboard"),
                           class = "action-button custom-action",
                         ),
                         
                         )
                )
            ),
            box(title = "Results",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(6, 
                         text_area_module_UI(ns("write_up_results"))
                  ),
                  column(6,
                         includeMarkdown(here("BlinkR_app", "markdown","writing_up_results.Rmd")),
                         actionButton(
                           ns("analysis_dashboard"),
                           label = tagList(icon("dashboard"), "Go to Analysis Dashboard"),
                           class = "action-button custom-action",
                         ),
                         )
                )
            ),
            box(title = "Discussion",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(6, 
                         text_area_module_UI(ns("write_up_discussion"))
                  ),
                  column(6,
                         includeMarkdown(here("BlinkR_app", "markdown","writing_up_discussion.Rmd"))
                         )
                )
            ),
            box(title = "Future Work",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(6, 
                         text_area_module_UI(ns("write_up_future"))
                  ),
                  column(6,
                         includeMarkdown(here("BlinkR_app", "markdown","writing_up_future_work.Rmd"))
                         
                         )
                )
            )
          ), 
    ),
    fluidRow(
      column(
        width = 12,
        div(
          style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
          concat_notes_ui("concat_write_up")
        )
      ),
    )
  )
)
}

write_up_module_server <- function(id, parent.session, auth){
  moduleServer(
    id,
    function(input, output, server){
      
      existing_data_module_server("existing_intro", auth, "Intro")
      
      #Intro <- "Intro"
      
      text_area_module_server("write_up_intro", auth, "Intro")
      text_area_module_server("write_up_methods", auth, "Methods")
      text_area_module_server("write_up_results", auth, "Results")
      text_area_module_server("write_up_discussion", auth, "Discussion")
      text_area_module_server("write_up_future", auth, "Future")
      
      concat_notes_server("concat_write_up", auth)
      
      observeEvent(input$background, {
        updateTabItems(parent.session, "sidebar", "Background")
      })
      observeEvent(input$hypothesis, {
        updateTabItems(parent.session, "sidebar", "Hypothesis")
      })
      observeEvent(input$protocol, {
        updateTabItems(parent.session, "sidebar", "Protocol")
      })
      observeEvent(input$analysis_dashboard, {
        updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
      })

    }
  )
}
