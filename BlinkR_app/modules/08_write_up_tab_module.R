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
                    text_area_module_UI(ns("write_up_intro"))
                    ),
                  column(6,
                         includeMarkdown("markdown/08_writing_up/writing_up_intro.Rmd"),
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
                         #actionButton(ns("background"), "Go to Background"),
                         #actionButton(ns("hypothesis"), "Go to Hypothesis")
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
                         includeMarkdown("markdown/08_writing_up/writing_up_methods.Rmd"),
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
                         # actionButton(ns("protocol"), "Go to Protocol"),
                         # actionButton(ns("analysis_dashboard"), "Go to Analysis Dashboard")
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
                         includeMarkdown("markdown/08_writing_up/writing_up_results.Rmd"),
                         actionButton(
                           ns("analysis_dashboard"),
                           label = tagList(icon("dashboard"), "Go to Analysis Dashboard"),
                           class = "action-button custom-action",
                         ),
                         #actionButton(ns("analysis_dashboard"), "Go to Analysis Dashboard")
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
                         includeMarkdown("markdown/08_writing_up/writing_up_discussion.Rmd")
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
                         includeMarkdown("markdown/08_writing_up/writing_up_future_work.Rmd")
                         
                         )
                )
            )
          ), 
    )
  )
)
}

write_up_module_server <- function(id, parent.session){
  moduleServer(
    id,
    function(input, output, server){
      text_area_module_server("write_up_intro")
      text_area_module_server("write_up_methods")
      text_area_module_server("write_up_results")
      text_area_module_server("write_up_discussion")
      text_area_module_server("write_up_future")
      
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
