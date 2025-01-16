write_up_module_ui <- function(id, session_folder_url) {
  ns <- NS(id)
  writing_up_tab <- 
    tabItem(tabName = "Writing-Up",
      fluidPage(
        fluidRow(
          column(
            width = 12,
            div(
              style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
              actionButton(ns("link_to_drive"),
                           label = tagList(icon("google-drive"), "View on Google Drive"),
                           class = "action-button custom-action"
              )
            )
          ),
        ),
        fluidRow(
          column(
            12,
              box(title = "Introduction",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 12,
                  solidHeader = TRUE,
                  fluidRow(
                    fluidRow(
                      column(
                        width = 12,
                        div(
                          style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
                          #download_handler_ui("download_script", "Download R Script")
                        )
                      ),
                    ),
                  column(6, 
                        text_area_module_UI(ns("introduction"), "Introduction")
                        ),
                  column(6,
                         includeMarkdown(here("BlinkR_app", "markdown","08_writing_up","writing_up_intro.Rmd")),
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
                         textOutput(ns("folder_url")),
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
                        text_area_module_UI(ns("methods"), "Methods")

                  ),
                  column(6,
                         includeMarkdown(here("BlinkR_app", "markdown","08_writing_up","writing_up_methods.Rmd")),
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
                        text_area_module_UI(ns("results"), "Results")

                  ),
                  column(6,
                         includeMarkdown(here("BlinkR_app", "markdown", "08_writing_up", "writing_up_results.Rmd")),
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
                      text_area_module_UI(ns("discussion"), "Discussion")
                  ),
                  column(6,
                         includeMarkdown(here("BlinkR_app", "markdown", "08_writing_up", "writing_up_discussion.Rmd"))
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
                      text_area_module_UI(ns("future_work"), "Future Work")

                  ),
                  column(6,
                         includeMarkdown(here("BlinkR_app", "markdown", "08_writing_up", "writing_up_future_work.Rmd"))
                         
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

write_up_module_server <- function(id, parent.session, auth, reload_trigger, session_folder_id){
  moduleServer(
    id,
    function(input, output, server){
      
      observeEvent(input$link_to_drive, {
        showModal(modalDialog(
          title = "Your Google Drive",
          your_google_drive_module_ui("your_drive_module_write_up"),
          
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "l" 
        ))    
      })
    
      your_google_drive_module_server("your_drive_module_write_up", session_folder_id = session_folder_id)
      
      
      # observeEvent(input$view_google_folder, {
      #   shinyjs::runjs(sprintf("window.open('%s', '_blank');", shQuote(session_folder_id, type = "cmd")))
      # })

    observeEvent(input$back_page, {
      updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
    })
    
    observeEvent(input$next_page, {
      updateTabItems(parent.session, "sidebar", "Feedback")
    })
      
      text_area_module_server("introduction", auth, "Intro")
      text_area_module_server("methods", auth, "Methods")
      text_area_module_server("results", auth, "Results")
      text_area_module_server("discussion", auth, "Discussion")
      text_area_module_server("future", auth, "Future")

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
