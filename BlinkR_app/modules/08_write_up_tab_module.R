write_up_module_ui <- function(id, session_folder_url) {
  ns <- NS(id)
  writing_up_tab <- 
    tabItem(tabName = "Writing-Up",
      fluidPage(
        fluidRow(
            column(
              width = 12,
              div(
                class = "page-title-box",
                tags$h2(
                  tagList(shiny::icon("pen"), "Writing Up")
                )
      )
    )),
        fluidRow(
          column(
            width = 12,
            div(
              style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
              actionButton(ns("link_to_drive"),
                           label = tagList(icon("google-drive"), "View on Google Drive"),
                           class = "btn-primary"
              )
            )
          ),
        ),
        fluidRow(
          column(
            12,
              box(title = tagList(shiny::icon("lightbulb"), "Introduction"),
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
                        text_area_module_UI(ns("introduction"), "Introduction", button_label = tagList(shiny::icon("floppy-disk"), "Save Notes"))
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
                         textOutput(ns("folder_url")),
                  )
                  )
                ),
            box(title = tagList(shiny::icon("flask"), "Methods"),
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(6, 
                        text_area_module_UI(ns("methods"), "Methods", button_label = tagList(shiny::icon("floppy-disk"), "Save Notes"))

                  ),
                  column(6,
                         includeMarkdown("markdown/08_writing_up/writing_up_methods.Rmd"),
                         actionButton(
                           ns("protocol"),
                           label = tagList(icon("list"), "Go to Protocol"),
                           class = "action-button custom-action",
                         ),
                         actionButton(
                           ns("analysis_dashboard1"),
                           label = tagList(icon("dashboard"), "Go to Analysis Dashboard"),
                           class = "action-button custom-action",
                         ),
                         
                         )
                )
            ),
            box(title = tagList(shiny::icon("chart-bar"), "Results"),
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(6, 
                        text_area_module_UI(ns("results"), "Results", button_label = tagList(shiny::icon("floppy-disk"), "Save Notes"))

                  ),
                  column(6,
                         includeMarkdown("markdown/08_writing_up/writing_up_results.Rmd"),
                         actionButton(
                           ns("analysis_dashboard2"),
                           label = tagList(icon("dashboard"), "Go to Analysis Dashboard"),
                           class = "action-button custom-action",
                         ),
                         )
                )
            ),
            box(title = tagList(shiny::icon("comments"), "Discussion"),
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(6, 
                      text_area_module_UI(ns("discussion"), "Discussion", button_label = tagList(shiny::icon("floppy-disk"), "Save Notes"))
                  ),
                  column(6,
                         includeMarkdown("markdown/08_writing_up/writing_up_discussion.Rmd")
                         )
                )
            ),
            box(title = tagList(shiny::icon("forward"), "Future Work"),
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(6, 
                      text_area_module_UI(ns("future_work"), "Future Work", button_label = tagList(shiny::icon("floppy-disk"), "Save Notes"))

                  ),
                  column(6,
                         includeMarkdown("markdown/08_writing_up/writing_up_future_work.Rmd")
                         
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
      style = "
        display: flex; 
        justify-content: center; 
        align-items: center; 
        gap: 10px;          
        margin: 0; 
        padding: 10px;
      ",
      actionButton(
        ns("back_page_write"),
        label = tagList(icon("arrow-left"), " Back"),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_write"), 
        label = tagList("Next ", icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
  )
)
}

write_up_module_server <- function(id, parent.session, auth, reload_trigger, session_folder_id){
  moduleServer(
    id,
    function(input, output, session){
      

  observeEvent(input$link_to_drive, {
    req(session_folder_id)
        
    showModal(modalDialog(
      title = "Your Google Drive",
      your_google_drive_module_ui(session$ns("your_drive_module_write_up")),

      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l" 
    ))
    
    your_google_drive_module_server("your_drive_module_write_up", session_folder_id)

  })

    observeEvent(input$back_page_write, {
      updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
    })
    
    observeEvent(input$next_page_write, {
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
      observeEvent(input$analysis_dashboard1, {
        updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
      })
      observeEvent(input$analysis_dashboard2, {
        updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
      })

    }
  )
}
