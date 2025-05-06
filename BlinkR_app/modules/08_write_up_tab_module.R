write_up_module_ui <- function(id, i18n, session_folder_url) {
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
                  tagList(shiny::icon("pen"), i18n$t("Writing Up"))
                )
              )
    )),
        fluidRow(
          column(
            width = 6,
            div(
              style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
              actionButton(ns("link_to_drive"),
                           label = tagList(icon("google-drive"), i18n$t("View on Google Drive")),
                           class = "btn-primary"
              )
            )
          ),
          column(
            width = 6,
            div(
              style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
              downloadButton(ns("download_instructions"),
                           label = i18n$t("Download pdf of instructions"),
                           class = "btn-primary"
              )
            )
          ),
        ),
        fluidRow(
          column(
            12,
            div(
                class = "yellow-box",
                  tagList(i18n$t("Use this section to take notes as a group to plan your writing-up"))
                
              ),
              box(title = tagList(shiny::icon("paper-plane"), i18n$t("Introduction")),
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
                        )
                      ),
                    ),
                  column(6, 
                        text_area_module_UI(ns("introduction"), i18n, i18n$t("Introduction"), button_label = tagList(shiny::icon("save"), i18n$t("Save Notes")))
                        ),
                  column(6,
                         uiOutput(ns("writing_up_intro_markdown")),
                         actionButton(
                           ns("background"),
                           label = tagList(icon("book-open"), i18n$t("Go to Background")),
                           class = "action-button custom-action",
                           
                         ),
                         actionButton(
                           ns("hypothesis"),
                           label = tagList(icon("pen-to-square"), i18n$t("Go to Hypothesis")),
                           class = "action-button custom-action",
                         ),
                         textOutput(ns("folder_url")),
                  )
                  )
                ),
            box(title = tagList(shiny::icon("flask"), i18n$t("Methods")),
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(6, 
                        text_area_module_UI(ns("methods"), i18n, i18n$t("Methods"), button_label = tagList(shiny::icon("save"), i18n$t("Save Notes")))

                  ),
                  column(6,
                         uiOutput(ns("writing_up_methods_markdown")),
                         actionButton(
                           ns("protocol"),
                           label = tagList(icon("list"), i18n$t("Go to Protocol")),
                           class = "action-button custom-action",
                         ),
                         actionButton(
                           ns("analysis_dashboard1"),
                           label = tagList(icon("dashboard"), i18n$t("Go to Analysis Dashboard")),
                           class = "action-button custom-action",
                         ),
                         
                         )
                )
            ),
            box(title = tagList(shiny::icon("chart-bar"), i18n$t("Results")),
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(6, 
                        text_area_module_UI(ns("results"), i18n, i18n$t("Results"), button_label = tagList(shiny::icon("save"), i18n$t("Save Notes")))

                  ),
                  column(6,
                         uiOutput(ns("writing_up_results_markdown")),
                         actionButton(
                           ns("analysis_dashboard2"),
                           label = tagList(icon("dashboard"), i18n$t("Go to Analysis Dashboard")),
                           class = "action-button custom-action",
                         ),
                         )
                )
            ),
            box(title = tagList(shiny::icon("comments"), i18n$t("Discussion")),
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(6, 
                      text_area_module_UI(ns("discussion"), i18n, i18n$t("Discussion"), button_label = tagList(shiny::icon("save"), i18n$t("Save Notes")))
                  ),
                  column(6,
                         uiOutput(ns("writing_up_discussion_markdown"))
                         )
                )
            ),
            box(title = tagList(shiny::icon("forward"), i18n$t("Future Work")),
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                solidHeader = TRUE,
                fluidRow(
                  column(6, 
                      text_area_module_UI(ns("future_work"), i18n, i18n$t("Future Work"), button_label = tagList(shiny::icon("save"), i18n$t("Save Notes")))

                  ),
                  column(6,
                         uiOutput(ns("writing_up_future_work_markdown"))
                         
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
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_write"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
  )
)
}

write_up_module_server <- function(id, parent.session, auth, reload_trigger, session_folder_id, include_markdown_language){
  moduleServer(
    id,
    function(input, output, session){
            vars <- get_experiment_vars()

  output$writing_up_intro_markdown <- renderUI({
  include_markdown_language("08_writing_up/writing_up_intro.Rmd")
})

output$writing_up_methods_markdown <- renderUI({
  include_markdown_language("08_writing_up/writing_up_methods.Rmd")
})

output$writing_up_results_markdown <- renderUI({
  include_markdown_language("08_writing_up/writing_up_results.Rmd")
})

output$writing_up_discussion_markdown <- renderUI({
  include_markdown_language("08_writing_up/writing_up_discussion.Rmd")
})

output$writing_up_future_work_markdown <- renderUI({
  include_markdown_language("08_writing_up/writing_up_future_work.Rmd")
})



  observeEvent(input$link_to_drive, {
    req(session_folder_id)
        
    showModal(modalDialog(
      title = "Your Google Drive",
      your_google_drive_module_ui(session$ns("your_drive_module_write_up"), i18n),

      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l" 
    ))
    
    your_google_drive_module_server("your_drive_module_write_up", i18n, session_folder_id)

  })

  output$download_instructions <- downloadHandler(
    filename = function() {
      "BlinkR_writing_up_advice.pdf"
    },
    content = function(file) {
      file.copy("www/BlinkR_writing_up_advice.pdf", file)
    },
    contentType = "application/pdf"
  )


    observeEvent(input$back_page_write, {
      updateTabItems(parent.session, "sidebar", "AI")
    })
    
    observeEvent(input$next_page_write, {
      updateTabItems(parent.session, "sidebar", "Upload_Report")
    })
      
      text_area_module_server("introduction", auth, "Intro", time = paste0("_", format(Sys.time(), "%d%m%y_%H-%M")))
      text_area_module_server("methods", auth, "Methods", time = paste0("_", format(Sys.time(), "%d%m%y_%H-%M")))
      text_area_module_server("results", auth, "Results", time = paste0("_", format(Sys.time(), "%d%m%y_%H-%M")))
      text_area_module_server("discussion", auth, "Discussion", time = paste0("_", format(Sys.time(), "%d%m%y_%H-%M")))
      text_area_module_server("future", auth, "Future", time = paste0("_", format(Sys.time(), "%d%m%y_%H-%M")))

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
