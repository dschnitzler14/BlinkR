write_up_module_ui <- function(id) {
  ns <- NS(id)
  writing_up_tab <- 
    tabItem(tabName = "Writing-Up",
      fluidPage(
        fluidRow(
          column(
            8,
              box(title = "Introduction",
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 12,
                  solidHeader = TRUE,
                  fluidRow(
                  column(6, 
                        md_input_ui(ns("introduction"), "Introduction")
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
                        md_input_ui(ns("methods"), "Methods")

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
                        md_input_ui(ns("results"), "Results")

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
                      md_input_ui(ns("discussion"), "Discussion")
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
                      md_input_ui(ns("future_work"), "Future Work")

                  ),
                  column(6,
                         includeMarkdown(here("BlinkR_app", "markdown", "08_writing_up", "writing_up_future_work.Rmd"))
                         
                         )
                )
            )
          ),
          column(4,
                 uiOutput(ns("markdown_preview"))
                 
          
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
    ),
     fluidRow(
      column(
      width = 12,
      div(
        style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
        actionButton(ns("back_page"),
                     label = tagList(icon("arrow-left"), "Back"))
          )
        ),
      ),
  )
)
}

write_up_module_server <- function(id, parent.session, auth, reload_trigger){
  moduleServer(
    id,
    function(input, output, server){
      
    observeEvent(input$back_page, {
      updateTabItems(parent.session, "sidebar", "Feedback")
    })
      
      intro_markdown <- md_input_server("introduction")
      methods_markdown <- md_input_server("methods")
      results_markdown <- md_input_server("results")
      discussion_markdown <- md_input_server("discussion")
      future_work_markdown <- md_input_server("future_work")
      
      # Combine Markdown sections dynamically
      combined_markdown <- reactive({
        paste0(
          "# Introduction\n", intro_markdown(), "\n\n",
          "# Methods\n", methods_markdown(), "\n\n",
          "# Results\n", results_markdown(), "\n\n",
          "# Discussion\n", discussion_markdown(), "\n\n",
          "# Future Work\n", future_work_markdown(), "\n"
        )
      })
      
      # Render combined Markdown for live preview
      output$markdown_preview <- renderUI({
        HTML(markdown::markdownToHTML(text = combined_markdown(), fragment.only = TRUE))
      })
      #Intro <- "Intro"
      
      # text_area_module_server("write_up_intro", auth, "Intro", reload_trigger)
      # text_area_module_server("write_up_methods", auth, "Methods", reload_trigger)
      # text_area_module_server("write_up_results", auth, "Results", reload_trigger)
      # text_area_module_server("write_up_discussion", auth, "Discussion", reload_trigger)
      # text_area_module_server("write_up_future", auth, "Future", reload_trigger)
      # 
      # concat_notes_server("concat_write_up", auth)
      
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
