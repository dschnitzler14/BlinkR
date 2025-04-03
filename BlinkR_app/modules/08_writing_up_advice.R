writing_up_advice_ui <- function(id, i18n) {
    ns <- NS(id)
  writing_up_advice_tab <- tabItem(tabName = "Writing_Up_Advice",
                              fluidPage(
                                fluidRow(
            column(
              width = 12,
              div(
                class = "page-title-box",
                tags$h2(
                  tagList(shiny::icon("circle-question"), i18n$t("Writing Up: Advice"))
                )
      )
    )),
                                fluidRow(
                                  box(
                                    title = tagList(shiny::icon("file-alt"), i18n$t("Structure")),
                                    id = "structure",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    class = "clickable-box",
                                    uiOutput(ns("writing_up_advice_structure")) 
                                  ),
                                  box(
                                    title = tagList(shiny::icon("paper-plane"), i18n$t("Introduction")),
                                    id = "structure_introduction",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("writing_up_advice_structure_introduction")) 
                                  ),
                                  box(
                                    title = tagList(shiny::icon("flask"), i18n$t("Methods")),
                                    id = "structure_methods",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("writing_up_advice_structure_methods")) 
                                  ),
                                  box(
                                    title = tagList(shiny::icon("chart-bar"), i18n$t("Results")),
                                    id = "structure_results",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("writing_up_advice_structure_results")) 
                                  ),
                                  box(
                                    title = tagList(shiny::icon("comments"), i18n$t("Discussion")),
                                    id = "structure_discussion",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("writing_up_advice_structure_discussion"))
                                  ),
                                  box(
                                    title = tagList(shiny::icon("forward"), i18n$t("Future Work")),
                                    id = "structure_future",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("writing_up_advice_structure_future_work"))
                                  ),
                                    box(
                                    title = tagList(shiny::icon("book"), i18n$t("References")),
                                    id = "structure_references",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("writing_up_advice_structure_references"))
                                    ),
                                    box(
                                    title = tagList(shiny::icon("tools"), i18n$t("Tips and Tricks")),
                                    id = "structure_tips",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("writing_up_advice_structure_tips"))
                                    ),
                                    box(
                                    title = tagList(shiny::icon("users"), i18n$t("Working Together")),
                                    id = "working_together_tips",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("writing_up_advice_structure_together"))
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
        ns("back_page_advice"),
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_advice"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
                              )
  )
    }

writing_up_advice_server <- function(id, i18n, parent.session, include_markdown_language) {
  moduleServer(
    id,
    function(input, output, session){

      output$writing_up_advice_structure <- renderUI({
        include_markdown_language("08_writing_up/writing_up_advice_structure.Rmd")
      })

      output$writing_up_advice_structure_introduction <- renderUI({
        include_markdown_language("08_writing_up/writing_up_advice_structure_introduction.Rmd")
      })

      output$writing_up_advice_structure_methods <- renderUI({
        include_markdown_language("08_writing_up/writing_up_advice_structure_methods.Rmd")
      })

      output$writing_up_advice_structure_results <- renderUI({
        include_markdown_language("08_writing_up/writing_up_advice_structure_results.Rmd")
      })

      output$writing_up_advice_structure_discussion <- renderUI({
        include_markdown_language("08_writing_up/writing_up_advice_structure_discussion.Rmd")
      })

      output$writing_up_advice_structure_future_work <- renderUI({
        include_markdown_language("08_writing_up/writing_up_advice_structure_future_work.Rmd")
      })

      output$writing_up_advice_structure_references <- renderUI({
        include_markdown_language("08_writing_up/writing_up_advice_structure_references.Rmd")
      })

      output$writing_up_advice_structure_tips <- renderUI({
        include_markdown_language("08_writing_up/writing_up_advice_structure_tips.Rmd")
      })

      output$writing_up_advice_structure_together <- renderUI({
        include_markdown_language("08_writing_up/writing_up_advice_structure_together.Rmd")
      })



    observeEvent(input$back_page_advice, {
        updateTabItems(parent.session, "sidebar", "Statistical_Analysis")
      })
      observeEvent(input$next_page_advice, {
        updateTabItems(parent.session, "sidebar", "AI")
      })

    }
  )

}

