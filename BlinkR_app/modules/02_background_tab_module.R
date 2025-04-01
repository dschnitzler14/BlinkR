background_module_ui <- function(id, i18n){
  ns <- NS(id)
  background_tab <- tabItem(tabName = "background",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          div(
            class = "page-title-box",
            tags$h2(
              tagList(shiny::icon("book-open"), i18n$t("Background"))
            )
  )
)

      ),
      fluidRow(
        column(12,
        box(
            title = tagList(shiny::icon("database"), i18n$t("Using Research Databases")),
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            uiOutput(ns("background_databases_markdown"))
          ),

          box(
            title = tagList(shiny::icon("search"), i18n$t("Search Strategies")),
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            uiOutput(ns("background_strategies_markdown"))
            #includeMarkdown("markdown/02_background/background_strategies.Rmd")
          ),
            box(
            title = tagList(shiny::icon("lightbulb"), i18n$t("Additional Tips for Effective Searches")),
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            uiOutput(ns("background_tips_markdown"))
            #includeMarkdown("markdown/02_background/background_tips.Rmd")
          ),
          box(
            title = tagList(shiny::icon("file-lines"), i18n$t("Advice for Reading a Paper")),
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            uiOutput(ns("background_reading_paper_markdown"))
            #includeMarkdown("markdown/02_background/background_reading_paper.Rmd")
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
        ns("back_page_background"),
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_background"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)

    )
  )
}

background_module_server <- function(id, i18n, parent.session, include_markdown_language){
  moduleServer(
    id,
    function(input, output, server){

    vars <- get_experiment_vars()

    output$background_databases_markdown <- renderUI({
    include_markdown_language("02_background/background_databases.Rmd")
  })

   output$background_strategies_markdown <- renderUI({
    include_markdown_language("02_background/background_strategies.Rmd")
  })

   output$background_tips_markdown <- renderUI({
    include_markdown_language("02_background/background_tips.Rmd")
  })

  output$background_reading_paper_markdown <- renderUI({
    include_markdown_language("02_background/background_reading_paper.Rmd")
  })

      
       observeEvent(input$back_page_background, {
      updateTabItems(parent.session, "sidebar", "Introduction")
    })
      observeEvent(input$next_page_background, {
      updateTabItems(parent.session, "sidebar", "Hypothesis")
    })
      
    }
  )
}