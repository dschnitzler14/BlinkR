background_module_ui <- function(id){
  ns <- NS(id)
  background_tab <- tabItem(tabName = "background",
    fluidPage(
            fluidRow(
        column(
          width = 12,
          div(
            class = "page-title-box",
            tags$h2(
              tagList(shiny::icon("book-open"), "Background")
            )
  )
)

      ),
      fluidRow(
        column(12,
        box(
            title = tagList(shiny::icon("database"), "Using Research Databases"),
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            includeMarkdown("markdown/02_background/background_databases.Rmd")
          ),

          box(
            title = tagList(shiny::icon("search"), "Search Strategies"),
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            includeMarkdown("markdown/02_background/background_strategies.Rmd")
          ),
            box(
            title = tagList(shiny::icon("lightbulb"), "Additional Tips for Effective Searches"),
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            includeMarkdown("markdown/02_background/background_tips.Rmd")
          ),
          box(
            title = tagList(shiny::icon("file-lines"), "Advice for Reading a Paper"),
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            includeMarkdown("markdown/02_background/background_reading_paper.Rmd")
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
        label = tagList(icon("arrow-left"), " Back"),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_background"), 
        label = tagList("Next ", icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)

    )
  )
}

background_module_server <- function(id, parent.session){
  moduleServer(
    id,
    function(input, output, server){

    vars <- get_experiment_vars()

      
       observeEvent(input$back_page_background, {
      updateTabItems(parent.session, "sidebar", "Introduction")
    })
      observeEvent(input$next_page_background, {
      updateTabItems(parent.session, "sidebar", "Hypothesis")
    })
      
    }
  )
}