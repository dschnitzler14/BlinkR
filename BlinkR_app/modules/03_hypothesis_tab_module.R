# Hypothesis Module ----
hypothesis_module_ui <- function(id) {
  ns <- NS(id)
  hypothesis_tab <- tabItem(
    tabName = "Hypothesis",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          div(
            class = "page-title-box",
            tags$h2(
              tagList(shiny::icon("pen-to-square"), "Hypothesis")
            )
  )
)

      ),
      fluidRow(
        box(
          title = tagList(shiny::icon("question-circle"), "What is a hypothesis?"),
          id = "hypothesis_box1",
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          solidHeader = TRUE,
          includeMarkdown("markdown/03_hypothesis/what_is_a_hypothesis.Rmd")
        ),
        box(
          title = tagList(shiny::icon("pen"), "Tips for Writing Your Hypothesis"),
          id = "hypothesis_box2",
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          solidHeader = TRUE,
          includeMarkdown("markdown/03_hypothesis/hypothesis_tips.Rmd")
        ),
        box(
          title = tagList(shiny::icon("lightbulb"), "What is your hypothesis?"),
          id = "hypothesis_box3",
          collapsible = FALSE,
          width = 12,
          solidHeader = TRUE,
          markdown("#### What is your hypothesis in plain language?"),
          text_area_module_UI(ns("hypothesis"), text_label = "Hypothesis", text_value = "", button_label = tagList(shiny::icon("save"), "Save Notes")),
          markdown("#### What is your null hypothesis?"),
          text_area_module_UI(ns("null_hypothesis"), text_label = "Null Hypothesis", text_value = "", button_label = tagList(shiny::icon("save"), "Save Notes")),
          markdown("#### What is your alterative hypothesis?"),
          text_area_module_UI(ns("alt_hypothesis"), text_label = "Alternative Hypothesis", text_value = "", button_label = tagList(shiny::icon("save"), "Save Notes"))
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
        ns("back_page_hypo"),
        label = tagList(icon("arrow-left"), " Back"),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_hypo"), 
        label = tagList("Next ", icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
    )
  )
}

# Hypothesis Module Server
hypothesis_module_server <- function(id, parent.session, auth) {
  moduleServer(
    id,
    function(input, output, session) {
            vars <- get_experiment_vars()

      
      text_area_module_server("hypothesis", auth, "Hypothesis")
      text_area_module_server("null_hypothesis", auth, "Null Hypothesis")
      text_area_module_server("alt_hypothesis", auth, "Alternative Hypothesis")


      
       observeEvent(input$back_page_hypo, {
      updateTabItems(parent.session, "sidebar", "Background")
    })
      observeEvent(input$next_page_hypo, {
      updateTabItems(parent.session, "sidebar", "Protocol")
    })
    }
  )
}
