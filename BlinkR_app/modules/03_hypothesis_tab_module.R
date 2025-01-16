# Hypothesis Module ----
hypothesis_module_ui <- function(id) {
  ns <- NS(id)
  hypothesis_tab <- tabItem(
    tabName = "Hypothesis",
    fluidPage(
      fluidRow(
        box(
          title = "What is a hypothesis?",
          id = "hypothesis_box1",
          collapsible = FALSE,
          width = 12,
          solidHeader = TRUE,
          includeMarkdown("markdown/03_hypothesis/what_is_a_hypothesis.Rmd")
        ),
        box(
          title = "Tips for Writing Your Hypothesis",
          id = "hypothesis_box2",
          collapsible = FALSE,
          width = 12,
          solidHeader = TRUE,
          includeMarkdown("markdown/03_hypothesis/hypothesis_tips.Rmd")
        ),
        box(
          title = "What is your hypothesis?",
          id = "hypothesis_box3",
          collapsible = FALSE,
          width = 12,
          solidHeader = TRUE,
          text_area_module_UI(ns("hypothesis"), text_label = "Hypothesis", text_value = "Given the background of x, y, and z, my hypothesis is ...")
        )
      ),
      fluidRow(
      column(
      width = 12,
      div(
        style = "display: flex; justify-content: center; margin: 0; padding: 10px;",
        actionButton(ns("back_page_hypo"),
                     label = tagList(icon("arrow-left"), "Back")),
        actionButton(ns("next_page_hypo"), 
              label = tagList("Next", icon("arrow-right")))
          )
        ),
      ),
    )
  )
}

# Hypothesis Module Server
hypothesis_module_server <- function(id, parent.session, auth) {
  moduleServer(
    id,
    function(input, output, session) {
      
      text_area_module_server("hypothesis", auth, "Hypothesis")
      
       observeEvent(input$back_page_hypo, {
      updateTabItems(parent.session, "sidebar", "Background")
    })
      observeEvent(input$next_page_hypo, {
      updateTabItems(parent.session, "sidebar", "Protocol")
    })
    }
  )
}
