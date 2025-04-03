# Hypothesis Module ----
hypothesis_module_ui <- function(id, i18n) {
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
              tagList(shiny::icon("pen-to-square"), i18n$t("Hypothesis"))
            )
  )
)

      ),
      fluidRow(
        box(
          title = tagList(shiny::icon("question-circle"), i18n$t("What is a hypothesis?")),
          id = "hypothesis_box1",
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          solidHeader = TRUE,
          uiOutput(ns("what_hypothesis_markdown")),
        ),
        box(
          title = tagList(shiny::icon("pen"), i18n$t("Tips for Writing Your Hypothesis")),
          id = "hypothesis_box2",
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          solidHeader = TRUE,
          uiOutput(ns("hypothesis_tips_markdown")),
        ),
        box(
          title = tagList(shiny::icon("lightbulb"), i18n$t("What is your hypothesis?")),
          id = "hypothesis_box3",
          collapsible = FALSE,
          width = 12,
          solidHeader = TRUE,
          h2(i18n$t("What is your hypothesis in plain language?")),
          text_area_module_UI(ns("hypothesis"), i18n, text_label = i18n$t("Hypothesis"), text_value = "", button_label = tagList(shiny::icon("save"), i18n$t("Save Notes"))),
          h2(i18n$t("What is your null hypothesis?")),
          text_area_module_UI(ns("null_hypothesis"), i18n, text_label = i18n$t("Null Hypothesis"), text_value = "", button_label = tagList(shiny::icon("save"), i18n$t("Save Notes"))),
          h2(i18n$t("What is your alterative hypothesis?")),
          text_area_module_UI(ns("alt_hypothesis"), i18n, text_label = i18n$t("Alternative Hypothesis"), text_value = "", button_label = tagList(shiny::icon("save"), i18n$t("Save Notes")))
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
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_hypo"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
    )
  )
}

hypothesis_module_server <- function(id, i18n, parent.session, auth, include_markdown_language) {
  moduleServer(
    id,
    function(input, output, session) {
            vars <- get_experiment_vars()


      output$what_hypothesis_markdown <- renderUI({
        include_markdown_language("03_hypothesis/what_is_a_hypothesis.Rmd")
      })

      output$hypothesis_tips_markdown <- renderUI({
        include_markdown_language("03_hypothesis/hypothesis_tips.Rmd")
      })
      
      text_area_module_server("hypothesis", i18n, auth, i18n$t("Hypothesis"))
      text_area_module_server("null_hypothesis", i18n, auth, i18n$t("Null Hypothesis"))
      text_area_module_server("alt_hypothesis", i18n, auth, i18n$t("Alternative Hypothesis"))


      
       observeEvent(input$back_page_hypo, {
      updateTabItems(parent.session, "sidebar", "Background")
    })
      observeEvent(input$next_page_hypo, {
      updateTabItems(parent.session, "sidebar", "Protocol")
    })
    }
  )
}
