writing_up_ai_ui <- function(id, i18n) {
    ns <- NS(id)
    simulated_experiment_measurements <- tabItem(tabName = "AI",
                              fluidPage(
                                fluidRow(
            column(
              width = 12,
              div(
                class = "page-title-box",
                tags$h2(
                  tagList(shiny::icon("wand-magic-sparkles"), i18n$t("Writing Up: AI"))
                )
      )
    )),
                                fluidRow(
                                    box(
                                    title = tagList(shiny::icon("robot"), i18n$t("Brief summary of how AI tools like ChatGPT work")),
                                    id = "what_is_ai",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("writing_up_ai_summary_markdown"))
                                  ),
                                  box(
                                    title = tagList(shiny::icon("exclamation-triangle"), i18n$t("AI Pitfalls")),
                                    id = "pitfalls",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("writing_up_ai_pitfalls_markdown"))                                  
                                  ),
                                ),
                                  fluidRow(
                                    column(6,
                                    box(
                                    title = i18n$t("✅ Proofreading"),
                                    id = "proofreading",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown(i18n$t("AI can help catch grammar, spelling, and punctuation mistakes, but always clarify in your prompt that you *do not want new information added*."))
                                    ),
                                    box(
                                    title = i18n$t("✅ Sentence Rewording and Clarity Improvements"),
                                    id = "formatting",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown(i18n$t("AI can suggest clearer ways to phrase sentences, but be cautious to ensure your original meaning remains intact."))
                                    ),
                                    box(
                                    title = i18n$t("✅ Brainstorming"),
                                    id = "brainstorming",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown(i18n$t("AI can help with topic ideas, essay outlines, and brainstorming arguments, but the final content should always be your own."))
                                    )
                                    ),
                                    column(6,
                                    box(
                                    title = i18n$t("❌ Factual Information"),
                                    id = "factual",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown(i18n$t("AI does not have access to real-time or verified sources and can generate incorrect, outdated, or misleading facts, which can significantly weaken academic work."))
                                    ),
                                    box(
                                    title = i18n$t("❌ Analysis"),
                                    id = "analysis",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown(i18n$t("AI cannot analyse data; relying on it can lead to data falsification, which is academic misconduct."))
                                    ),
                                    box(
                                    title = i18n$t("❌ Data Visualisation"),
                                    id = "analysis",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown(i18n$t("AI cannot generate figures from a given dataset; It lacks access to the data and will produce entirely fabricated visualizsations, which is also considered data fabrication."))
                                    ),
                                    box(
                                    title = i18n$t("❌ References and Citations"),
                                    id = "references",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown(i18n$t("AI frequently generates fake sources or incorrectly cites real ones, leading to plagiarism, which is a serious academic offense."))
                                    ),
                                    box(
                                    title = i18n$t("❌ Interpretation of Data"),
                                    id = "interpretation",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown(i18n$t("AI often misinterprets studies or evidence, leading to incorrect conclusions and poor science, which can result in a low grade."))
                                    ),
                                    box(
                                    title = i18n$t("❌ Writing the Report"),
                                    id = "interpretation",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown(i18n$t("AI-generated essays lack original thought, critical reasoning, and often contain factual errors, leading to academic dishonesty and potential plagiarism."))
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
        ns("back_page_ai"),
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_ai"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
                              )
    )
    }


writing_up_ai_server <- function(id, i18n, parent.session, include_markdown_language) {
  moduleServer(
    id,
    function(input, output, session){

output$writing_up_ai_summary_markdown <- renderUI({
  include_markdown_language("08_writing_up/writing_up_ai_summary.Rmd")
})

output$writing_up_ai_pitfalls_markdown <- renderUI({
  include_markdown_language("08_writing_up/writing_up_ai_pitfalls.Rmd")
})


    observeEvent(input$back_page_ai, {
        updateTabItems(parent.session, "sidebar", "Writing_Up_Advice")
      })
      observeEvent(input$next_page_ai, {
        updateTabItems(parent.session, "sidebar", "Writing-Up")
      })
}
  )
  }