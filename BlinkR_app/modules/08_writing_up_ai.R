writing_up_ai_ui <- function(id) {
    ns <- NS(id)
    simulated_experiment_measurements <- tabItem(tabName = "AI",
                              fluidPage(
                                fluidRow(
            column(
              width = 12,
              div(
                class = "page-title-box",
                tags$h2(
                  tagList(shiny::icon("wand-magic-sparkles"), "Writing Up: AI")
                )
      )
    )),
                                fluidRow(
                                    box(
                                    title = tagList(shiny::icon("robot"), "Brief summary of how AI tools like ChatGPT work"),
                                    id = "what_is_ai",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_ai_summary.Rmd")
                                    
                                    
                                  ),
                                  box(
                                    title = tagList(shiny::icon("exclamation-triangle"), "AI Pitfalls"),
                                    id = "pitfalls",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_ai_pitfalls.Rmd")
                                  
                                  ),
                                ),
                                  fluidRow(
                                    column(6,
                                    box(
                                    title = "✅ Proofreading",
                                    id = "proofreading",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown("AI can help catch grammar, spelling, and punctuation mistakes, but always clarify in your prompt that you *do not want new information added*.")
                                    ),
                                    box(
                                    title = "✅ Sentence Rewording & Clarity Improvements",
                                    id = "formatting",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown("AI can suggest clearer ways to phrase sentences, but be cautious to ensure your original meaning remains intact.")
                                    ),
                                    box(
                                    title = "✅ Brainstorming",
                                    id = "brainstorming",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown("AI can help with topic ideas, essay outlines, and brainstorming arguments, but the final content should always be your own.")
                                    )
                                    ),
                                    column(6,
                                    box(
                                    title = "❌ Factual Information",
                                    id = "factual",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown("AI does not have access to real-time or verified sources and can generate incorrect, outdated, or misleading facts, which can significantly weaken academic work.")
                                    ),
                                    box(
                                    title = "❌ Analysis",
                                    id = "analysis",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown("AI cannot analyse data; relying on it can lead to data falsification, which is academic misconduct.")
                                    ),
                                    box(
                                    title = "❌ Data Visualisation",
                                    id = "analysis",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown("AI cannot generate figures from a given dataset; It lacks access to the data and will produce entirely fabricated visualizsations, which is also considered data fabrication.")
                                    ),
                                    box(
                                    title = "❌ References & Citations",
                                    id = "references",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown("AI frequently generates fake sources or incorrectly cites real ones, leading to plagiarism, which is a serious academic offense.")
                                    ),
                                    box(
                                    title = "❌ Interpretation of Data",
                                    id = "interpretation",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown("AI often misinterprets studies or evidence, leading to incorrect conclusions and poor science, which can result in a low grade.")
                                    ),
                                    box(
                                    title = "❌ Writing the Report",
                                    id = "interpretation",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    markdown("AI-generated essays lack original thought, critical reasoning, and often contain factual errors, leading to academic dishonesty and potential plagiarism.")
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
        label = tagList(icon("arrow-left"), " Back"),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_ai"), 
        label = tagList("Next ", icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
                              )
    )
    }


writing_up_ai_server <- function(id, parent.session) {
  moduleServer(
    id,
    function(input, output, session){

    observeEvent(input$back_page_ai, {
        updateTabItems(parent.session, "sidebar", "Writing_Up_Advice")
      })
      observeEvent(input$next_page_ai, {
        updateTabItems(parent.session, "sidebar", "Writing-Up")
      })
}
  )
  }