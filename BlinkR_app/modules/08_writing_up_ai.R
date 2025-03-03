writing_up_ai_ui <- function(id) {
    ns <- NS(id)
    simulated_experiment_measurements <- tabItem(tabName = "AI",
                              fluidPage(
                                fluidRow(
                                    box(
                                    title = "Brief summary of how AI tools like ChatGPT work",
                                    id = "what_is_ai",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    
                                  ),
                                  box(
                                    title = "AI Pitfalls",
                                    id = "pitfalls",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    
                                  )
                                  ),
                                  fluidRow(
                                    column(6,
                                    box(
                                    title = "✅ Proofreading",
                                    id = "proofreading",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    ),
                                    box(
                                    title = "✅ Formatting for Code-based text-editors",
                                    id = "formatting",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    )
                                    ),
                                    column(6,
                                    box(
                                    title = "❌ Factual Information",
                                    id = "factual",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    ),
                                    box(
                                    title = "❌ Analysis",
                                    id = "analysis",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = FALSE,
                                    )
                                    )
                                  
                                  )
                              )
    )
    }


writing_up_ai_server <- function(input, output, session) {
    #ns <- session$ns
}