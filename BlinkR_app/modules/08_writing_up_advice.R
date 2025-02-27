writing_up_advice_ui <- function(id) {
    ns <- NS(id)
  writing_up_advice_tab <- tabItem(tabName = "Writing_Up_Advice",
                              fluidPage(
                                fluidRow(
                                  box(
                                    title = "Structure",
                                    id = "structure",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure.Rmd")
                                  ),
                                  box(
                                    title = "Introduction",
                                    id = "structure_introduction",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_introduction.Rmd")
                                  ),
                                  box(
                                    title = "Methods",
                                    id = "structure_methods",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_methods.Rmd")
                                  ),
                                  box(
                                    title = "Results",
                                    id = "structure_results",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_results.Rmd")
                                  ),
                                  box(
                                    title = "Discussion",
                                    id = "structure_discussion",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_discussion.Rmd")
                                  ),
                                  box(
                                    title = "Future Work",
                                    id = "structure_future",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_future_work.Rmd")
                                  ),
                                    box(
                                        title = "References",
                                        id = "structure_references",
                                        collapsible = TRUE,
                                        width = 12,
                                        solidHeader = TRUE,
                                        includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_references.Rmd")
                                    ),
                                    box(
                                            title = "Tips and Tricks",
                                            id = "structure_tips",
                                            collapsible = TRUE,
                                            width = 12,
                                            solidHeader = TRUE,
                                            includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_tips.Rmd")
                                        )
                                )
                              )
  )
    }

writing_up_advice_server <- function(input, output, session) {
    ns <- session$ns
}

