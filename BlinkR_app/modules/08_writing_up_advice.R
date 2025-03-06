writing_up_advice_ui <- function(id) {
    ns <- NS(id)
  writing_up_advice_tab <- tabItem(tabName = "Writing_Up_Advice",
                              fluidPage(
                                fluidRow(
            column(
              width = 12,
              div(
                class = "page-title-box",
                tags$h2(
                  tagList(shiny::icon("circle-question"), "Writing Up: Advice")
                )
      )
    )),
                                fluidRow(
                                  box(
                                    title = tagList(shiny::icon("file-alt"), "Structure"),
                                    id = "structure",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    class = "clickable-box", 
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure.Rmd")
                                  ),
                                  box(
                                    title = tagList(shiny::icon("lightbulb"), "Introduction"),
                                    id = "structure_introduction",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_introduction.Rmd")
                                  ),
                                  box(
                                    title = tagList(shiny::icon("flask"), "Methods"),
                                    id = "structure_methods",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_methods.Rmd")
                                  ),
                                  box(
                                    title = tagList(shiny::icon("chart-bar"), "Results"),
                                    id = "structure_results",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_results.Rmd")
                                  ),
                                  box(
                                    title = tagList(shiny::icon("comments"), "Discussion"),
                                    id = "structure_discussion",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_discussion.Rmd")
                                  ),
                                  box(
                                    title = tagList(shiny::icon("forward"), "Future Work"),
                                    id = "structure_future",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_future_work.Rmd")
                                  ),
                                    box(
                                    title = tagList(shiny::icon("book"), "References"),
                                    id = "structure_references",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_references.Rmd")
                                    ),
                                    box(
                                    title = tagList(shiny::icon("tools"), "Tips and Tricks"),
                                    id = "structure_tips",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_tips.Rmd")
                                        ),
                                    box(
                                    title = tagList(shiny::icon("users"), "Working Together"),
                                    id = "working_together_tips",
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    includeMarkdown("markdown/08_writing_up/writing_up_advice_structure_together.Rmd")
                                    )
                                )
                              )
  )
    }

writing_up_advice_server <- function(input, output, session) {
    #ns <- session$ns
}

