introduction_module_ui <- function(id, i18n){
  ns <- NS(id)
  vars <- get_experiment_vars()

  rmd_content_introduction_box1 <- readLines("markdown/01_introduction/introduction_box1.Rmd")
  processed_rmd_introduction_box1 <- whisker.render(paste(rmd_content_introduction_box1, collapse = "\n"), vars)

  introduction_tab <- tabItem(tabName = "Introduction",
                              fluidPage(
                                
                                  fluidRow(
                                      column(
                                        width = 12,
                                        div(
                                          class = "page-title-box",
                                          tags$h2(
                                            tagList(shiny::icon("sun"), "Introduction")
                                          )
                                )
                              )),
                                fluidRow(
                                  box(
                                    title = tagList(icon("right-to-bracket"), "Log in"),
                                    id = "introduction_box1",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    HTML(markdownToHTML(text = processed_rmd_introduction_box1, fragment.only = TRUE)                                  ),
                
                                ),
                        fluidRow(
                            uiOutput(ns("action_buttons_ui"))
                        )
      )
    )
  )
}

introduction_module_server <- function(id, parent.session, auth_status){
  moduleServer(
    id,
    function(input, output, session){
      
      vars <- get_experiment_vars()

      rmd_content <- readLines("markdown/01_introduction/introduction_box2.Rmd")
      processed_rmd <- whisker.render(paste(rmd_content, collapse = "\n"), vars)

      output$action_buttons_ui <- renderUI({
        req(auth_status())
        tagList(
        box(title = "Welcome to BlinkR",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          HTML(markdownToHTML(text = processed_rmd, fragment.only = TRUE))
          ),
        box(
          title = "Research Roadmap",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,

          column(12,
            div(
              class = "roadmap-section",
              tags$h4("Start Here"),
              actionButton(session$ns("background"), 
                label = tagList(icon("book-open"), "Background"),
                class = "action-button primary-action",
                `data-id` = "Background"
              )
            )
          ),
          
          column(12,
            div(
              class = "roadmap-section",
              tags$h4("Research Preparation"),
              actionButton(session$ns("hypothesis"), label = tagList(icon("pen-to-square"), "Hypothesis"), class = "action-button custom-action", `data-id` = "Hypothesis"),
              actionButton(session$ns("protocol"), label = tagList(icon("list"), "Protocol"), class = "action-button custom-action", `data-id` = "Protocol"),
              actionButton(session$ns("measurements"), label = tagList(icon("ruler"), "Measurements"), class = "action-button custom-action", `data-id` = "Measurements"),
              actionButton(session$ns("raw_data"), label = tagList(icon("database"), "Raw Data"), class = "action-button custom-action", `data-id` = "Raw_Data"),

            )
          ),

          column(12,
            div(
              class = "roadmap-section",
              tags$h4("Data Collection & Analysis"),
              actionButton(session$ns("playground"), label = tagList(icon("hand"), "Playground"), class = "action-button custom-action", `data-id` = "Playground"),
              actionButton(session$ns("analysis_dashboard"), label = tagList(icon("dashboard"), "Analysis Dashboard"), class = "action-button custom-action", `data-id` = "Analysis_Dashboard"),
              actionButton(session$ns("prepare_data"), label = tagList(icon("rectangle-list"), "Prepare Data"), class = "action-button custom-action", `data-id` = "Prepare_Data"),
              actionButton(session$ns("summarise_data"), label = tagList(icon("chart-bar"), "Summarise Data"), class = "action-button custom-action", `data-id` = "Summarise_Data"),
              actionButton(session$ns("statistical_analysis"), label = tagList(icon("equals"), "Statistical Analysis"), class = "action-button custom-action", `data-id` = "Statistical_Analysis"),
              actionButton(session$ns("create_figure"), label = tagList(icon("chart-simple"), "Create Figure"), class = "action-button custom-action", `data-id` = "Create_Figure"),

            )
          ),

          column(12,
            div(
              class = "roadmap-section",
              tags$h4("Writing & Presentation"),
              actionButton(session$ns("writing_up"), label = tagList(icon("pen"), "Writing Up"), class = "action-button custom-action", `data-id` = "Writing-Up"),
              actionButton(session$ns("writing_up_advice"), label = tagList(icon("circle-question"), "Writing Up Advice"), class = "action-button custom-action", `data-id` = "Writing_Up_Advice"),
              actionButton(session$ns("ai"), label = tagList(icon("wand-magic-sparkles"), "AI Writing Help"), class = "action-button custom-action", `data-id` = "AI")
            )
          ),

          column(12,
            div(
              class = "roadmap-section",
              tags$h4("Final Steps"),
              actionButton(session$ns("upload_report"), label = tagList(icon("upload"), "Upload Report"), class = "action-button custom-action", `data-id` = "Upload_Report")
            )
          )
        
      )
      )
      })

      observeEvent(input$next_page_intro, {
        updateTabItems(parent.session, "sidebar", "Background")
      })
      
      observeEvent(input$background, { updateTabItems(parent.session, "sidebar", "Background") })
      observeEvent(input$hypothesis, { updateTabItems(parent.session, "sidebar", "Hypothesis") })
      observeEvent(input$protocol, { updateTabItems(parent.session, "sidebar", "Protocol") })
      observeEvent(input$measurements, { updateTabItems(parent.session, "sidebar", "Measurements") })
      observeEvent(input$raw_data, { updateTabItems(parent.session, "sidebar", "Raw_Data") })
      observeEvent(input$playground, { updateTabItems(parent.session, "sidebar", "Playground") })
      observeEvent(input$analysis_dashboard, { updateTabItems(parent.session, "sidebar", "Analysis_Dashboard") })
      observeEvent(input$prepare_data, { updateTabItems(parent.session, "sidebar", "Prepare_Data") })
      observeEvent(input$summarise_data, { updateTabItems(parent.session, "sidebar", "Summarise_Data") })
      observeEvent(input$statistical_analysis, { updateTabItems(parent.session, "sidebar", "Statistical_Analysis") })
      observeEvent(input$create_figure, { updateTabItems(parent.session, "sidebar", "Create_Figure") })
      observeEvent(input$writing_up, { updateTabItems(parent.session, "sidebar", "Writing-Up") })
      observeEvent(input$writing_up_advice, { updateTabItems(parent.session, "sidebar", "Writing_Up_Advice") })
      observeEvent(input$ai, { updateTabItems(parent.session, "sidebar", "AI") })
      observeEvent(input$upload_report, { updateTabItems(parent.session, "sidebar", "Upload_Report") })
  

    }
  )
}