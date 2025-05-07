introduction_module_ui <- function(id, i18n){
  ns <- NS(id)
  vars <- get_experiment_vars()

  introduction_tab <- tabItem(tabName = "Introduction",
                              fluidPage(
                                
                                  fluidRow(
                                      column(
                                        width = 12,
                                        div(
                                          class = "page-title-box",
                                          tags$h2(
                                            tagList(shiny::icon("sun"), i18n$t("Introduction"))
                                          )
                                )
                              )),
                                fluidRow(
                                  box(
                                    title = tagList(icon("right-to-bracket"), i18n$t("Log in")),
                                    id = "introduction_box1",
                                    collapsible = TRUE,
                                    width = 12,
                                    solidHeader = TRUE,
                                    uiOutput(ns("introduction_box1"))
                                ),
                        fluidRow(
                            uiOutput(ns("action_buttons_ui"))
                        )
      )
    )
  )
}

introduction_module_server <- function(id, i18n, parent.session, auth_status, process_markdown){
  moduleServer(
    id,
    function(input, output, session){
      
      vars <- get_experiment_vars()
      
      output$introduction_box1 <- renderUI({
        process_markdown("01_introduction/introduction_box1.Rmd")
      })

      output$introduction_box2 <- renderUI({
        process_markdown("01_introduction/introduction_box2.Rmd")
      })

      output$action_buttons_ui <- renderUI({
        req(auth_status())
        tagList(
        box(title = i18n$t("Welcome to BlinkR"),
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          uiOutput(session$ns("introduction_box2"))
          ),
        box(
          title = i18n$t("Research Roadmap"),
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,

          column(12,
            div(
              class = "roadmap-section",
              tags$h4(i18n$t("Start Here")),
              actionButton(session$ns("background"), 
                label = tagList(icon("book-open"), i18n$t("Background")),
                class = "action-button primary-action",
                `data-id` = "Background"
              )
            )
          ),
          
          column(12,
            div(
              class = "roadmap-section",
              tags$h4(i18n$t("Research Preparation")),
              actionButton(session$ns("hypothesis"), label = tagList(icon("pen-to-square"), i18n$t("Hypothesis")), class = "action-button custom-action", `data-id` = "Hypothesis"),
              actionButton(session$ns("protocol"), label = tagList(icon("list"), i18n$t("Protocol")), class = "action-button custom-action", `data-id` = "Protocol"),
              actionButton(session$ns("measurements"), label = tagList(icon("ruler"), i18n$t("Measurements")), class = "action-button custom-action", `data-id` = "Measurements"),
              actionButton(session$ns("raw_data"), label = tagList(icon("database"), i18n$t("Raw Data")), class = "action-button custom-action", `data-id` = "Raw_Data"),

            )
          ),

          column(12,
            div(
              class = "roadmap-section",
              tags$h4(i18n$t("Data Collection & Analysis")),
              actionButton(session$ns("playground"), label = tagList(icon("hand"), i18n$t("Playground")), class = "action-button custom-action", `data-id` = "Playground"),
              actionButton(session$ns("analysis_dashboard"), label = tagList(icon("dashboard"), i18n$t("Analysis Dashboard")), class = "action-button custom-action", `data-id` = "Analysis_Dashboard"),
              actionButton(session$ns("prepare_data"), label = tagList(icon("rectangle-list"), i18n$t("Prepare Data")), class = "action-button custom-action", `data-id` = "Prepare_Data"),
              actionButton(session$ns("summarise_data"), label = tagList(icon("chart-bar"), i18n$t("Summarise Data")), class = "action-button custom-action", `data-id` = "Summarise_Data"),
              actionButton(session$ns("statistical_analysis"), label = tagList(icon("equals"), i18n$t("Statistical Analysis")), class = "action-button custom-action", `data-id` = "Statistical_Analysis"),
              actionButton(session$ns("create_figure"), label = tagList(icon("chart-simple"), i18n$t("Create Figure")), class = "action-button custom-action", `data-id` = "Create_Figure"),

            )
          ),

          column(12,
            div(
              class = "roadmap-section",
              tags$h4(i18n$t("Writing & Presentation")),
              actionButton(session$ns("writing_up"), label = tagList(icon("pen"), i18n$t("Writing Up")), class = "action-button custom-action", `data-id` = "Writing-Up"),
              actionButton(session$ns("writing_up_advice"), label = tagList(icon("circle-question"), i18n$t("Writing Up Advice")), class = "action-button custom-action", `data-id` = "Writing_Up_Advice"),
              actionButton(session$ns("ai"), label = tagList(icon("wand-magic-sparkles"), i18n$t("AI Writing Help")), class = "action-button custom-action", `data-id` = "AI")
            )
          ),

          column(12,
            div(
              class = "roadmap-section",
              tags$h4(i18n$t("Final Steps")),
              actionButton(session$ns("upload_report"), label = tagList(icon("upload"), i18n$t("Upload Report")), class = "action-button custom-action", `data-id` = "Upload_Report")
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