library(shiny)

source("global.R")

i18n_path <- "translations/translations.json"
i18n <- Translator$new(translation_json_path = i18n_path)
i18n$set_translation_language("en")

### load all modules in modules/ directory ----
module_files <- list.files(path = "modules", pattern = "\\.R$", full.names = TRUE)
sapply(module_files, source)

### header ----
header <- dashboardHeader(
  title = "BlinkR",
    tags$li(
  class = "dropdown",
  style = "padding: 10px 15px;",
  tags$select(
    id = "selected_language",
    class = "form-control",
    tags$option(value = "en", "🇬🇧 English"),
    tags$option(value = "de", "🇩🇪 Deutsch")
  )
),
    tags$li(
      class = "dropdown",
      actionLink("about_link", label = i18n$t("About"), icon = icon("info-circle"))
    ),
    tags$li(
      class = "dropdown",
      actionLink("citing", label = i18n$t("Cite BlinkR"), icon = icon("asterisk"))
    )
  )

sidebar <- dashboardSidebar(
  

  sidebarMenu(
    usei18n(i18n),
    id = "sidebar",


  conditionalPanel(
    condition = "output.user_auth",
    div(
    style = "padding: 10px; font-weight: bold; background-color: white; color: black; border-radius: 5px; text-align: center;",
    textOutput("sidebar_group_name")
    )
),

    menuItem(i18n$t("Introduction"), tabName = "Introduction", icon = icon("sun")),

    conditionalPanel(
      condition = "!output.user_auth",
      actionButton("login_button", i18n$t("Log In"), icon = icon("sign-in-alt"), class = "btn-primary", style = "margin: 10px; width: 90%")
    ),
    
    conditionalPanel(
    condition = "output.user_role === 'admin'",
    div(
      style = "padding: 10px; font-weight: bold; background-color: #ff9800; color: white !important; border-radius: 5px; text-align: left;",
    tags$li(
        menuItem(i18n$t("Admin Area"), tabName = "admin_area", icon = icon("lock"))  
    )
    )
),
    
    conditionalPanel(
      condition = "output.user_auth",
      sidebarMenu(
        usei18n(i18n),
        
        menuItem(i18n$t("Background"), tabName = "Background", icon = icon("book-open")),
        menuItem(i18n$t("Hypothesis"), tabName = "Hypothesis", icon = icon("pen-to-square")),
        menuItem(i18n$t("Protocol"), tabName = "Protocol", icon = icon("list")),
        menuItem(i18n$t("Measurements"), tabName = "Measurements", icon = icon("ruler")),
        menuItem(i18n$t("Raw Data"), tabName = "Raw_Data", icon = icon("database")),
        menuItem(i18n$t("Playground"), tabName = "Playground", icon = icon("hand")),
        menuItem(i18n$t("Analysis"), tabName = "Analysis", icon = icon("play"),
                 menuItem(i18n$t("Analysis Dashboard"), tabName = "Analysis_Dashboard", icon = icon("dashboard")),
                 menuItem(i18n$t("1. Prepare Data"), tabName = "Prepare_Data", icon = icon("magnifying-glass")),
                 menuItem(i18n$t("2. Summarise Data"), tabName="Summarise_Data", icon = icon("rectangle-list")),
                 menuItem(i18n$t("3. Create Figure"), tabName="Create_Figure", icon = icon("chart-simple")),
                 menuItem(i18n$t("4. Statistical Analysis"), tabName="Statistical_Analysis", icon = icon("equals"))
        ),
        menuItem(i18n$t("Writing Up"), tabName = "Writing-Up-menu", icon = icon("pen"),
            menuItem(i18n$t("Write Up Advice"), tabName = "Writing_Up_Advice", icon = icon("circle-question")),
            menuItem(i18n$t("AI"), tabName = "AI", icon = icon("wand-magic-sparkles")),
            menuItem(i18n$t("Write Up"), tabName = "Writing-Up", icon = icon("pen")),
            menuItem(i18n$t("Upload Final Report"), tabName = "Upload_Report", icon = icon("upload"))
          ),
        menuItem(i18n$t("Simulated Experiment"), tabName = "Simulated_Experiment", icon = icon("microscope"),
            menuItem(i18n$t("Description"), tabName = "Simulated_Experiment_Description", icon = icon("circle-info")),
            menuItem(i18n$t("Background"), tabName = "Simulated_Experiment_Background", icon = icon("book-open")),
            menuItem(i18n$t("Hypothesis"), tabName = "Simulated_Experiment_Hypothesis", icon = icon("pen-to-square")),
            menuItem(i18n$t("Protocol"), tabName = "Simulated_Experiment_Protocol", icon = icon("list")),
            menuItem(i18n$t("Measurements"), tabName = "Simulated_Experiment_Measurements", icon = icon("ruler")),
            menuItem(i18n$t("Raw Data"), tabName = "Simulated_Experiment_Raw_Data", icon = icon("database")),
            menuItem(i18n$t("Analysis"), tabName = "Simulated_Experiment_Analysis", icon = icon("chart-simple")),
            menuItem(i18n$t("Writing Up"), tabName = "Simulated_Experiment_Writing_Up", icon = icon("pen"))
          ),
        menuItem(i18n$t("Feedback"), tabName = "Feedback", icon = icon("comment"))

      )
    ),
    
    conditionalPanel(
      condition = "output.user_auth",
      actionButton("your_drive_button", i18n$t("View Your Drive"), icon = icon("google-drive"), class = "btn-primary", style = "margin: 10px; width: 90%")
    ),
    
    conditionalPanel(
      condition = "output.user_auth",
      actionButton("logout_button", i18n$t("Logout"), icon = icon("sign-out-alt"), class = "btn-danger", style = "margin: 10px; width: 90%")
    )
  )
  
  
)



# dashboard body combined ----
body <- dashboardBody(
  usei18n(i18n),
  css_link,
  useShinyjs(),

  uiOutput("login_ui"),

  tabItems(
    tabItem(
      tabName = "Introduction",
      introduction_module_ui("introduction", i18n) 
    ),
    tabItem(
      tabName = "admin_area",
      conditionalPanel(
        condition = "output.user_role === 'admin'",
        admin_area_module_ui("admin_module", i18n)
      )
    ),
    tabItem(
      tabName = "Background",
      conditionalPanel(
        condition = "output.user_auth",
        background_module_ui("background", i18n)
      )
    ),
    tabItem(
      tabName = "Hypothesis",
      conditionalPanel(
        condition = "output.user_auth",
        hypothesis_module_ui("hypothesis", i18n)
      )
    ),
    tabItem(
      tabName = "Protocol",
      conditionalPanel(
        condition = "output.user_auth",
        protocol_module_ui("protocol", i18n)
      )
    ),
    tabItem(
      tabName = "Measurements",
      conditionalPanel(
        condition = "output.user_auth",
        measurements_module_ui("measurements", i18n)
      )
    ),
    tabItem(
      tabName = "Raw_Data",
      conditionalPanel(
        condition = "output.user_auth", 
        class_data_module_ui("class_data", i18n)
      )
    ),
    tabItem(
      tabName = "Playground",
      conditionalPanel(
        condition = "output.user_auth",
        playground_module_ui("playground", i18n)
      )
  ),
    tabItem(
      tabName = "Analysis_Dashboard",
      conditionalPanel(
        condition = "output.user_auth", 
        analysis_dashboard_module_ui("analysis_dashboard", i18n)
      )
    ),
    tabItem(
      tabName = "Prepare_Data",
      conditionalPanel(
        condition = "output.user_auth", 
        analysis_prepare_data_module_ui("analysis_prepare_data", i18n)
      )
    ),
    tabItem(
      tabName = "Summarise_Data",
      conditionalPanel(
        condition = "output.user_auth", 
        analysis_summarise_data_module_ui("summarise", i18n)
      )
    ),
    tabItem(
      tabName = "Statistical_Analysis",
      conditionalPanel(
        condition = "output.user_auth", 
        analysis_stats_module_ui("stats", i18n)
      )
    ),
    tabItem(
      tabName = "Create_Figure",
      conditionalPanel(
        condition = "output.user_auth", 
        analysis_create_figure_module_ui("figure", i18n)
      )
    ),
    tabItem(
      tabName = "Writing-Up",
      conditionalPanel(
        condition = "output.user_auth",
        write_up_module_ui("write_up", i18n)
      )
    ),
    tabItem(
      tabName = "Writing_Up_Advice",
      conditionalPanel(
        condition = "output.user_auth",
        writing_up_advice_ui("writing_up_advice", i18n)
      )
    ),
    tabItem(
      tabName = "AI",
      conditionalPanel(
        condition = "output.user_auth",
        writing_up_ai_ui("AI", i18n)
      )
    ),
     tabItem(
      tabName = "Upload_Report",
      conditionalPanel(
        condition = "output.user_auth",
        upload_report_module_ui("upload_report", i18n)
        
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Description",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_description_module_ui("simulated_experiment_description", i18n)
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Background",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_background_module_ui("simulated_experiment_background", i18n)
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Hypothesis",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_hypothesis_module_ui("simulated_experiment_hypothesis", i18n)
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Protocol",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_protocol_module_ui("simulated_experiment_protocol", i18n)
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Measurements",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_measurements_module_ui("simulated_experiment_measurements", i18n)
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Raw_Data",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_raw_data_module_ui("simulated_experiment_raw_data", i18n)
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Analysis",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_analysis_module_ui("simulated_experiment_analysis", i18n)
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Writing_Up",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_writing_up_module_ui("simulated_experiment_writing_up", i18n)
      )
    ),
    tabItem(
      tabName = "Feedback",
      conditionalPanel(
        condition = "output.user_auth",
        feedback_module_ui("feedback", i18n)
        
      )
    )
  )
)


# ui combined ----
ui <- dashboardPage(header, sidebar, body)


#server function ----
server <- function(input, output, session) {

  markdown_path <- reactiveVal("markdown/english/")

  observeEvent(input$selected_language, {
    if (input$selected_language == "en") {
      markdown_path("markdown/english/")
    } else if (input$selected_language == "de") {
      markdown_path("markdown/german/")
    }
    
    update_lang(input$selected_language, session)
    updateActionLink(session, "about_link", label = i18n$t("About"))
    updateActionLink(session, "citing", label = i18n$t("Cite BlinkR"))
  })

  include_markdown_language <- function(filepath) {
    cat("markdown path: ", markdown_path(), "\n")
    cat("file path: ", filepath, "\n")

    includeMarkdown(file.path(markdown_path(), filepath))
  }



auth_status <- reactiveVal(FALSE)

  observe({
    req(auth())
    auth_status(auth()$user_auth)
  })

  output$login_ui <- renderUI({
    req(isolate(!auth_status()))
    custom_login_ui("login_module", i18n)
})

outputOptions(output, "login_ui", suspendWhenHidden = FALSE)

all_users <- reactiveVal()

observe({
  req(user_base_google_sheet)
  user_data <- googlesheets4::read_sheet(user_base_google_sheet)
  all_users(user_data)
})

### combined data
combined_class_data_read_reactive <- reactiveVal()

observe({
  req(combined_class_data_sheet)
  combined_class_data_sheet_observe <- googlesheets4::read_sheet(combined_class_data_sheet)
  combined_class_data_read_reactive(combined_class_data_sheet_observe)
})
####

saved_results <- reactiveValues(
  plots = list(),
  recorded_plots = list(),
  scripts = list(),
  user_writing = list()
)
  
  reload_trigger <- reactiveValues(reload = 0)

  db_measurement <- reactiveVal(db_measurement_dataframe)
  
  db_student_table <- reactiveVal(db_student_table_dataframe)
  
  feedback_data <- reactiveVal(feedback_data_dataframe)
  
  introduction_module_server("introduction", i18n, parent.session = session, auth_status)

  auth <- custom_login_server("login_module", i18n, user_base_google_sheet, all_users, base_group_files_url, external_logout_button = reactive(input$logout_button))

  output$sidebar_group_name <- renderText({
    req(auth()$user_auth)
    paste("Group ID:", auth()$user_info$group)
  })

  outputOptions(output, "sidebar_group_name", suspendWhenHidden = FALSE) 

  output$group_id <- reactive({ auth()$user_info$group })
  output$user_auth <- reactive({ auth()$user_auth })
  output$user_role <- reactive({ auth()$user_info$role })
  output$data_permission <- reactive({ auth()$user_info$data })
  output$protocol_permission <- reactive({ auth()$user_info$protocol })
  
  outputOptions(output, "user_role", suspendWhenHidden = FALSE)
  outputOptions(output, "user_auth", suspendWhenHidden = FALSE)
  
  output$session_folder_url <- reactive({ auth()$session_folder_url })

  observeEvent(input$login_button, {
    output$login_ui <- renderUI({
      req(!auth()$user_auth)
      custom_login_ui("login_module", i18n)
    })
  })
  
  
  observeEvent(input$admin_area_button, {
    req(auth()$user_info$role == "admin")
    updateTabItems(session, "main_tabs", "admin_area")
  })
  
  observeEvent(input$your_drive_button, {
    req(auth()$user_auth)
    
    showModal(modalDialog(
      title = "Your Google Drive",
      your_google_drive_module_ui("your_drive_module", i18n),
      
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l" 
    ))
  })
  
  
  observe({
    req(auth()$user_auth)
    output$login_ui <- renderUI(NULL)
    session_folder_id = auth()$session_folder_id
    
    admin_area_module_server("admin_module", group_data_file_id = group_data_file_id, parent.session = session, user_base = all_users, final_reports_folder_id = final_reports_folder_id, user_base_google_sheet, session_folder_id = session_folder_id)
    background_module_server("background", i18n, parent.session = session, include_markdown_language = include_markdown_language)
    hypothesis_module_server("hypothesis", i18n, parent.session = session, auth = auth, include_markdown_language = include_markdown_language)
    protocol_module_server("protocol", i18n, auth = auth, parent.session = session, protocol_file_id = protocol_file_id, session_folder_id = session_folder_id)
    measurements_module_server("measurements", db_student_table = db_student_table, db_measurement = db_measurement, auth = auth, parent.session = session)
    class_data_module_server("class_data", db_measurement = db_measurement, BlinkR_measurement_sheet = BlinkR_measurement_sheet, parent.session = session, auth = auth)
    playground_module_server("playground", session_folder_id = session_folder_id, parent.session = session)
    analysis_dashboard_module_server("analysis_dashboard", parent.session = session, saved_results, session_folder_id = session_folder_id)
    analysis_prepare_data_module_server("analysis_prepare_data", results_data = combined_class_data_read_reactive, parent.session = session, session_folder_id = session_folder_id)
    analysis_summarise_data_module_server("summarise", results_data = combined_class_data_read_reactive, parent.session = session, saved_results = saved_results, session_folder_id = session_folder_id)
    analysis_stats_module_server("stats", results_data = combined_class_data_read_reactive, parent.session = session, saved_results = saved_results, session_folder_id = session_folder_id)
    analysis_create_figure_module_server("figure", results_data = combined_class_data_read_reactive, parent.session = session, saved_results = saved_results, session_folder_id = session_folder_id)
    writing_up_advice_server("writing_up_advice", parent.session = session)
    writing_up_ai_server("AI", parent.session = session)
    write_up_module_server("write_up", parent.session = session, auth = auth, reload_trigger,  session_folder_id = session_folder_id)
    upload_report_module_server("upload_report", auth = auth, base_group_files_url = base_group_files_url, final_reports_folder_id = final_reports_folder_id, parent.session = session)
    simulated_experiment_description_module_server("simulated_experiment_description", parent.session = session)
    simulated_experiment_background_module_server("simulated_experiment_background", parent.session = session)
    simulated_experiment_hypothesis_module_server("simulated_experiment_hypothesis", parent.session = session)
    simulated_experiment_protocol_module_server("simulated_experiment_protocol", parent.session = session)
    simulated_experiment_measurements_module_server("simulated_experiment_measurements", parent.session = session)
    simulated_experiment_raw_data_module_server("simulated_experiment_raw_data", caf_data_read = caf_data_read, parent.session = session)
    simulated_experiment_analysis_module_server("simulated_experiment_analysis", caf_data_read = caf_data_read, parent.session = session)
    simulated_experiment_writing_up_module_server("simulated_experiment_writing_up", parent.session = session)
    feedback_module_server("feedback", feedback_data, parent.session = session)
    your_google_drive_module_server("your_drive_module", session_folder_id = session_folder_id)
    
  })

  observeEvent(input$about_link, {
    showModal(
      modalDialog(
        title = "About BlinkR",
        includeMarkdown("markdown/00_about/about_box.Rmd"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  observeEvent(input$citing, {
    showModal(
      modalDialog(
        title = "Citing BlinkR",
        includeMarkdown("markdown/00_about/citing.Rmd"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

}

# runapp ----
shinyApp(ui = ui, server = server)

