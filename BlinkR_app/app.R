library(shiny)
library(bslib)
library(shinydashboard)
library(markdown)
library(DT)
library(shinyAce)
library(shinyauthr)
library(dplyr)
library(ggplot2)
library(car)
library(tidyr)
library(utils)
library(googlesheets4)
library(googledrive)
library(readr)
library(markdownInput)
library(knitr)
library(shinycssloaders)
library(future)
library(promises)
library(shinyWidgets)
library(tibble)
library(stringr)
library(shinyjs)
library(jsonlite)
library(datasets)
library(evaluate)
library(rstatix)

options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = "BlinkR_app/.secrets"
)

#only run once:
#gs4_auth(email = "appdemo41@gmail.com", cache = "BlinkR_app/.secrets")
#drive_auth(email = "appdemo41@gmail.com", cache = "BlinkR_app/.secrets")


googlesheets4::gs4_auth()
googledrive::drive_auth()


user_base_google_sheet <- drive_get("BlinkR Users")$id

#user_base_read <- read_sheet(user_base_google_sheet)


base_group_files_url <- paste0("https://drive.google.com/drive/u/0/folders/")

final_reports_folder_id <- drive_get("BlinkR_final_reports")$id

group_data_file_id <- drive_get("BlinkR_Measurements")$id

protocol_file_id <- drive_get("BlinkR_protocols")$id

BlinkR_measurement_sheet <- drive_get("BlinkR_Measurements")$id


#load all modules in modules/ directory ----
module_files <- list.files(path = "modules", pattern = "\\.R$", full.names = TRUE)
sapply(module_files, source)

# variable to point to css ----
css_link <- tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                      tags$script(src = "app.js"),
                      tags$script("hljs.highlightAll();"),
                      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0")
                      )

# header ----
header <- dashboardHeader(title = "BlinkR", uiOutput("user_area"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar",
    menuItem("Introduction", tabName = "Introduction", icon = icon("sun")),

    conditionalPanel(
      condition = "!output.user_auth",
      actionButton("login_button", "Log In", icon = icon("sign-in-alt"), class = "btn-primary", style = "margin: 10px; width: 90%")
    ),
    
    conditionalPanel(
    condition = "output.user_role === 'admin'",
    menuItem("Admin Area", tabName = "admin_area", icon = icon("lock"))  
    ),
    
    conditionalPanel(
      condition = "output.user_auth",
      sidebarMenu(
        menuItem("Background", tabName = "Background", icon = icon("book-open")),
        menuItem("Hypothesis", tabName = "Hypothesis", icon = icon("pen-to-square")),
        menuItem("Protocol", tabName = "Protocol", icon = icon("list")),
        menuItem("Measurements", tabName = "Measurements", icon = icon("ruler")),
        menuItem("Raw Data", tabName = "Raw_Data", icon = icon("database")),
        menuItem("Playground", tabName = "Playground", icon = icon("hand")),
        menuItem("Analysis", tabName = "Analysis", icon = icon("play"),
                 menuItem("Analysis Dashboard", tabName = "Analysis_Dashboard", icon = icon("dashboard")),
                 menuItem("1. Prepare Data", tabName = "Prepare_Data", icon = icon("magnifying-glass")),
                 menuItem("2. Summarise Data", tabName="Summarise_Data", icon = icon("rectangle-list")),
                 menuItem("3. Create Figure", tabName="Create_Figure", icon = icon("chart-simple")),
                 menuItem("4. Statistical Analysis", tabName="Statistical_Analysis", icon = icon("equals"))
        ),
        menuItem("Writing Up", tabName = "Writing-Up-menu",icon = icon("pen"),
          menuItem("Write Notes", tabName = "Writing-Up", icon = icon("pen")),
          menuItem("Upload Final Report", tabName = "Upload_Report", icon = icon("upload"))
          ),
        menuItem("Feedback", tabName = "Feedback", icon = icon("comment"))
      )
    ),
    
    conditionalPanel(
      condition = "output.user_auth",
      actionButton("your_drive_button", "View Your Drive", icon = icon("google-drive"), class = "btn-primary", style = "margin: 10px; width: 90%")
    ),
    
    conditionalPanel(
      condition = "output.user_auth",
      actionButton("logout_button", "Logout", icon = icon("sign-out-alt"), class = "btn-danger", style = "margin: 10px; width: 90%")
    )
  )
  
  
)



# dashboard body combined ----
body <- dashboardBody(
  css_link,
  useShinyjs(),
    
  uiOutput("login_ui"),

  tabItems(
    tabItem(
      tabName = "Introduction",
      introduction_module_ui("introduction") 
    ),
    tabItem(
      tabName = "admin_area",
      conditionalPanel(
        condition = "output.user_role === 'admin'",
        admin_area_module_ui("admin_module")
      )
    ),
    tabItem(
      tabName = "Background",
      conditionalPanel(
        condition = "output.user_auth",
        background_module_ui("background")
      )
    ),
    tabItem(
      tabName = "Hypothesis",
      conditionalPanel(
        condition = "output.user_auth",
        hypothesis_module_ui("hypothesis")
      )
    ),
    tabItem(
      tabName = "Protocol",
      conditionalPanel(
        condition = "output.user_auth",
        protocol_module_ui("protocol")
      )
    ),
    tabItem(
      tabName = "Measurements",
      conditionalPanel(
        condition = "output.user_auth",
        measurements_module_ui("measurements")
      )
    ),
    tabItem(
      tabName = "Raw_Data",
      conditionalPanel(
        condition = "output.user_auth", 
        class_data_module_ui("class_data")
      )
    ),
    tabItem(
      tabName = "Playground",
      conditionalPanel(
        condition = "output.user_auth",
        playground_module_ui("playground")
      )
  ),
    tabItem(
      tabName = "Analysis_Dashboard",
      conditionalPanel(
        condition = "output.user_auth", 
        analysis_dashboard_module_ui("analysis_dashboard")
      )
    ),
    tabItem(
      tabName = "Prepare_Data",
      conditionalPanel(
        condition = "output.user_auth", 
        analysis_prepare_data_module_ui("analysis_prepare_data")
      )
    ),
    tabItem(
      tabName = "Summarise_Data",
      conditionalPanel(
        condition = "output.user_auth", 
        analysis_summarise_data_module_ui("summarise")
      )
    ),
    tabItem(
      tabName = "Statistical_Analysis",
      conditionalPanel(
        condition = "output.user_auth", 
        analysis_stats_module_ui("stats")
      )
    ),
    tabItem(
      tabName = "Create_Figure",
      conditionalPanel(
        condition = "output.user_auth", 
        analysis_create_figure_module_ui("figure")
      )
    ),
    tabItem(
      tabName = "Writing-Up",
      conditionalPanel(
        condition = "output.user_auth",
        write_up_module_ui("write_up")
      )
    ),
     tabItem(
      tabName = "Upload_Report",
      conditionalPanel(
        condition = "output.user_auth",
        upload_report_module_ui("upload_report")
        
      )
    ),
    tabItem(
      tabName = "Feedback",
      conditionalPanel(
        condition = "output.user_auth",
        feedback_module_ui("feedback")
        
      )
    )
  )
)


# ui combined ----
ui <- dashboardPage(header, sidebar, body)

# server function ----
server <- function(input, output, session) {

  
saved_results <- reactiveValues(
  plots = list(),
  recorded_plots = list(),
  scripts = list()
)
  
  reload_trigger <- reactiveValues(reload = 0)

  db_measurement <- reactiveVal(data.frame(Group = character(), ID = integer(), Initials = character(), Stress_Status = character(), Technical_Replicate = integer(), Blinks_Per_Minute = integer(), Submission_ID = character(), stringsAsFactors = FALSE))
  
  db_student_table <- reactiveVal(data.frame(Group = character(), ID = integer(), Initials = character(), Remove = character(), Submission_ID = character(), stringsAsFactors = FALSE))
    
  combined_class_data_sheet <- drive_get("BlinkR_Combined_Class_Data")$id
  
  combined_class_data_read <- read_sheet(combined_class_data_sheet)
  
  introduction_module_server("introduction", parent.session = session)

  auth <- custom_login_server("login_module", user_base_google_sheet, base_group_files_url)

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
      custom_login_ui("login_module")
    })
  })
  
  admin_area_module_server("admin_module", group_data_file_id = group_data_file_id, parent.session = session, user_base_google_sheet = user_base_google_sheet, final_reports_folder_id = final_reports_folder_id)
  
  observeEvent(input$admin_area_button, {
    req(auth()$user_info$role == "admin")
    updateTabItems(session, "main_tabs", "admin_area")
  })
  
  observeEvent(input$your_drive_button, {
    req(auth()$user_auth)
    
    showModal(modalDialog(
      title = "Your Google Drive",
      your_google_drive_module_ui("your_drive_module"),
      
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l" 
    ))
  })
  
  
  observeEvent(input$logout_button, {
    auth()$user_auth <- FALSE
    auth()$user_info <- list(Group = NULL, role = NULL)
    
    # output$login_ui <- renderUI({ 
    #   custom_login_ui("login_module")
    # })
  })
  

  
  observe({
    req(auth()$user_auth)
    output$login_ui <- renderUI(NULL)
    session_folder_id = auth()$session_folder_id
    
    background_module_server("background", parent.session = session)
    hypothesis_module_server("hypothesis", parent.session = session, auth = auth)
    protocol_module_server("protocol", auth = auth, parent.session = session, protocol_file_id = protocol_file_id)
    measurements_module_server("measurements", db_student_table = db_student_table, db_measurement = db_measurement, auth = auth, parent.session = session)
    class_data_module_server("class_data", db_measurement = db_measurement, BlinkR_measurement_sheet = BlinkR_measurement_sheet, parent.session = session, auth = auth)
    playground_module_server("playground", session_folder_id = session_folder_id)
    analysis_dashboard_module_server("analysis_dashboard", parent.session = session, saved_results, session_folder_id = session_folder_id)
    analysis_prepare_data_module_server("analysis_prepare_data", results_data = combined_class_data_read, parent.session = session, session_folder_id = session_folder_id)
    analysis_summarise_data_module_server("summarise", results_data = combined_class_data_read, parent.session = session, saved_results = saved_results, session_folder_id = session_folder_id)
    analysis_stats_module_server("stats", results_data = combined_class_data_read, parent.session = session, saved_results = saved_results, session_folder_id = session_folder_id)
    analysis_create_figure_module_server("figure", results_data = combined_class_data_read, parent.session = session, saved_results = saved_results, session_folder_id = session_folder_id)
    write_up_module_server("write_up", parent.session = session, auth = auth, reload_trigger,  session_folder_id = session_folder_id)
    upload_report_module_server("upload_report", auth = auth, base_group_files_url = base_group_files_url, final_reports_folder_id = final_reports_folder_id)
    feedback_module_server("feedback")
    your_google_drive_module_server("your_drive_module", session_folder_id = session_folder_id)
    
  })
}


# runapp ----
shinyApp(ui = ui, server = server)
