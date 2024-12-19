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
library(here)

options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = "BlinkR/BlinkR_app/.newsecrets"
)

#only run once:
# googledrive::drive_auth()
# googlesheets4::gs4_auth()

user_base_google_sheet <- drive_get("BlinkR Users")$id

user_base <- read_sheet(user_base_google_sheet)

# num_groups <- 100
# group_names <- paste0("Group", 1:num_groups)
# 
# user_base <- tibble::tibble(
#   user = group_names,
#   permissions = rep("group", num_groups),
#   name = group_names
# )



BlinkR_measurement_sheet <- drive_get("BlinkR_Measurements")$id

#load all modules in modules/ directory ----
module_files <- list.files(path = "modules", pattern = "\\.R$", full.names = TRUE)
sapply(module_files, source)

# variable to point to css ----
css_link <- tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                      tags$script(src = "app.js"),
                      tags$script("hljs.highlightAll();"))

# header ----
# Header with Logout Button ----
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
      condition = "output.user_auth", 
      sidebarMenu(
        menuItem("Background", tabName = "Background", icon = icon("book-open")),
        menuItem("Hypothesis", tabName = "Hypothesis", icon = icon("pen-to-square")),
        menuItem("Protocol", tabName = "Protocol", icon = icon("list")),
        menuItem("Measurements", tabName = "Measurements", icon = icon("ruler")),
        menuItem("Raw Data", tabName = "Raw_Data", icon = icon("database")),
        menuItem("Analysis", tabName = "Analysis", icon = icon("play"),
                 menuItem("Analysis Dashboard", tabName = "Analysis_Dashboard", icon = icon("dashboard")),
                 menuItem("Prepare Data", tabName = "Prepare_Data", icon = icon("magnifying-glass")),
                 menuItem("Summarise Data", tabName="Summarise_Data", icon = icon("rectangle-list")),
                 menuItem("Statistical Analysis", tabName="Statistical_Analysis", icon = icon("equals")),
                 menuItem("Create Figure", tabName="Create_Figure", icon = icon("chart-simple"))
        ),
        menuItem("Writing Up", tabName = "Writing-Up", icon = icon("pen")),
        menuItem("Feedback", tabName = "Feedback", icon = icon("comment"))
      )
    )
  ),
  
  conditionalPanel(
    condition = "output.user_auth",
    div(
      actionButton("logout_button", "Logout", icon = icon("sign-out-alt"), class = "btn-danger", style = "margin: 10px; width: 90%; position: absolute; bottom: 10px;")
    )
  )
)



# dashboard body combined ----
body <- dashboardBody(
  css_link,
  
  uiOutput("login_ui"),
  
  tabItems(
    tabItem(
      tabName = "Introduction",
      introduction_module_ui("introduction") 
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
    )
  )
)


# ui combined ----
ui <- dashboardPage(header, sidebar, body)

saved_results <- reactiveValues(
  plots = list(),
  recorded_plots = list(),
  scripts = list()
)
# server function ----
server <- function(input, output, session) {
  
  
  db_measurement <- reactiveVal(data.frame(Group = character(), ID = integer(), Initials = character(), Stress_Status = character(), Technical_Replicate = integer(), Blinks_Per_Minute = integer(), Submission_ID = character(), stringsAsFactors = FALSE))
  
  db_student_table <- reactiveVal(data.frame(Group = character(), ID = integer(), Initials = character(), Remove = character(), Submission_ID = character(), stringsAsFactors = FALSE))
  
  data_read <- read.csv(here("BlinkR_app", "data", "dummy_blinking_data.csv"))
  
  introduction_module_server("introduction")
  
  auth <- custom_login_server("login_module", user_base, user_base_google_sheet)
  
  output$user_auth <- reactive({ auth()$user_auth })
  outputOptions(output, "user_auth", suspendWhenHidden = FALSE)
  
  observeEvent(input$login_button, {
    output$login_ui <- renderUI({
      req(!auth()$user_auth)
      custom_login_ui("login_module", title = "Custom Login Page", subtitle = "Welcome! Please log in below.")
    })
  })
  
  observeEvent(input$logout_button, {
    auth()$user_auth <- FALSE
    auth()$user_info <- NULL
    output$login_ui <- renderUI({ 
      custom_login_ui("login_module", title = "Custom Login Page", subtitle = "Welcome! Please log in below.")
    })
  })
  
  observe({
    req(auth()$user_auth)
    output$login_ui <- renderUI(NULL) 
    
    # Load authenticated-only modules
    background_module_server("background")
    hypothesis_module_server("hypothesis")
    protocol_module_server("protocol")
    measurements_module_server("measurements", db_student_table = db_student_table, db_measurement = db_measurement, auth = auth, parent.session = session, BlinkR_measurement_sheet = BlinkR_measurement_sheet)
    class_data_module_server("class_data", db_measurement = db_measurement, BlinkR_measurement_sheet = BlinkR_measurement_sheet)
    analysis_dashboard_module_server("analysis_dashboard", parent.session = session, saved_results)
    analysis_prepare_data_module_server("analysis_prepare_data", results_data = data_read, parent.session = session)
    
    analysis_summarise_data_module_server("summarise", results_data = data_read, parent.session = session, saved_results = saved_results)
    analysis_stats_module_server("stats", results_data = data_read, parent.session = session, saved_results = saved_results)
    analysis_create_figure_module_server("figure", results_data = data_read, parent.session = session, saved_results = saved_results)
    write_up_module_server("write_up", parent.session = session, auth = auth)
  })
}


# runapp ----
shinyApp(ui = ui, server = server)
