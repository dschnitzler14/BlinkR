library(shiny)
library(bslib)
library(shinydashboard)
library(markdown)
library(DT)
library(shinyAce)
library(shinyauthr)
library(dplyr)

# User base for login credentials
user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

#load all modules in modules/ directory ----
module_files <- list.files(path = "modules", pattern = "\\.R$", full.names = TRUE)
sapply(module_files, source)

# variable to point to css ----
css_link <- tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))

# header ----
# Header with Logout Button ----
header <- dashboardHeader(title = "Experiment", uiOutput("user_area"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "Introduction", icon = icon("sun")),
    
    conditionalPanel(
      condition = "output.user_auth",
      menuItem("User Area", tabName = "User_Area", icon = icon("user"))
    ),
    
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
        menuItem("Analysis", tabName = "Analysis", icon = icon("magnifying-glass")),
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
      tabName = "User_Area",
      conditionalPanel(
        condition = "output.user_auth",
        group_info_module_ui("student_initials")
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
      tabName = "Analysis",
      conditionalPanel(
        condition = "output.user_auth", 
        analysis_module_ui("analysis")
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
ui <- dashboardPage(header, sidebar,body)

# server function ----
server <- function(input, output, session) {
  
  db <- reactiveVal(data.frame(ID = integer(), Initials = character(), Unstressed_1 = integer(), Unstressed_2 = integer(), Unstressed_3 = integer(), Stressed_1 = integer(), Stressed_2 = integer(), Stressed_3 = integer(), stringsAsFactors = FALSE))
  
  introduction_module_server("introduction")
  
  auth <- custom_login_server("login_module", user_base = user_base)
  
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
    group_info_module_server("student_initials", db = db)
    hypothesis_module_server("hypothesis")
    protocol_module_server("protocol")
    measurements_module_server("measurements", db = db)
    class_data_module_server("class_data")
    analysis_module_server("analysis")
    write_up_module_server("write_up")
  })
}


# runapp ----
shinyApp(ui = ui, server = server)
