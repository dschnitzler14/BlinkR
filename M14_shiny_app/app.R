library(shiny)
library(bslib)
library(shinydashboard)
library(markdown)
library(DT)
library(googlesheets4)
library(shinyAce)

#load all modules in modules/ directory ----
module_files <- list.files(path = "modules", pattern = "\\.R$", full.names = TRUE)
sapply(module_files, source)

# load google sheets ----
SHEET_ID_UNSTRESSED <- gs4_get("https://docs.google.com/spreadsheets/d/13lZ_Icc3wFOdeYW-lSKnYC7R3s9YAenTmaO_iSx-ciE/edit?usp=sharing")
SHEET_ID_STRESSED <- gs4_get("https://docs.google.com/spreadsheets/d/1wMLrxKj6JViqH4s8td1MURsY8-VuEVxuDcBE2Z_fAio/edit?usp=sharing")

gs4_auth(cache=".secrets", email="dschnitzler1@gmail.com")

# variable to point to css ----
css_link <- tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))

# header ----
header <- dashboardHeader(title = "Experiment")

# sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "Introduction", icon = icon("sun")),
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

# dashboard body combined ----
body <- dashboardBody(
            css_link, 
            tabItems(introduction_module_ui("introduction"),
                     background_module_ui("background"),
                     hypothesis_module_ui("hypothesis"),
                     protocol_module_ui("protocol"),
                     measurements_module_ui("measurements"),
                     class_data_module_ui("class_data"),
                     analysis_module_ui("analysis"),
                     write_up_module_ui("write_up")
              )
            )

# ui combined ----
ui <- dashboardPage(header, sidebar,body)

# server function ----
server <- function(input, output, session) {
  introduction_module_server("introduction")
  background_module_server("background")
  hypothesis_module_server("hypothesis")
  protocol_module_server("protocol")
  measurements_module_server("measurements")
  class_data_module_server("class_data")
  analysis_module_server("analysis")
  write_up_module_server("write_up")
}

# runapp ----
shinyApp(ui = ui, server = server)
