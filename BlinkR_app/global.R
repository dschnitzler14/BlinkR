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
library(coin)
library(rsconnect)
library(cookies)
library(whisker)
library(sortable)
library(grid)
library(png)
library(gridExtra)
library(shiny.i18n)
library(htmltools)
library(stats)
library(lubridate)
library(shinyvalidate)
library(digest)
library(mailtoR)

source("STEP1_google_drive_app_set_up.R")
source("STEP2_define_variables.R")
source("data_hazards_list.R")

site_url <- "https://ds1405.shinyapps.io/blinkr_app/"

# fetch experimental variables
experiment_vars <- experiment_variables

get_experiment_vars <- function() {
  return(experiment_vars)
}

### load and read files from google drive ----
## user base data
user_base_google_sheet <- drive_get("BlinkR Users")$id
#user_base_read <- read_sheet(user_base_google_sheet)

## base url for group files
base_group_files_url <- paste0("https://drive.google.com/drive/u/0/folders/")
group_data_file_id <- drive_get("BlinkR_Measurements")$id

## user-generated data
final_reports_folder_id <- drive_get("BlinkR_final_reports")$id
BlinkR_measurement_sheet <- drive_get("BlinkR_Measurements")$id

## admin operated files
protocol_file_id <- drive_get("BlinkR_protocols")$id
combined_class_data_sheet <- drive_get("BlinkR_Combined_Class_Data")$id
#combined_class_data_read <- read_sheet(combined_class_data_sheet)

## simulated experiment data
caf_data_path <- "data/Caf_Dummy_Data.csv"
caf_data_read_csv <- read.csv(caf_data_path, header = TRUE)
caf_data_read <- as.data.frame(caf_data_read_csv)


# dataframes for reactive values

  db_measurement_dataframe <- data.frame(group = character(), id = integer(), initials = character(), technical_replicate = integer(), submission_id = character(), stringsAsFactors = FALSE)
  db_measurement_dataframe[[experiment_variables$levels_variable_name]] <- character()
  db_measurement_dataframe[[experiment_variables$measurement_variable_name]] <- integer()


  db_student_table_dataframe <- data.frame(group = character(), id = integer(), initials = character(), remove = character(), submission_id = character(), stringsAsFactors = FALSE)
  
  feedback_data_dataframe <- data.frame(
    timestamp = character(),
    overall_experience = numeric(),
    clarity = numeric(),
    clarity_issues = character(),
    bugs = character(),
    bug_details = character(),
    experiment_tools = character(),
    missing_features = character(),
    useful_features = character(),
    least_useful_features = character(),
    general_feedback = character(),
    stringsAsFactors = FALSE
  )

# variable to point to css and js ----
css_link <- tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                      tags$script(src = "app.js"),
                      tags$script("hljs.highlightAll();"),
                      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0")
                      )

