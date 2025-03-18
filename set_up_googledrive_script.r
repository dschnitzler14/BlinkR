library(googlesheets4)
library(googledrive)

#enter your gmail address here
email_address = "appdemo41@gmail.com"

options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = "BlinkR_app/.secrets"
)

gs4_auth(email = email_address, cache = "BlinkR_app/.secrets")
drive_auth(email = email_address, cache = "BlinkR_app/.secrets")


check_and_create_folder <- function(folder_name) {
  folder <- drive_find(pattern = folder_name, type = "folder")
  if (nrow(folder) == 0) {
    drive_mkdir(name = folder_name)
    drive_share_anyone(folder_name)
    message(paste("Created folder:", folder_name))
  } else {
    message(paste("Folder already exists:", folder_name))
  }
}

folders <- c("Feedback_folder", "BlinkR_text_results", "BlinkR_final_reports")
lapply(folders, check_and_create_folder)

check_and_create_file <- function(file_name, headers = NULL, initial_entry = NULL) {
  file <- drive_find(pattern = file_name, type = "spreadsheet")
  if (nrow(file) == 0) {
    if (!is.null(headers)) {
      data <- as.data.frame(matrix(ncol = length(headers), nrow = 0))
      colnames(data) <- headers
      if (!is.null(initial_entry)) {
        data <- rbind(data, initial_entry)
      }
    } else {
      data <- data.frame()
    }
    gs4_create(file_name, sheets = list("Sheet1" = data))
    drive_share_anyone(file_name)

    message(paste("Created file:", file_name))
  } else {
    message(paste("File already exists:", file_name))
  }
}

files <- list(
  "BlinkR_protocols" = NULL,
  "BlinkR_Measurements" = NULL,
  "BlinkR_Combined_Class_Data" = c("Group", "Initials", "ID", "Stress_Status", 
                                   "Technical_Replicate", "Blinks_Per_Minute", "Submission_ID"), 
  "BlinkR_Class_Protocol" = NULL,
  "BlinkR Users" = c("Group", "Role", "Name", "Date", "Protocol", "Data"),
  "Feedback" = c("Timestamp", "Overall_Experience", "Clarity", "Clarity_Issues", "Bugs", "Bug_Details", "Experiment_Tools", 
                  "Missing_Features", "Useful_Features", "Least_Useful_Features", "General_Feedback")
)

initial_entry_users <- data.frame(
  Group = "5767",
  Role = "admin",
  Name = "Admin",
  Date = as.character(Sys.Date()),
  Protocol = TRUE,
  Data = TRUE,
  stringsAsFactors = FALSE
)

lapply(names(files), function(file) {
  if (file == "BlinkR Users") {
    check_and_create_file(file, headers = files[[file]], initial_entry = initial_entry_users)
  } else {
    check_and_create_file(file, headers = files[[file]])
  }
})

read_user_base <- function(file_name) {
  sheet <- drive_find(pattern = file_name, type = "spreadsheet")
  if (nrow(sheet) > 0) {
    user_base <- read_sheet(sheet$id)
    user_base <- user_base %>%
      mutate(
        Group = as.character(Group),
        Role = as.character(Role),
        Name = as.character(Name),
        Date = as.Date(Date),
        Protocol = as.logical(Protocol),
        Data = as.logical(Data)
      )
    return(user_base)
  } else {
    stop("File not found.")
  }
}

read_combined_class_data <- function(file_name) {
  sheet <- drive_find(pattern = file_name, type = "spreadsheet")
  if (nrow(sheet) > 0) {
    combined_class_data <- read_sheet(sheet$id)
    combined_class_data <- combined_class_data %>%
      mutate(
        Group = as.character(Group),
        Initials = as.character(Initials),
        ID = as.character(ID),
        Stress_Status = as.character(Stress_Status),
        Technical_Replicate = as.character(Technical_Replicate),
        Blinks_Per_Minute = as.numeric(Blinks_Per_Minute),
        Submission_ID = as.character(Submission_ID)
      )
    return(combined_class_data)
  } else {
    stop("File not found.")
  }
}

message("Script execution completed.")


name <- NULL

print(paste0("Hello ", name))