

sheet_names <- sheet_names(group_data_file_id)

combined_data <- NULL

for (i in 2:length(sheet_names)) {
  sheet_data <- read_sheet(group_data_file_id, sheet = sheet_names[i])
  
  if (i == 2) {
    combined_data <- sheet_data
  } else {
    combined_data <- bind_rows(combined_data, sheet_data[-1, ])
  }
}

new_file <- gs4_create("BlinkR_Combined_Class_Data", sheets = list(combined_class_data = combined_data))
