observe({
  measurement_data <- db_measurement()
  req(nrow(measurement_data) > 0)

  # 1. Read existing from the day’s sheet
  if (sheet_name %in% existing_sheets) {
    existing_data <- read_sheet(BlinkR_measurement_sheet, sheet = sheet_name)
  } else {
    # If no sheet yet, create empty data frame or skip
    existing_data <- data.frame()
    sheet_add(ss = BlinkR_measurement_sheet, sheet = sheet_name)
  }

  # 2. Remove old row(s) for same submission_id from existing
  updated_data <- existing_data %>%
    dplyr::filter(Submission_ID %notin% measurement_data$Submission_ID)
  
  # 3. Add the new data
  updated_data <- dplyr::bind_rows(updated_data, measurement_data)

  # 4. Write the result
  sheet_write(data = updated_data, ss = BlinkR_measurement_sheet, sheet = sheet_name)
})