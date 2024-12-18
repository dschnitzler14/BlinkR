existing_data_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("These are your previous notes:"),
    textOutput(ns("existing_data"))
)
}


existing_data_module_server <- function(id, auth, section) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$existing_data <- renderText({
        req(auth())
        req(section)
        
        Group <- auth()$user_info$user
        
        googledrive::drive_auth()
        
        parent_folder_name <- "BlinkR_text_results"
        parent_folder <- googledrive::drive_get(parent_folder_name)
        if (nrow(parent_folder) == 0) {
          return("Parent folder not found.")
        }
        
        group_folder <- googledrive::drive_ls(
          path = googledrive::as_id(parent_folder$id),
          pattern = Group
        )
        if (nrow(group_folder) == 0) {
          return("Group folder not found.")
        }
        
        txt_files <- googledrive::drive_ls(
          path = googledrive::as_id(group_folder$id),
          pattern = "\\.txt$"
        )
        if (nrow(txt_files) == 0) {
          return("No .txt files found in the group folder.")
        }
        
        matching_files <- txt_files %>%
          dplyr::filter(stringr::str_starts(name, section))
        
        if (nrow(matching_files) == 0) {
          return(paste0("No files found starting with section: ", section))
        }
        
        temp_dir <- tempdir()
        all_content <- character()
        
        for (i in seq_len(nrow(matching_files))) {
          file_name <- matching_files$name[i]
          
          date_part <- stringr::str_extract(file_name, "\\d{6}_\\d{2}-\\d{2}")
          formatted_date <- stringr::str_replace_all(date_part, c("_" = " ", "-" = ":"))
          formatted_date <- format(as.POSIXct(formatted_date, format = "%d%m%y %H:%M"), "%d.%m.%y %H:%M")
          
          local_file <- file.path(temp_dir, file_name)
          googledrive::drive_download(
            file = googledrive::as_id(matching_files$id[i]),
            path = local_file,
            overwrite = TRUE
          )
          
          file_content <- readLines(local_file, warn = FALSE)
          
          all_content <- c(all_content, paste0("### Date: ", formatted_date, "\n"), file_content, "\n")
        }
        
        paste(all_content, collapse = "\n")
      })
    }
  )
}



