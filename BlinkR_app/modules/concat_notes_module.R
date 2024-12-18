concat_notes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("concat_notes"), "Concat Notes", class = "fun-download-button"),
    uiOutput(ns("download_ui"))
  )
}

concat_notes_server <- function(id, auth) {
  moduleServer(id, 
               function(input, output, session) {
                 
                 ns <- session$ns
                 
                 concatenate_txt_to_md <- function(group_folder_name) {
                   googledrive::drive_auth()
                   
                   parent_folder_name <- "BlinkR_text_results"
                   parent_folder <- googledrive::drive_get(parent_folder_name)
                   if (nrow(parent_folder) == 0) {
                     stop("Parent folder not found in Google Drive.")
                   }
                   
                   group_folder <- googledrive::drive_ls(
                     path = googledrive::as_id(parent_folder$id),
                     pattern = group_folder_name
                   )
                   if (nrow(group_folder) == 0) {
                     stop("Group folder not found in Google Drive.")
                   }
                   
                   txt_files <- googledrive::drive_ls(
                     path = googledrive::as_id(group_folder$id),
                     pattern = "\\.txt$"
                   )
                   if (nrow(txt_files) == 0) {
                     stop("No .txt files found in the group folder.")
                   }
                   
                   temp_dir <- tempdir()
                   all_contents <- character()
                   
                   for (file in txt_files$name) {
                     local_file <- file.path(temp_dir, file)
                     googledrive::drive_download(
                       file = googledrive::as_id(txt_files$id[txt_files$name == file]),
                       path = local_file,
                       overwrite = TRUE
                     )
                     
                     file_contents <- readLines(local_file)
                     all_contents <- c(all_contents, paste0("# File: ", file), file_contents, "\n")
                   }
                   
                   output_file <- paste0(
                     group_folder_name, "_",
                     format(Sys.time(), "%d%m%y"),
                     "_All_Notes.md"
                   )
                   output_path <- file.path(temp_dir, output_file)
                   
                   writeLines(all_contents, output_path)
                   return(output_path)
                 }
                 
                 concatenated_file <- reactiveVal(NULL)
                 
                 observeEvent(input$concat_notes, {
                   req(auth()$user_info$user)
                   
                   group_folder_name <- auth()$user_info$user
                   file_path <- concatenate_txt_to_md(group_folder_name)
                   concatenated_file(file_path)
                   
                   output$download_ui <- renderUI({
                     req(concatenated_file())
                     downloadButton(ns("download_file"), "Download Notes")
                   })
                 })
                 
                 output$download_file <- downloadHandler(
                   filename = function() {
                     req(concatenated_file())
                     basename(concatenated_file())
                   },
                   content = function(file) {
                     req(concatenated_file())
                     file.copy(concatenated_file(), file)
                   }
                 )
               })
}

