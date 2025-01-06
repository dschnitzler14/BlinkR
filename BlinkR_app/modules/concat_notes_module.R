concat_notes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("concat_notes"), "Concatenate All Notes", class = "fun-concat-button"),
    uiOutput(ns("notes_display_ui")) # Replace the download UI with a display UI
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
                   
                   # Return the concatenated content instead of saving it to a file
                   return(paste(all_contents, collapse = "\n"))
                 }
                 
                 concatenated_content <- reactiveVal(NULL)
                 
                 observeEvent(input$concat_notes, {
                   req(auth()$user_info$Group)
                   
                   group_folder_name <- auth()$user_info$Group
                   content <- concatenate_txt_to_md(group_folder_name)
                   concatenated_content(content)
                   
                   # Render the concatenated content as HTML or Markdown
                   output$notes_display_ui <- renderUI({
                     req(concatenated_content())
                     tagList(
                       h3("Concatenated Notes"),
                       pre(concatenated_content()) # Display notes as preformatted text
                     )
                   })
                 })
               })
}
