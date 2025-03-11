your_google_drive_module_ui <- function(id){
  ns <- NS(id)
   tagList(
        markdown("# View Your Google Drive Folder
                 As you add content throughout the app, it will get uploaded to this Google Drive.
                 From there, you can download your data, results, and figures. "),
        uiOutput(ns("iframe_to_your_drive")),
        div(
          style = "text-align: center; margin-top: 20px;",
        uiOutput(ns("file_picker")),
        tags$br(),
        downloadButton(ns("download_files"), "Download Selected Files", class = "fun-save-button" ),
        )
   )
}

your_google_drive_module_server <- function(id, session_folder_id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      path <- paste0("https://drive.google.com/embeddedfolderview?id=", session_folder_id, "#list")  
      output$iframe_to_your_drive <- renderUI({
        tagList(
          tags$iframe(
            src = path,
            width = "100%",
            height = "400px",
            style = "border:none;"
          )
        )
      })

      files_in_folder <- reactive({
        req(session_folder_id)
        drive_ls(as_id(session_folder_id))
      })

      output$file_picker <- renderUI({
        req(files_in_folder())
        
        file_choices <- setNames(files_in_folder()$id, files_in_folder()$name)
        
        virtualSelectInput(
          inputId = ns("selected_files"),
          label = "Select Files to Download:",
          choices = file_choices,
          showValueAsTags = TRUE,
          search = TRUE,
          multiple = TRUE
        )
      })

      output$download_files <- downloadHandler(
        filename = function() {
          paste0("Selected_Files_", Sys.Date(), ".zip")
        },
        content = function(file) {
          req(input$selected_files)
          
          temp_dir <- tempdir()
          zip_file <- file.path(temp_dir, "selected_files.zip")
          
          file_paths <- c()
          for (file_id in input$selected_files) {
            file_info <- files_in_folder()[files_in_folder()$id == file_id, ]
            local_path <- file.path(temp_dir, file_info$name)
            drive_download(as_id(file_id), path = local_path, overwrite = TRUE)
            file_paths <- c(file_paths, local_path)
          }
          
          zip(zip_file, files = file_paths)
          file.copy(zip_file, file)
        }
      )
    }
  )
}