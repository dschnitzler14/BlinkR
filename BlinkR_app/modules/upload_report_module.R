upload_report_module_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "Upload Report",
    fluidPage(
      fluidRow(
            column(
              width = 12,
              div(
                class = "page-title-box",
                tags$h2(
                  tagList(shiny::icon("upload"), "Upload Report")
                )
      )
    )),
      fluidRow(
        box(
          title = tagList(shiny::icon("upload"), "Upload Your Reports Here"),
          id = ns("upload_report_box1"),
          collapsible = TRUE,
          width = 12,
          solidHeader = TRUE,
          textInput(ns("your_name"), "Your First and Last Name"),
          fileInput(
            ns("file_upload"),
            label = "Choose a file to upload"
          ),
          actionButton(
            ns("upload_button"),
            tagList(shiny::icon("upload"), "Upload Report to Drive"),
            class = "btn btn-primary"
          ),
          br(),
          textOutput(ns("upload_status")
          ),
          br(),
          h4("Files uploaded in this session:"),
          uiOutput(ns("uploaded_files_list"))
        )
      )
    )
  )
}

upload_report_module_server <- function(id, auth, base_group_files_url, final_reports_folder_id) {
  moduleServer(
    id,
    function(input, output, session) {
      uploaded_files <- reactiveVal(list())
      
      observeEvent(input$upload_button, {
        req(auth())
        req(input$file_upload)
        
        Group <- auth()$user_info$Group
        user_name <- input$your_name
        uploaded_file <- input$file_upload
        
        if (is.null(user_name) || user_name == "") {
          output$upload_status <- renderText("Please enter your name before uploading.")
          return()
        }
        
        pathname <- paste0(base_group_files_url, final_reports_folder_id)
        drive_folder <- googledrive::drive_get(pathname)
        
        if (nrow(drive_folder) == 0) {
          output$upload_status <- renderText("Error: Group folder not found on Google Drive.")
          return()
        }
        
        file_name <- paste0("Final Report - ", user_name, " - Group ", Group, ".", tools::file_ext(uploaded_file$name))
        
        tryCatch({
          drive_upload(
            media = uploaded_file$datapath,
            path = pathname,
            name = file_name,
            overwrite = TRUE
          )
          
          uploaded_files(c(uploaded_files(), file_name))
          
          updateTextInput(session, "your_name", value = "")
          session$sendCustomMessage("resetFileInput", list(id = session$ns("file_upload")))
          
          output$upload_status <- renderText("File uploaded successfully!")
        }, error = function(e) {
          output$upload_status <- renderText(paste("Error uploading file:", e$message))
        })
      })
      
      output$uploaded_files_list <- renderUI({
        file_list <- uploaded_files()
        if (length(file_list) > 0) {
          tags$ul(lapply(file_list, tags$li))
        } else {
          "No files uploaded yet."
        }
      })
    }
  )
}

