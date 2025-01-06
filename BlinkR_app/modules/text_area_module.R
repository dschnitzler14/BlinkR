text_area_module_UI <- function(id, text_label = "Type Your Notes Here", text_value = "", button_label = "Submit") {
  ns <- NS(id)
  tagList(
    textAreaInput(
      inputId = ns("text_input"),
      label = text_label,
      value = text_value
    ),
    actionButton(ns("inputTextButton"), label = button_label, class = "fun-submit-button"
),
    uiOutput(ns("submission_feedback"))
  )
}

text_area_module_server <- function(id, auth, filename = "Filename", reload_trigger) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$inputTextButton, {
        text_to_save <- input$text_input
        
        temp_file <- tempfile(fileext = ".txt")
        writeLines(text_to_save, temp_file)
        
        Group <- auth()$user_info$Group
        
        pathname <- paste0("BlinkR_text_results/", Group)
        
        name <- paste0(
          filename, "_", 
          format(Sys.time(), "%d%m%y_%H-%M"), 
          ".txt"
        )
        
        drive_upload(
          media = temp_file,
          path = pathname,
          name = name
        )
        
        output$submission_feedback <- renderUI({
          div(class = "success-box", "\U1F64C Your text has been submitted.")
        })
        
        # Increment reload trigger to refresh existing data
        if (!is.null(reload_trigger)) {
          reload_trigger$reload <- isolate(reload_trigger$reload + 1)
        }
      })
    }
  )
}

# 
# text_area_module_server <- function(id, auth, filename = "Filename", reload_trigger) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       observeEvent(input$inputTextButton, {
#         
#         text_to_save <- input$text_input
#         
#         temp_file <- tempfile(fileext = ".txt")
#         writeLines(text_to_save, temp_file)
#         
#         Group = auth()$user_info$Group
#         
#         pathname <- paste0("BlinkR_text_results/", Group)
#         
#         name <- paste0(
#           filename, "_", 
#           format(Sys.time(), "%d%m%y_%H-%M"), 
#           ".txt"
#         )
#         
#         drive_upload(
#           media = temp_file,
#           path = pathname,
#           name = name
#         )
#         
#         output$submission_feedback <- renderUI({
#           div(class = "success-box", "\U1F64C Your text has been submitted.")
#         })
#         
#          if (!is.null(reload_trigger)) {
#           reload_trigger$reload <- isolate(reload_trigger$reload + 1)
#         }
#         
#       })
#     }
#   )
# }