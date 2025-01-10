feedback_module_ui <- function(id) {
  ns <- NS(id)
  feedback_tab <- tabItem(tabName = "feedback",
                            fluidPage(
                                textAreaInput(
                                  ns("feedback"),
                                  label = "Feedback",
                                  placeholder = "Share your feedback here!"
                                ),
                                actionButton(ns("submit_feedback"), "Send Feedback"),
                                uiOutput(ns("submission_feedback"))
                            )
  )
}

feedback_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns 
  
    observeEvent(input$submit_feedback, {
      
      text_to_save <- input$feedback
      
      temp_file <- tempfile(fileext = ".txt")
      writeLines(text_to_save, temp_file)
      
      pathname <- paste0("Feedback/")
     
      name <- paste0("Feedback_", 
                     format(Sys.time(), "%d%m%y_%H-%M"),
                     ".txt")
      
      drive_upload(
        media = temp_file,
        path = pathname,
        name = name
      )
      
      output$submission_feedback <- renderUI({
        div(class = "success-box", "\U1F64C Thank you, your feedback has been sent!")
      })
      
    })
    
  })
}
