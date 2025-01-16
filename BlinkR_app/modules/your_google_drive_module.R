your_google_drive_module_ui <- function(id){
  ns <- NS(id)
    fluidPage(
      fluidRow(
        markdown("# View Your Google Drive Folder
                 As you add content throughout the app, it will get uploaded to this Google Drive.
                 From there, you can download your data, results, and figures. "),
        uiOutput(ns("iframe_to_your_drive")),
      )
    )
}

your_google_drive_module_server <- function(id, session_folder_id){
  moduleServer(
    id,
    function(input, output, server){
      
      path <- paste0("https://drive.google.com/drive/folders/", session_folder_id)
      
      output$iframe_to_your_drive <- renderUI({
        tagList(
          tags$iframe(
            src = path,
            width = "100%",
            height = "800px",
            style = "border:none;"
          )
        )
      })
      
    }
  )
}