download_handler_ui <- function(id, i18n, label = "Download File") {
  ns <- NS(id)
  tagList(
    downloadButton(ns("download_file"), label, class = "fun-download-button")
  )
}

download_handler_server <- function(id, content_reactive, filename_generator, type) {
  moduleServer(id, function(input, output, session) {
    if (!type %in% c("text", "plot", "file")) {
      stop("Invalid `type` argument. Must be one of: 'text', 'plot', 'file'.")
    }
    
    output$download_file <- downloadHandler(
      filename = function() {
        filename_generator()
      },
      content = function(file) {
        req(content_reactive())
        if (type == "plot") {
          ggsave(
            file,
            plot = content_reactive() + theme(
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA)
            ),
            width = 8,
            height = 6,
            dpi = 300
          )
        } else if (type == "text") {
          writeLines(content_reactive(), file)
        } else if (type == "file") {
          file_path <- content_reactive()
          req(file.exists(file_path))
          file.copy(file_path, file)
        }
      }
    )
  })
}


