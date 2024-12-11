download_plot_ui <- function(id, label = "Download Plot") {
  ns <- NS(id)
  tagList(
    downloadButton(ns("download_plot"), label)
  )
}


download_plot_server <- function(id, plot_reactive) {
  moduleServer(id, function(input, output, session) {
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("plot-", Sys.Date(), ".png")
      },
      content = function(file) {
        req(plot_reactive())
        
        ggsave(
          file,
          plot = plot_reactive(),
          width = 8,
          height = 6,
          dpi = 300
        )
      }
    )
  })
}
