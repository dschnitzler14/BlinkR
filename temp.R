bucketListModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Display the selected items
    output$results_2 <- renderPrint(input$rank_list_2)
    
    # Handle submit button
    observeEvent(input$submit_hazards, {
      if (length(input$rank_list_2) == 0) {
        # Nothing selected
        showNotification("Error: please select data hazards", type = "error")
      } else {
        # At least one hazard is selected
        showNotification("Hazards submitted!", type = "message")
        
        # Create a single-page PDF
        out_file <- file.path(tempdir(), "selected_hazards.pdf")
        pdf(out_file, width = 8, height = 11)  # Standard letter size
        
        # Read and store all selected images
        image_list <- list()
        for (hazard in input$rank_list_2) {
          img_path <- file.path("www", "hazards", paste0(hazard, ".png"))
          image_list[[hazard]] <- rasterGrob(readPNG(img_path))
        }
        
        # Arrange images in a grid (3 images per row)
        num_images <- length(image_list)
        num_cols <- 3  # Define the number of images per row
        num_rows <- ceiling(num_images / num_cols) # Compute required rows
        
        # Generate the layout
        grid.arrange(grobs = image_list, ncol = num_cols, nrow = num_rows)
        
        dev.off()  # Close PDF file
        
        # Show a modal with the PDF path (for local testing)
        showModal(modalDialog(
          title = "PDF Created",
          paste("A PDF containing the selected hazards has been generated at:", out_file),
          easyClose = TRUE
        ))
      }
    })
    
  })
}