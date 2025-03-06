# note

tagList(shiny::icon("user"), "Your Protocol"),

tagList(shiny::icon("users"), "Class Protocol"),



menuItem("Analysis Dashboard", tabName = "Analysis_Dashboard", icon = icon("dashboard")),
                 menuItem("1. Prepare Data", tabName = "Prepare_Data", icon = icon("magnifying-glass")),
                 menuItem("2. Summarise Data", tabName="Summarise_Data", icon = icon("rectangle-list")),
                 menuItem("3. Create Figure", tabName="Create_Figure", icon = icon("chart-simple")),
                 menuItem("4. Statistical Analysis", tabName="Statistical_Analysis", icon = icon("equals"))
        ),



.fun-submit-button {
  background-color: #60f542; 
  color: #ffffff;           /* White text */
  font-weight: bold;        /* Bold text */
  font-size: 16px;          /* Slightly larger text */
  border: none;             /* No border */
  border-radius: 25px;      /* Fully rounded edges for a pill shape */
  padding: 10px 20px;       /* Padding for size */
  cursor: pointer;          /* Pointer cursor on hover */
  transition: all 0.3s ease; /* Smooth animation */
  text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.3); /* Fun text shadow */
}

.fun-submit-button:hover {
  background-color: #60f542; 
  transform: scale(1.1);     /* Slight zoom-in effect */
  box-shadow: 0 4px 10px rgba(0, 0, 0, 0.2); /* Soft hover shadow */
}

.fun-submit-button:active {
  background-color: #60f542; 
  transform: scale(1.0);     
}



.fun-generate-button {
  background-color: #faea3e; /* Hot pink background */
  color: #000000;           /* White text */
  font-weight: bold;        /* Bold text */
  font-size: 16px;          /* Slightly larger text */
  border: none;             /* No border */
  border-radius: 25px;      /* Fully rounded edges for a pill shape */
  padding: 10px 20px;       /* Padding for size */
  cursor: pointer;          /* Pointer cursor on hover */
  transition: all 0.3s ease; /* Smooth animation */
  text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.3); /* Fun text shadow */
}

.fun-generate-button:hover {
  background-color: #faea3e; /* Darker pink on hover */
  transform: scale(1.1);     /* Slight zoom-in effect */
  box-shadow: 0 4px 10px rgba(0, 0, 0, 0.2); /* Soft hover shadow */
}

.fun-generate-button:active {
  background-color: #faea3e; /* Even darker pink on click */
  transform: scale(1.0);     /* Return to normal size */
}



observeEvent(input$run_hist_Plot, {
  output$not_normal_unpaired_ui <- renderUI({NULL})
  output$not_normal_paired_ui <- renderUI({NULL})
  output$normal_unpaired_ui <- renderUI({NULL})
  output$normal_paired_ui <- renderUI({NULL})

  output$effect_size_t_test_paired <- renderUI({NULL})
  output$effect_size_t_test_unpaired <- renderUI({NULL})
  output$effect_size_wilcoxon_paired <- renderUI({NULL})
  output$effect_size_wilcoxon_unpaired <- renderUI({NULL})
  output$interpretation_quiz <- renderUI({NULL})
  output$enter_effect_size_feedback <- renderUI({NULL})
  output$enter_p_value_feedback <- renderUI({NULL})


if(!is.null(normal_unpaired_result()$result)){
    normal_unpaired_result <- NULL
  } else if (!is.null(normal_paired_result()$result)) {
     normal_paired_result <- NULL
  } else if (!is.null(not_normal_unpaired_result()$result)) {
     not_normal_unpaired_result <- NULL
  } else if (!is.null(not_normal_paired_result()$result)) {
     not_normal_paired_result <- NULL
  }

      req(average_trs())
      
      hist_plot_reactive <- reactive({
  req(average_trs())  # Make sure data is available
  hist(
    average_trs()$Average_Blinks_Per_Minute,
    main  = "Distribution of Blinks/Minute",
    xlab  = "Average Blinks/Minute",
    ylab  = "Frequency",
    col   = "grey49",
    border = "black"
  )
  # Store recorded plot
  saved_results$recorded_plots[["hist_plot"]] <- recordPlot()
  # Return the recorded plot
  saved_results$recorded_plots[["hist_plot"]]
})

output$hist_plot <- renderPlot({
  replayPlot(hist_plot_reactive())
})

observeEvent(hist_plot_reactive(), ignoreNULL = TRUE, {
  req(hist_plot_reactive())  # Wait for it to be created

  temp_file <- tempfile(fileext = ".png")
  png(temp_file, width = 800, height = 600)
  replayPlot(hist_plot_reactive())  # Replay the recorded plot
  dev.off()

  path <- drive_get(as_id(session_folder_id))
  drive_upload(
    media = temp_file,
    path = path,
    name = "hist_plot.png",
    overwrite = TRUE
  )

  unlink(temp_file)
  showNotification("Plot saved successfully.", type = "message", duration = 3)
})


    output$hist_explainer_ui <- renderUI({
      tagList(
      
      actionButton(session$ns("normal"), "The Data is Normal", class = "fun-save-button"),
        actionButton(session$ns("not_normal"), "The Data is Not Normal", class = "fun-save-button")
      )
      })

  })




fluidRow(
  column(
    width = 12,
    div(
      style = "
        display: flex; 
        justify-content: center; 
        align-items: center; 
        gap: 10px;          
        margin: 0; 
        padding: 10px;
      ",
      actionButton(
        ns("back_page_background"),
        label = tagList(icon("arrow-left"), " Back"),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_background"), 
        label = tagList("Next ", icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)