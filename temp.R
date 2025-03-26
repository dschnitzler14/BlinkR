parent.session
###

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
        ns("back_page_summarise"),
        label = tagList(icon("arrow-left"), " Back"),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_summarise"), 
        label = tagList("Next ", icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)

###
observeEvent(input$back_page_summarise, {
        updateTabItems(parent.session, "sidebar", "Prepare_Data")
      })
      observeEvent(input$next_page_summarise, {
        updateTabItems(parent.session, "sidebar", "Create_Figure")
      })

###
menuItem("Simulated Experiment", tabName = "Simulated_Experiment", icon = icon("microscope"),
        menuItem("Description", tabName = "Simulated_Experiment_Description", icon = icon("circle-info")),
        menuItem("Background", tabName = "Simulated_Experiment_Background", icon = icon("book-open")),
        menuItem("Hypothesis", tabName = "Simulated_Experiment_Hypothesis", icon = icon("pen-to-square")),
        menuItem("Protocol", tabName = "Simulated_Experiment_Protocol", icon = icon("list")),
        menuItem("Measurements", tabName = "Simulated_Experiment_Measurements", icon = icon("ruler")),
        menuItem("Raw Data", tabName = "Simulated_Experiment_Raw_Data", icon = icon("database")),
        menuItem("Analysis", tabName = "Simulated_Experiment_Analysis", icon = icon("chart-simple")),
        menuItem("Writing Up", tabName = "Simulated_Experiment_Writing_Up", icon = icon("pen"))
        ),