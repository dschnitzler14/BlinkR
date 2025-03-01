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



tabItem(
      tabName = "Simulated_Experiment_Description",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_description_module_ui("simulated_experiment_description")
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Background",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_background_module_ui("simulated_experiment_background")
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Hypothesis",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_hypothesis_module_ui("simulated_experiment_hypothesis")
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Protocol",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_protocol_module_ui("simulated_experiment_protocol")
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Measurements",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_measurements_module_ui("simulated_experiment_measurements")
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Raw_Data",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_raw_data_module_ui("simulated_experiment_raw_data")
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Analysis",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_analysis_module_ui("simulated_experiment_analysis")
      )
    ),
    tabItem(
      tabName = "Simulated_Experiment_Writing_Up",
      conditionalPanel(
        condition = "output.user_auth",
        simulated_experiment_writing_up_module_ui("simulated_experiment_writing_up")
      )
    )

    simulated_experiment_description_module_server("simulated_experiment_description")
    simulated_experiment_background_module_server("simulated_experiment_background")
    simulated_experiment_hypothesis_module_server("simulated_experiment_hypothesis")
    simulated_experiment_protocol_module_server("simulated_experiment_protocol")
    simulated_experiment_measurements_module_server("simulated_experiment_measurements")
    simulated_experiment_raw_data_module_server("simulated_experiment_raw_data")
    simulated_experiment_analysis_module_server("simulated_experiment_analysis")
    simulated_experiment_writing_up_module_server("simulated_experiment_writing_up")
