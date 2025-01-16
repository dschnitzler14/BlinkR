analysis_create_figure_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Create_Figure",
    fluidPage(
      fluidRow(
        column(
          12,
          
          box(
            title = "Creating a Figure",
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            solidHeader = TRUE,
            fluidRow(
              column(
                4,
                markdown(
                  "Time to make a figure!"
                ),
                radioButtons(ns("figure_type_selector"),
                             label = "What type of figure would be best here?",
                             choices = c("bar chart" = "bar",
                                         "box plot" = "box"
                                         ),
                             selected = character(0)),
                uiOutput(ns("figure_type_selector_output")),
                uiOutput(ns("figure_editor_feedback"))
              ),
              column(
                8,
                uiOutput(ns("editor_ui")),
                uiOutput(ns("save_plot"))
                
              )
            )
          ),
        ),
        fluidRow(
          column(
            width = 12,
            div(
              style = "display: flex; justify-content: center; align-items: center; gap: 20px; height: 100px;",
              actionButton(
                ns("summarise"),
                label = tagList(icon("rectangle-list"), "Summarise the Data"),
                class = "action-button custom-action",
                `data-id` = "summarise_data"
              ),
              actionButton(
                ns("statistics"),
                label = tagList(icon("equals"), "Run Statistical Analysis"),
                class = "action-button custom-action",
                `data-id` = "stats"
              ),
              actionButton(
                ns("figure"),
                label = tagList(icon("chart-simple"), "Create a Figure"),
                class = "action-button custom-action",
                `data-id` = "create_figure"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              style = "display: flex; justify-content: center; align-items: center; height: 100px;",
              actionButton(
                ns("dashboard"),
                label = tagList(icon("dashboard"), "Go to Dashboard"),
                class = "action-button custom-dark-yellow"
              )
            )
          )

))))

}

analysis_create_figure_module_server <- function(id, results_data, parent.session, saved_results, session_folder_id) {
  moduleServer(id, function(input, output, session) {

    
  average_trs <- reactive({ NULL })
    
  average_trs_results <- results_data %>%
    select(-"Group", -"Initials", -"Submission_ID") %>%
    dplyr::group_by(ID, Stress_Status) %>%
    dplyr::summarise(
      Average_Blinks_Per_Minute = mean(Blinks_Per_Minute, na.rm = TRUE),
      .groups = 'drop'
    )
  
  average_trs <- reactive({ average_trs_results })
  
  
  #Make Figure

  observeEvent(input$figure_type_selector, {
    figure_type_selector_output <- if (input$figure_type_selector == "bar") {
      includeMarkdown(here("BlinkR_app", "markdown", "07_analysis", "analysis_create_figure_barplot.Rmd"))
    } else {
      includeMarkdown(here("BlinkR_app", "markdown", "07_analysis", "analysis_create_figure_boxplot.Rmd"))
    }
    
    output$figure_type_selector_output <- renderUI({
      figure_type_selector_output
    })
    
    output$figure_editor_feedback <- renderUI({
      NULL
    })
    
    output$save_plot <- renderUI({
      NULL
    })
    
    output$editor_ui <- renderUI({
      if (input$figure_type_selector == "bar") {
        editor_module_ui(session$ns("figure_editor_bar_plot"))
      } else {
        editor_module_ui(session$ns("figure_editor_box_plot"))
    
      }
    })
})

predefined_code_barplot <- "data_summary <- average_trs %>%
    group_by(Stress_Status) %>%
    summarise(
      n = n(),
      mean = mean(Average_Blinks_Per_Minute, na.rm = TRUE),
      sd = sd(Average_Blinks_Per_Minute, na.rm = TRUE),
      sem = sd / sqrt(n)
    )
  
  data_summary$Stress_Status <- factor(data_summary$Stress_Status, levels = c(\"Unstressed\", \"Stressed\"))
  
  barplot <- ggplot(data_summary, aes(x = Stress_Status, y = mean, fill = Stress_Status)) +
    geom_bar(stat = \"identity\", color = \"black\", position = position_dodge()) +
    geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .2, position = position_dodge(.9)) +
    geom_jitter(
      data = average_trs, 
      aes(x = Stress_Status, y = Average_Blinks_Per_Minute), 
      width = 0.2, 
      size = 2, 
      color = \"maroon\"
    ) +
    scale_fill_manual(values = c(\"Unstressed\" = \"grey49\", \"Stressed\" = \"lightgrey\")) +
    labs(x = \"Stress Status\",
         y = \"Mean Blinks/Minute\",
         title = \"Mean Blinks/Minute by Stress Status\") +
    theme_minimal() +
    theme(
      legend.position = \"none\",
      plot.background = element_rect(fill = \"white\", color = NA),
      panel.background = element_rect(fill = \"white\", color = NA)
    ) + 
    ylim(0, max(data_summary$mean + data_summary$sem) * 1.2)"

predefined_code_boxplot <- "average_trs$Stress_Status <- factor(average_trs$Stress_Status, levels = c(\"Unstressed\", \"Stressed\"))

boxplot <- ggplot(average_trs, aes(x = Stress_Status, y = Average_Blinks_Per_Minute, fill = Stress_Status)) + 
  geom_boxplot(outlier.shape = NA, width = 0.5) + 
  geom_jitter(
    data = average_trs, 
    aes(x = Stress_Status, y = Average_Blinks_Per_Minute), 
    width = 0.2, 
    size = 2, 
    color = \"maroon\"
  ) +
  scale_fill_manual(values = c(\"Unstressed\" = \"grey49\", \"Stressed\" = \"lightgrey\")) +
  labs(
    x = \"Stress Status\",
    y = \"Blinks/Minute\",
    title = \"Blinks/Minute by Stress Status\"
  ) +
  theme_minimal() +
  theme(
    legend.position = \"none\",
    plot.background = element_rect(fill = \"white\", color = NA),
    panel.background = element_rect(fill = \"white\", color = NA)
  )
  "

  figure_editor_bar_plot <- editor_module_server("figure_editor_bar_plot", data = average_trs, variable_name = "average_trs", predefined_code = predefined_code_barplot, return_type = "result", session_folder_id, save_header = "Create Bar Plot Code")
  figure_editor_box_plot <- editor_module_server("figure_editor_box_plot", data = average_trs, variable_name = "average_trs", predefined_code = predefined_code_boxplot, return_type = "result", session_folder_id, save_header = "Create Box Plot Code")
  

#bar plot
  observe({
    req(!is.null(figure_editor_bar_plot()))
    if (inherits(barplot, "ggplot")) {
      output$figure_editor_feedback <- renderUI({
        tagList(
          div(class = "success-box", "\U1F64C Great Job!"),
          includeMarkdown(here("BlinkR_app", "markdown", "07_analysis", "analysis_figure_editing_colours.Rmd")),
          box(title = "Open me for a hint",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 12,
              status = "info",
              markdown("Look for existing colour names in the code!")
              )
        )
      })
      
      output$save_plot <- renderUI({
        actionButton(
          session$ns("save_bar_plot"),
          label = tagList(icon("save"), "Save Plot to Dashboard"),
          class = "action-button custom-action"
        )
      })
    } else {
      output$figure_editor_feedback <- renderUI({
        div(class = "error-box", "\U1F914 Not quite - try again!")
    })
      output$save_plot <- renderUI({
        NULL
      })
  }
  })
  
#boxplot
  observe({
    req(!is.null(figure_editor_box_plot()))
    if (inherits(boxplot, "ggplot")) {
      output$figure_editor_feedback <- renderUI({
        tagList(
          div(class = "success-box", "\U1F64C Great Job!"),
          includeMarkdown(here("BlinkR_app", "markdown", "07_analysis", "analysis_figure_editing_colours.Rmd")),
          box(title = "Open me for a hint",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 12,
              status = "info",
              markdown("Look for existing colour names in the code!")
          ),
        )
      })
  
      output$save_plot <- renderUI({
        actionButton(
          session$ns("save_box_plot"),
          label = tagList(icon("save"), "Save Plot to Dashboard"),
          class = "action-button custom-action"
        )
      })
    } else {
      output$figure_editor_feedback <- renderUI({
        div(class = "error-box", "\U1F914 Not quite - try again!")
      })
      output$save_plot <- renderUI({
        NULL
      })
    }
  })
  
  #bar plot save
  observeEvent(input$save_bar_plot, {
    if (!is.null(figure_editor_bar_plot())) {
      if (inherits(figure_editor_bar_plot(), "ggplot")) {
        key <- "bar_plot"
        saved_results$plots[["bar_plot"]] <- NULL
        saved_results$plots[["box_plot"]] <- NULL
        
        saved_results$plots[[key]] <- figure_editor_bar_plot()
        
      } else {
        saved_results$plots[[key]] <- recordPlot()
      }
      results_fig <- (saved_results$plots[[key]])
      
      temp_file <- tempfile(fileext = ".png")
      writeLines(result_as_char, con = temp_file)
      
      path <- drive_get(as_id(session_folder_id))
      
      drive_upload(
        media = temp_file,
        path = path,
        name = paste0(key, ".png"),
        overwrite = TRUE,
      )
      
      unlink(temp_file)
      
      showNotification("Plot saved successfully", type = "message")
    } else {
      showNotification("No plot to save. Please create a plot first.", type = "error")
    }
  })
  
  # box plot save
  observeEvent(input$save_box_plot, {
    if (!is.null(figure_editor_box_plot())) {
      key <- "box_plot"
      saved_results$plots[["bar_plot"]] <- NULL
      saved_results$plots[["box_plot"]] <- NULL
      
      temp_file <- tempfile(fileext = ".png")
      
      if (inherits(figure_editor_box_plot(), "ggplot")) {
        saved_results$plots[[key]] <- figure_editor_box_plot()
        ggsave(
          filename = temp_file,
          plot = saved_results$plots[[key]],
          device = "png",
          width = 8, height = 6, dpi = 300
        )
      } else {
        saved_results$plots[[key]] <- recordPlot()
        png(temp_file, width = 800, height = 600)
        replayPlot(saved_results$plots[[key]])
        dev.off()
      }
      
      path <- drive_get(as_id(session_folder_id))
      drive_upload(
        media = temp_file,
        path = path,
        name = paste0(key, ".png"),
        overwrite = TRUE
      )
      
      unlink(temp_file)
      
      showNotification("Plot saved successfully.", type = "message")
    } else {
      showNotification("No plot to save. Please create a plot first.", type = "error")
    }
  })
  
  
  
  
  
  observeEvent(input$summarise, {
    updateTabItems(parent.session, "sidebar", "Summarise_Data")
  })
  observeEvent(input$statistics, {
    updateTabItems(parent.session, "sidebar", "Statistical_Analysis")
  })
  
# go to dashboard button
observeEvent(input$dashboard, {
  updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
})


  }
  )
}