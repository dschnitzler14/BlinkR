analysis_create_figure_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Create_Figure",
    fluidPage(
      fluidRow(
            column(
              width = 12,
              div(
                class = "page-title-box",
                tags$h2(
                  tagList(shiny::icon("chart-simple"), "Analysis: Create Figure")
                )
      )
    )),
      fluidRow(
        column(
          12,
          
          box(
            title = "1️⃣ Creating a Figure",
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
                uiOutput(ns("figure_type_selector_output"))
                
              ),
              column(
                8,
                div(class = "editor-container", uiOutput(ns("editor_ui"))),
                uiOutput(ns("save_plot")),
                uiOutput(ns("figure_editor_feedback"))
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
                label = tagList(icon("dashboard"), "Go to Analysis Dashboard"),
                class = "action-button custom-dark-yellow"
              )
            )
          )

),
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
        ns("back_page_figure"),
        label = tagList(icon("arrow-left"), " Back"),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_figure"), 
        label = tagList("Next ", icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
)))

}

analysis_create_figure_module_server <- function(id, results_data, parent.session, saved_results, session_folder_id) {
  moduleServer(id, function(input, output, session) {

          vars <- get_experiment_vars()

  average_trs <- reactive({ NULL })
    
  average_trs_results <- results_data() %>%
      select(-"group", -"initials", -"submission_id") %>%
      dplyr::group_by(id, !!sym(vars$levels_variable_name)) %>%
      dplyr::summarise(
        average_measurement = mean(!!sym(vars$measurement_variable_name), na.rm = TRUE),
        .groups = 'drop'
      )
  
  average_trs <- reactive({ average_trs_results })
  
  
  #Make Figure

rmd_content_analysis_create_figure_barplot <- readLines("markdown/07_analysis/analysis_create_figure_barplot.Rmd")
processed_rmd_analysis_create_figure_barplot <- whisker.render(paste(rmd_content_analysis_create_figure_barplot, collapse = "\n"), vars)
rmd_content_analysis_create_figure_boxplot <- readLines("markdown/07_analysis/analysis_create_figure_boxplot.Rmd")
processed_rmd_analysis_create_figure_boxplot <- whisker.render(paste(rmd_content_analysis_create_figure_boxplot, collapse = "\n"), vars)

  observeEvent(input$figure_type_selector, {
    figure_type_selector_output <- if (input$figure_type_selector == "bar") {
      HTML(markdownToHTML(text = processed_rmd_analysis_create_figure_barplot, fragment.only = TRUE))
    } else {
      HTML(markdownToHTML(text = processed_rmd_analysis_create_figure_boxplot, fragment.only = TRUE))
    }
    
    output$figure_type_selector_output <- renderUI({
      req(input$figure_type_selector)

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


predefined_code_barplot <- whisker.render(
  read_file(
      "markdown/07_analysis/predefined_code_barplot.txt"
    ),
    vars)

predefined_code_boxplot <- whisker.render(
  read_file(
      "markdown/07_analysis/predefined_code_box_plot.txt"
    ),
    vars)

  figure_editor_bar_plot <- editor_module_server("figure_editor_bar_plot", data = average_trs, variable_name = "average_trs", predefined_code = predefined_code_barplot, return_type = "result", session_folder_id, save_header = "Create Bar Plot Code")
  figure_editor_box_plot <- editor_module_server("figure_editor_box_plot", data = average_trs, variable_name = "average_trs", predefined_code = predefined_code_boxplot, return_type = "result", session_folder_id, save_header = "Create Box Plot Code")
  

#bar plot
  observe({
    req(!is.null(input$figure_type_selector), input$figure_type_selector == "bar")
    req(!is.null(figure_editor_bar_plot()), !is.null(figure_editor_bar_plot()$result))
    if (isTRUE(figure_editor_bar_plot()$is_plot)) {
      output$figure_editor_feedback <- renderUI({
        tagList(
          div(class = "success-box", "\U1F64C Great Job!"),
          includeMarkdown("markdown/07_analysis/change_axis.Rmd"),
          includeMarkdown("markdown/07_analysis/analysis_figure_editing_colours.Rmd"),
          box(title = "💡 Open me for a hint",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 12,
              status = "info",
              markdown("Look for existing colour names in the code!")
              )
        )
      })
      
      output$save_plot <- renderUI({
        tagList(
          div(
            style = "display: flex; justify-content: center; align-items: center; width: 100%;",
        actionButton(
          session$ns("save_bar_plot"),
          label = tagList(icon("save"), "Save Plot to Dashboard"),
          class = "action-button custom-action"
        )
          )
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
   req(!is.null(input$figure_type_selector), input$figure_type_selector == "box")
  req(!is.null(figure_editor_box_plot()), !is.null(figure_editor_box_plot()$result))

  if (isTRUE(figure_editor_box_plot()$is_plot) && inherits(figure_editor_box_plot()$result, "ggplot")) {
      output$figure_editor_feedback <- renderUI({
        tagList(
          div(class = "success-box", "\U1F64C Great Job!"),
          includeMarkdown("markdown/07_analysis/change_axis.Rmd"),
          includeMarkdown("markdown/07_analysis/analysis_figure_editing_colours.Rmd"),
          box(title = "💡 Open me for a hint",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 12,
              status = "info",
              markdown("Look for existing colour names in the code!")
          ),
        )
      })
  
      output$save_plot <- renderUI({
        tagList(
          div(
            style = "display: flex; justify-content: center; align-items: center; width: 100%;",
        actionButton(
          session$ns("save_box_plot"),
          label = tagList(icon("save"), "Save Plot to Dashboard"),
          class = "action-button custom-action"
        )
        )
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
        key <- "bar_plot"

        saved_results$plots[["bar_plot"]] <- NULL
        saved_results$plots[["box_plot"]] <- NULL

        if (inherits(figure_editor_bar_plot()$result, "ggplot")) {
            saved_results$plots[[key]] <- figure_editor_bar_plot()$result
        } else if (inherits(figure_editor_bar_plot()$result, "recordedplot")) {
            saved_results$plots[[key]] <- figure_editor_bar_plot()$result
        } else {
            showNotification("Invalid plot type. Unable to save.", type = "error", duration = 3)
            return()
        }

        temp_file <- tempfile(fileext = ".png")
        png(temp_file, width = 800, height = 600)
        tryCatch({
            if (inherits(saved_results$plots[[key]], "ggplot")) {
                print(saved_results$plots[[key]])
            } else if (inherits(saved_results$plots[[key]], "recordedplot")) {
                replayPlot(saved_results$plots[[key]])
            }
        }, finally = {
            dev.off()
        })

        path <- drive_get(as_id(session_folder_id))
        drive_upload(
            media = temp_file,
            path = path,
            name = paste0(key, ".png"),
            overwrite = TRUE
        )

        unlink(temp_file)
        showNotification("Plot saved successfully", type = "message", duration = 3)
    } else {
        showNotification("No plot to save. Please create a plot first.", type = "error", duration = 3)
    }
})

 
  
  # box plot save
  observeEvent(input$save_box_plot, {
  req(figure_editor_box_plot()$result)

  if (inherits(figure_editor_box_plot()$result, "ggplot")) {
    key <- "box_plot"
    saved_results$plots[[key]] <- figure_editor_box_plot()$result

    temp_file <- tempfile(fileext = ".png")
    ggsave(
      filename = temp_file,
      plot = saved_results$plots[[key]],
      device = "png",
      width = 8, height = 6, dpi = 300
    )

    path <- drive_get(as_id(session_folder_id))
    drive_upload(
      media = temp_file,
      path = path,
      name = paste0(key, ".png"),
      overwrite = TRUE
    )

    unlink(temp_file)
    showNotification("Plot saved successfully.", type = "message", duration = 3)
  } else {
    showNotification("No valid plot to save.", type = "error", duration = 3)
  }
})


  
  observeEvent(input$summarise, {
    updateTabItems(parent.session, "sidebar", "Summarise_Data")
  })
  observeEvent(input$statistics, {
    updateTabItems(parent.session, "sidebar", "Statistical_Analysis")
  })
  
# Go to Analysis Dashboard button
observeEvent(input$dashboard, {
  updateTabItems(parent.session, "sidebar", "Analysis_Dashboard")
})

observeEvent(input$back_page_figure, {
        updateTabItems(parent.session, "sidebar", "Summarise_Data")
      })
      observeEvent(input$next_page_figure, {
        updateTabItems(parent.session, "sidebar", "Statistical_Analysis")
      })


  }
  )
}