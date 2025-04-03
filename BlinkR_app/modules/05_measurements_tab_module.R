measurements_module_ui <- function(id, i18n) {
  ns <- NS(id)
  tabItem(
    tabName = "Measurements",
    fluidPage(
      fluidRow(
            column(
              width = 12,
              div(
                class = "page-title-box",
                tags$h2(
                  tagList(shiny::icon("ruler"), i18n$t("Measurements"))
                )
      )
    )),
      fluidRow(
        box(
          title = tagList(shiny::icon("circle-plus"), i18n$t("Add Students")),
          collapsible = FALSE,
          width = 12,
          solidHeader = TRUE,
          textOutput(ns("get_started_text")),
          group_info_module_ui(ns("add_students"), i18n)
        ),
        box(
          title = tagList(shiny::icon("ruler"), i18n$t("Measurements")),
          collapsible = FALSE,
          width = 12,
          solidHeader = TRUE,
          textOutput(ns("students_added_helper_text")),
          uiOutput(ns("students_ui")),
          textOutput(ns("please_add_students"))
        )
      ),
      fluidRow(
        column(
          width = 12,
          div(
            style = "display: flex; justify-content: center; align-items: center; height: 100px;",
            actionButton(
              ns("raw_data"),
              label = tagList(icon("database"), i18n$t("View Raw Data")),
              class = "action-button custom-action"
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
        ns("back_page_measure"),
        label = tagList(icon("arrow-left"), HTML("&nbsp;"), i18n$t("Back")),
        class = "fun-nav-button"
      ),
      actionButton(
        ns("next_page_measure"), 
        label = tagList(i18n$t("Next"),  HTML("&nbsp;"), icon("arrow-right")), 
        class = "fun-nav-button"
      )
    )
  )
)
    )
  )
}


measurements_module_server <- function(id, i18n, db_student_table, db_measurement, auth, parent.session, include_markdown_language) {
  moduleServer(
    id,
    function(input, output, session) {
            vars <- get_experiment_vars()

      ns <- session$ns

      output$get_started_text <- renderText(
            i18n$t("To get started, use this box to enter everyone's initials.\n
                   Please enter them one at a time and hit enter after each one.\n
                   If you make a mistake, you can delete a student below.\n
                   Each student will be assigned an \"ID\" for this experiment.")
          )
      
      observeEvent(input$back_page_measure, {
        updateTabItems(parent.session, "sidebar", "Protocol")
      })
      observeEvent(input$next_page_measure, {
        updateTabItems(parent.session, "sidebar", "Raw_Data")
      })
      
      group_info_module_server("add_students", i18n, db_student_table = db_student_table, auth = auth)
      
      student_ids <- reactiveVal(character())
      
      observeEvent(db_student_table(), {
        req(db_student_table())
        
        if (nrow(db_student_table()) != 0) {
          output$please_add_students <- renderText(
            NULL
          )
          current_ids <- db_student_table()$id
          
          new_students <- setdiff(current_ids, student_ids())
          
          for (student_ID in new_students) {
            student_row <- db_student_table() %>% filter(id == student_ID)
            student_name <- student_row$initials
            group_name <- student_row$group
            
            generate_random_id <- function() {
              sprintf("%06d", sample(0:999999, 1))
            }
            
            submission_id_value <- paste0(student_ID, "_", generate_random_id())
            
            if (!"submission_id" %in% colnames(db_student_table())) {
              db_student_table <- db_student_table() %>%
                mutate(submission_id = NA_character_)
            }
            
            db_student_table <- db_student_table %>%
              mutate(submission_id = ifelse(id == student_ID, submission_id_value, submission_id))
            
            insertUI(
              selector = paste0("#", ns("students_ui")),
              where = "beforeEnd",
              ui = fluidRow(
                id = ns(paste0("row_", student_ID)),
                column(10,
                       measurement_input_module_ui(
                         ns(paste0("student_module_", student_ID)),
                         i18n, 
                         student_name = student_name,
                         student_ID = student_ID,
                         db_student_table = db_student_table
                       )
                ),
                column(2,
                       actionButton(
                         ns(paste0("delete_student_", student_ID)),
                         label = i18n$t("Delete Student"),
                         class = "fun-delete-button"
                       )
                )
              )
            )
            
            measurement_input_module_server(
              paste0("student_module_", student_ID),
              i18n,
              student_name = student_name,
              student_ID = student_ID,
              group_name = group_name,
              submission_id = submission_id_value,
              db_measurement = db_measurement,
              db_student_table = db_student_table,
              include_markdown_language = include_markdown_language
            )
            
            observeEvent(input[[paste0("delete_student_", student_ID)]], {
              updated_students <- db_student_table() %>% filter(id != student_ID)
              db_student_table(updated_students)
              
              updated_measurements <- db_measurement() %>% filter(id != student_ID)
              db_measurement(updated_measurements)
              
              removeUI(selector = paste0("#", ns(paste0("row_", student_ID))))
              
              student_ids(setdiff(student_ids(), student_ID))
              
              showNotification(paste(i18n$t("Deleted student with ID:"), student_ID), type = "message", duration = 3)
            }, ignoreInit = TRUE)
          }
          
          student_ids(current_ids)
        } else {
          output$please_add_students <- renderText(
            i18n$t("To get started, please add students in the box above!")
          )
        }
      }, ignoreNULL = FALSE, ignoreInit = FALSE)
      
      output$students_ui <- renderUI({
        div(id = ns("students_ui"))
      })
      
      output$students_added_helper_text <- renderText(
        i18n$t("You can enter your measurement results here.\n
        If you make a mistake, you can re-enter the results and overwrite the existing data.\n
        You can also delete students from your group using the button on the right.")
        
      )
      
      observeEvent(input$raw_data, {
        updateTabItems(parent.session, "sidebar", "Raw_Data")
      })  
    }
  )
}
