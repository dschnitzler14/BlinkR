measurements_module_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "Measurements",
    fluidPage(
      fluidRow(
        uiOutput(ns("students_ui"))
      )
    )
  )
}

measurements_module_server <- function(id, db_student_table, db_measurement) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observe({
        req(db_student_table())
        num_students <- nrow(db_student_table())
        
        if (num_students != 0) {
          output$students_ui <- renderUI({
            student_ui_list <- lapply(1:num_students, function(i) {
              student_name <- db_student_table()$Initials[i]
              group_name <- db_student_table()$Group[i]
              student_ID <- db_student_table()$ID[i]
              measurement_input_module_ui(
                ns(paste0("student_module_", student_name, " | ", student_ID)),
                student_number = student_name,
                student_ID = student_ID
                )
            })
            
            do.call(fluidRow, student_ui_list)
          })
          
          lapply(1:num_students, function(i) {
            student_name <- db_student_table()$Initials[i]
            group_name <- db_student_table()$Group[i]
            student_ID <- db_student_table()$ID[i]
            measurement_input_module_server(
              ns(paste0("student_module_", student_name, " | ", student_ID)),
              student_number = student_name,
              student_ID = student_ID,
              group_name = group_name,
              db_measurement
            )
          })
          
        } else {
          output$students_ui <- renderUI({
            tagList(
              h3("No Subjects Added"),
              p("Please check the database or add new students in 'User Area' to proceed.")
            )
          })
        }
      })
    }
  )
}

