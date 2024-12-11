# numeric input module ----
measurement_input_module_ui <- function(id, label = "Label"){
  ns <- NS(id)
  tagList(
    numericInput(
      inputId = ns("numeric_input1"),
      label = label,
      value = 0,
      min = 5,
      max = 60
    )
  )
}

measurement_input_module_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
    }
  )
}


# student tab box module ----
student_tabBox_module_UI <- function(id, student_number) {
  ns <- NS(id)
  tagList(
  tabBox(
    title = paste("Subject", student_number),
    id = ns("student_box"),
    width = 12,
    side = "left",
    
    tabPanel(
      title = "Unstressed Condition",
      # fluidRow(
      #   column(width = 4, measurement_input_module_ui(id = ns("unstressed_condition_module_1"), label = "Blinks/ 1 Minute: Technical Replicate 1")),
      #   column(width = 4, measurement_input_module_ui(id = ns("unstressed_condition_module_2"), label = "Blinks/ 1 Minute: Technical Replicate 2")),
      #   column(width = 4, measurement_input_module_ui(id = ns("unstressed_condition_module_3"), label = "Blinks/ 1 Minute: Technical Replicate 3"))
      # ),
      p("text goes here"),
      actionButton(ns("submit_unstressed"), "Submit Unstressed Measurements"),
      uiOutput(ns('success_msg'))
    ),
    
    tabPanel(
       title = "Stressed Condition",
      # fluidRow(
      #   column(width = 4, measurement_input_module_ui(id = ns("stressed_condition_module_1"), label = "Blinks/ 1 Minute: Technical Replicate 1")),
      #   column(width = 4, measurement_input_module_ui(id = ns("stressed_condition_module_2"), label = "Blinks/ 1 Minute: Technical Replicate 2")),
      #   column(width = 4, measurement_input_module_ui(id = ns("stressed_condition_module_3"), label = "Blinks/ 1 Minute: Technical Replicate 3"))
      # ),
      # actionButton(ns("submit_stressed"), "Submit Stressed Measurements"),
    )
  )
)
}


student_tabBox_module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      #Observe the submit button
      observeEvent(input$submit_unstressed, {
        output$success_msg <- renderUI({
          p("Success")
        })
      

        # 
        # unstressed_1 <- input[["unstressed_condition_module_1-numeric_input1"]]
        # unstressed_2 <- input[["unstressed_condition_module_2-numeric_input1"]]
        # unstressed_3 <- input[["unstressed_condition_module_3-numeric_input1"]]
        # 
        # if (unstressed_1 != 0 & unstressed_2 != 0 & unstressed_3 != 0) {
        #   showNotification("All unstressed measurements are valid and submitted successfully.", type = "message")
        # } else {
        #   showNotification("Please ensure all measurement fields have non-zero values before submitting.", type = "error")
        # }
      })
    }
  )
}

# student_tabBox_module_server <- function(id, db_measurement, student_id, initials, auth) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       ns <- session$ns
#       
#       measurement_input_module_server(id = "unstressed_condition_module_1")
#       measurement_input_module_server(id = "unstressed_condition_module_2")
#       measurement_input_module_server(id = "unstressed_condition_module_3")
#       measurement_input_module_server(id = "stressed_condition_module_1")
#       measurement_input_module_server(id = "stressed_condition_module_2")
#       measurement_input_module_server(id = "stressed_condition_module_3")
#       
#       observeEvent(input$submit_unstressed, {
# 
#         # Retrieve unstressed inputs
#         unstressed_1 <- input[[ns("unstressed_condition_module_1-numeric_input1")]]
#         unstressed_2 <- input[[ns("unstressed_condition_module_2-numeric_input1")]]
#         unstressed_3 <- input[[ns("unstressed_condition_module_3-numeric_input1")]]
#         
#         print(paste("Input values:", unstressed_1, unstressed_2, unstressed_3))
#         
#         if (is.null(unstressed_1) || is.null(unstressed_2) || is.null(unstressed_3)) {
#           showNotification("Please complete all unstressed measurements before submitting.", type = "error")
#           return()
#         }
#         
#         # Retrieve logged-in user
#         logged_in_user <- auth()$user_info$user
#         
#         # Create new entries for the database
#         new_entries <- data.frame(
#           Group = rep(logged_in_user, 3),
#           ID = rep(student_id, 3),
#           Initials = rep(initials, 3),
#           Stress_Status = rep("Unstressed", 3),
#           Technical_Replicate = 1:3,
#           Blinks_Per_Minute = c(unstressed_1, unstressed_2, unstressed_3),
#           stringsAsFactors = FALSE
#         )
#         
#         print(new_entries)
#         
#         db_measurement(rbind(db_measurement(), new_entries))
# 
#         # Output success messages
#         output$Submission_unstressed_outcome <- renderText("Unstressed measurements submitted.")
#         showNotification("Unstressed measurements submitted successfully.", type = "message")
#       })
#       
#     }
#   )
# }
# 
# 


# main measurements module ----
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


measurements_module_server <- function(id, db) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observe({
        req(db())
        num_students <- nrow(db())
        
        if (num_students != 0) {
          output$students_ui <- renderUI({
            student_ui_list <- lapply(1:num_students, function(i) {
              student_name <- db()$Initials[i]
              student_tabBox_module_UI(
                id = paste0("student_module_", i),
                student_number = student_name
              )
            })
            do.call(fluidRow, student_ui_list)
          })
          
          # Call the server function for each student module
          lapply(1:num_students, function(i) {
            student_tabBox_module_server(
              id = paste0("student_module_", i)
                   
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


# 
# measurements_module_server <- function(id, db) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       ns <- session$ns
#       
#       observe({
#         req(db())
#         num_students <- nrow(db())
#         
#         #test
#         if(num_students != 0){
#         
#         output$students_ui <- renderUI({
#           student_ui_list <- lapply(1:num_students, function(i) {
#             student_name <- db()$Initials[i]
#             student_id <- db()$ID[i]
#             
#             student_tabBox_module_UI(
#               id = ns(paste0("student_module_", i)),
#               student_number = student_name
#             )
#           })
#           do.call(fluidRow, student_ui_list)
#         })
#         
#         lapply(1:num_students, function(i) {
#           student_id <- db()$ID[i]
#           student_tabBox_module_server(
#             id = paste0(id, "-student_module_", i),
#             db_measurement = db_measurement,  # Reactive database
#             student_id = student_id,          # Student ID
#             initials = db()$Initials[i],      # Student initials
#             auth = auth                       # Pass the auth object
#           )
#         })
#         
#         } #test
#         else {
#           output$students_ui <- renderUI({
#             tagList(
#               h3("No Subjects Added"),
#               p("Please check the database or add new students in 'User Area' to proceed.")
#             )
#           })
#         }
#       })
#     }
#   )
# }


