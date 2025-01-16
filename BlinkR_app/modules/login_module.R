library(shinyauthr)
library(sodium)


custom_login_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("login-panel"),
      class = "panel panel-primary",
      div(class = "panel-heading", h3(class = "panel-title", "Welcome Back")),
      div(class = "panel-body",
          p("Please enter your group ID to continue"), 
          textInput(ns("group_name"), "Group ID", placeholder = "Enter group ID"),
          div(class = "text-danger", textOutput(ns("error"), inline = TRUE)),
          actionButton(ns("login_button"), "Login", class = "btn btn-primary btn-block")
      )
    ),
    div(
      id = ns("signup-panel"),
      class = "panel panel-primary",
      div(class = "panel-heading", h3(class = "panel-title", "Welcome")),
      div(class = "panel-body",
          p("Please Sign Up to Continue"),
          textInput(ns("sign_up_group_name"), "Group ID", placeholder = "Enter a 4 digit group ID"),
          textInput(ns("name"), "Your Name", placeholder = "Enter the name of anyone in your group"),
          div(class = "text-danger", uiOutput(ns("sign_uperror"), inline = TRUE)),
          actionButton(ns("sign_up_button"), "Sign up", class = "btn btn-primary btn-block"),
          uiOutput(ns("sign_up_status")),
      )
    ),
    #uiOutput(ns("logout_ui"))
  )
}


custom_login_server <- function(id, user_base_google_sheet, base_group_files_url) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #credentials <- reactiveValues(user_auth = FALSE, info = NULL, session_folder = NULL)
    
    user_base <- read_sheet(user_base_google_sheet)
    
    credentials <- reactiveValues(
  user_auth = FALSE,
  info = list(Group = NULL, role = NULL, date = NULL, protocol = NULL, data = NULL),
  session_folder = NULL,
  session_folder_url = NULL,
  session_folder_id = NULL
)
    
    observeEvent(input$login_button, {
      req(input$group_name)
      
      user <- user_base %>% 
        filter(Group == input$group_name) %>% 
        slice(1)
      
      if (nrow(user) == 1) {
        credentials$user_auth <- TRUE
        credentials$info <- user
        credentials$info$role <- user$Role
        credentials$info$data <- user$Data
        credentials$info$protocol <- user$Protocol
        output$error <- renderText("")
        
        if (credentials$info$role != "admin") {
          parent_folder_name <- "BlinkR_text_results"
          parent_folder <- googledrive::drive_get(parent_folder_name)
          
          if (nrow(parent_folder) == 0) {
            parent_folder <- googledrive::drive_mkdir(parent_folder_name)
          }
          
          group_name <- input$group_name
          session_folder_name <- group_name
          
          existing_folder <- googledrive::drive_ls(
            path = googledrive::as_id(parent_folder$id),
            pattern = session_folder_name
          )
          
          if (nrow(existing_folder) == 0) {
            new_folder <- googledrive::drive_mkdir(
              name = session_folder_name, 
              path = googledrive::as_id(parent_folder$id)
            )
            
            folder_id <- new_folder$id
            
            credentials$session_folder <- new_folder
          } else {
            credentials$session_folder <- existing_folder
            
            folder_id <- existing_folder$id
          }
          
          drive_share_anyone(as_id(folder_id))
          
          credentials$session_folder_id <- folder_id
          
          folder_url <- paste0(base_group_files_url, folder_id)
          
          credentials$session_folder_url <- folder_url
        } else {
          credentials$session_folder <- NULL
          credentials$session_folder_url <- NULL
          credentials$session_folder_id <- NULL
        }
        
        output$error <- renderText(paste("Logged in successfully."))
      } else {
        output$error <- renderText("Invalid Group ID. Please try again.")
      }
      
    })

    observeEvent(input$sign_up_button, {
      req(input$sign_up_group_name, input$name)
      
      user <- user_base %>%
        filter(Group == input$sign_up_group_name) %>%
        slice(1)
      
      if (!(nrow(user) == 1)) {
        credentials$user_auth <- TRUE
        credentials$info <- list(
          Group = input$sign_up_group_name,
          role = "group",
          date = format(Sys.Date(), "%d/%m/%y"),
          protocol = "FALSE",
          data = "FALSE"
        )
        output$error <- renderText("")
        
        user_data <- data.frame(
          Group = as.character(input$sign_up_group_name),
          Role = as.character("group"),
          Name = as.character(input$name),
          Date = credentials$info$date,
          Protocol = "FALSE",
          Data = "FALSE",
          
          
          stringsAsFactors = FALSE
        )
        
        tryCatch(
          {
            sheet_append(user_base_google_sheet, user_data)
            output$sign_up_status <- renderUI("Group successfully signed up!")
          },
          error = function(e) {
            output$sign_up_status <- renderUI(paste("Error: ", e$message))
          }
        )
        
        if (credentials$info$role != "admin") {
          parent_folder_name <- "BlinkR_text_results"
          parent_folder <- googledrive::drive_get(parent_folder_name)
          
          if (nrow(parent_folder) == 0) {
            parent_folder <- googledrive::drive_mkdir(parent_folder_name)
          }
          
          group_name <- input$sign_up_group_name
          session_folder_name <- group_name
          
          new_folder <- googledrive::drive_mkdir(
            name = session_folder_name,
            path = googledrive::as_id(parent_folder$id)
          )
          
          folder_id <- new_folder$id
          
          credentials$session_folder <- new_folder
          
          drive_share_anyone(as_id(folder_id))
          
          folder_url <- paste0(base_group_files_url, folder_id)
          
          credentials$session_folder_id <- folder_id
          credentials$session_folder_url <- folder_url
        } else {
          credentials$session_folder <- NULL
          credentials$session_folder_url <- NULL
          credentials$session_folder_id <- NULL
        }
      } else {
        output$sign_uperror <- renderUI("Group already exists.")
      }
    })
    
    
    # output$logout_ui <- renderUI({
    #   req(credentials$user_auth)
    #   tagList(
    #     #p(paste("Logged in as:", credentials$info$Group)),
    #     actionButton(ns("logout_button"), "Log out", class = "btn btn-danger btn-block")
    #   )
    # })
    # 
    # observeEvent(input$logout_button, {
    #     req(credentials$user_auth)      
    #   # if (!is.null(credentials$session_folder)) {
    #   #   folder_files <- googledrive::drive_ls(credentials$session_folder)
    #   #   if (nrow(folder_files) == 0) {
    #   #     googledrive::drive_rm(credentials$session_folder)
    #   #     output$error <- renderText("Session folder was empty and has been deleted.")
    #   #   } else {
    #   #     output$error <- renderText("Session folder contains files and was not deleted.")
    #   #   }
    #   # } else {
    #   #   output$error <- renderText("Session folder does not exist.")
    #   # }
    #   
    #     credentials$user_auth <- NULL
    #     credentials$info <- list(Group = NULL)
    #     credentials$session_folder <- NULL
    #   
    #     output$logout_ui <- renderUI({ NULL }) 
    #     output$error <- renderText("Logged out successfully.")
    #       })
    # 
    reactive({
      list(
        user_auth = credentials$user_auth,
        user_info = credentials$info,
        session_folder = credentials$session_folder,
        session_folder_url = credentials$session_folder_url,
        session_folder_id = credentials$session_folder_id
      )
    })
    
    
  })
}