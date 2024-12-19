library(shinyauthr)
library(sodium)


custom_login_ui <- function(id, title = "Welcome", subtitle = "Please enter your group ID to continue") {
  ns <- NS(id)
  tagList(
    div(
      id = ns("login-panel"),
      class = "panel panel-primary",
      div(class = "panel-heading", h3(class = "panel-title", title)),
      div(class = "panel-body",
          p(subtitle), 
          textInput(ns("user_name"), "Group ID", placeholder = "Enter group ID"),
          div(class = "text-danger", textOutput(ns("error"), inline = TRUE)),
          actionButton(ns("login_button"), "Log in", class = "btn btn-primary btn-block")
      )
    ),
    div(
      id = ns("signup-panel"),
      class = "panel panel-primary",
      div(class = "panel-heading", h3(class = "panel-title", title)),
      div(class = "panel-body",
          p(subtitle),
          textInput(ns("sign_up_user_name"), "Group ID", placeholder = "Enter a 4 digit group ID"),
          textInput(ns("name"), "Your Name", placeholder = "Enter the name of anyone in your group"),
          div(class = "text-danger", textOutput(ns("sign_uperror"), inline = TRUE)),
          actionButton(ns("sign_up_button"), "Sign up", class = "btn btn-primary btn-block"),
          textOutput(ns("sign_up_status")),
      )
    ),
    uiOutput(ns("logout_ui"))
  )
}


custom_login_server <- function(id, user_base, user_base_google_sheet) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    credentials <- reactiveValues(user_auth = FALSE, info = NULL, session_folder = NULL)
    
    observeEvent(input$login_button, {
      req(input$user_name)
      
      user <- user_base %>% 
        filter(User == input$user_name) %>% 
        slice(1)
      
      if (nrow(user) == 1) {
        credentials$user_auth <- TRUE
        credentials$info <- user
        output$error <- renderText("")
        
        parent_folder_name <- "BlinkR_text_results"
        parent_folder <- googledrive::drive_get(parent_folder_name)
        
        if (nrow(parent_folder) == 0) {
          parent_folder <- googledrive::drive_mkdir(parent_folder_name)
        }
        
        group_name <- user$name
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
          credentials$session_folder <- new_folder
        } else {
          credentials$session_folder <- existing_folder
        }
        
        output$error <- renderText(paste("Logged in successfully. Session folder created:", session_folder_name))
      } else {
        output$error <- renderText("Invalid Group ID. Please try again.")
      }
    })
    
    observeEvent(input$sign_up_button, {
      req(input$sign_up_user_name, input$name)
      
      user <- user_base %>%
        filter(User == input$sign_up_user_name) %>%
        slice(1)
      
      if (!(nrow(user) == 1)) {
        credentials$user_auth <- TRUE
        credentials$info <- user
        output$error <- renderText("")
        
        user_data <- data.frame(
          input$sign_up_user_name,  
          "group",                  
          input$name,               
          stringsAsFactors = FALSE
        )
        
        tryCatch(
          {
            sheet_append(user_base_google_sheet, user_data)
            output$sign_up_status <- renderText("User successfully signed up!")
          },
          error = function(e) {
            output$sign_up_status <- renderText(paste("Error: ", e$message))
          }
        )
        
        parent_folder_name <- "BlinkR_text_results"
        parent_folder <- googledrive::drive_get(parent_folder_name)
        
        group_name <- input$sign_up_user_name
        session_folder_name <- group_name
        
        new_folder <- googledrive::drive_mkdir(
          name = session_folder_name, 
          path = googledrive::as_id(parent_folder$id)
        )
        credentials$session_folder <- new_folder
        
      } else {
        output$sign_uperror <- renderText("User already exists.")
      }
    })
    
    
    output$logout_ui <- renderUI({
      req(credentials$user_auth)
      tagList(
        p(paste("Logged in as:", credentials$info$name)),
        actionButton(ns("logout_button"), "Log out", class = "btn btn-danger btn-block")
      )
    })
    
    observeEvent(input$logout_button, {
      req(credentials$session_folder)
      
      if (!is.null(credentials$session_folder$id)) {
        folder_files <- googledrive::drive_ls(credentials$session_folder$id)
        if (nrow(folder_files) == 0) {
          googledrive::drive_rm(credentials$session_folder)
          output$error <- renderText("Session folder was empty and has been deleted.")
        } else {
          output$error <- renderText("Session folder contains files and was not deleted.")
        }
      } else {
        output$error <- renderText("Session folder does not exist.")
      }
      
      credentials$user_auth <- FALSE
      credentials$info <- NULL
      credentials$session_folder <- NULL
    })
    
    reactive({
      list(
        user_auth = credentials$user_auth,
        user_info = credentials$info,
        session_folder = credentials$session_folder
      )
    })
    
    
  })
}