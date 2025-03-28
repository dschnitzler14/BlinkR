library(shinyauthr)
library(sodium)



custom_login_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
          div(
            id = ns("signup-login-choice"),
            class = "custom-login-panel",
            
            h3("Welcome to BlinkR"),
            p("Please Login with your Group ID or Sign Up with a new Group ID to continue"),
            
            uiOutput(ns("login_signup_choice")),
            
            div(
              id = ns("inner-container"),
              uiOutput(ns("signup_panel")),
              uiOutput(ns("login_panel")),
              uiOutput(ns("sign_up_status"))
            )
          )
        )
    )
    
    
  )
}


custom_login_server <- function(id, user_base_sheet_id, all_users, base_group_files_url, external_logout_button = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

    user_base <- reactiveVal()


      observe({
      user_base(read_sheet(user_base_sheet_id))
    })

output$login_signup_choice <- renderUI({
  div(
    id = ns("login-signup-options"),
  
  tagList(
    actionButton(ns("login_choice_button"), tagList(shiny::icon("circle-check"), "I have a Group ID"), class = "fun-choice-button"),
    actionButton(ns("sign_up_choice_button"), tagList(shiny::icon("circle-xmark"), "I do not have a Group ID"), class = "fun-choice-button")
  )
  )
})

observeEvent(input$sign_up_choice_button, {
output$signup_panel <- renderUI({
  req(input$sign_up_choice_button)
  output$login_panel <- renderUI({NULL})

  tagList(
    div(
      id = ns("signup-panel"),
      p("Please Sign Up to Continue"),
        div(
        style = "max-width: 300px; margin: 0 auto;",
        p("Enter a 4-digit number or generate a random Group ID")
      ),
      actionButton(ns("generate_random_ID"), "Generate New Group ID",  class = "generate-id-button"),
      textInput(ns("sign_up_group_name"), "Group ID", placeholder = "Enter a new Group ID"),
      textInput(ns("name"), "Your First Name", placeholder = "Enter the First Name of Anyone in your Group"),
      div(class = "text-danger", uiOutput(ns("sign_uperror"), inline = TRUE)),
      actionButton(ns("sign_up_button"), tagList(shiny::icon("right-to-bracket"), "Sign Up"), class = "custom-login-button"),
      actionButton(ns("cancel_sign_up"), tagList(shiny::icon("trash-can"), "Cancel Signup"), class = "custom-cancel-button")
    )
  )
})
})

observeEvent(input$login_choice_button, {

output$login_panel <- renderUI({
  req(input$login_choice_button)
  output$signup_panel <- renderUI({NULL})

  tagList(
    div(
      id = ns("login-panel"),
      p("Please enter your Group ID to continue"),
      textInput(ns("group_name"), "Group ID", placeholder = "Enter your Group ID"),
      div(class = "text-danger", textOutput(ns("error"), inline = TRUE)),
      actionButton(ns("login_button"), tagList(shiny::icon("right-to-bracket"), "Login"), class = "custom-login-button"),
      actionButton(ns("cancel_login"), tagList(shiny::icon("trash-can"), "Cancel Login"), class = "custom-cancel-button")
    )
  )
})
})

observeEvent(input$cancel_login, {
  output$login_panel <- renderUI({NULL})
  output$signup_panel <- renderUI({NULL})
  output$login_signup_choice <- renderUI({
    div(
    id = ns("login-signup-options"),
    tagList(
       actionButton(ns("login_choice_button"), tagList(shiny::icon("circle-check"), "I have a Group ID"), class = "fun-choice-button"),
       actionButton(ns("sign_up_choice_button"), tagList(shiny::icon("circle-xmark"), "I do not have a Group ID"), class = "fun-choice-button")
    )
    )
  })
})

observeEvent(input$cancel_sign_up, {
  output$signup_panel <- renderUI({NULL})
  output$login_panel <- renderUI({NULL})
  output$login_signup_choice <- renderUI({
    div(
    id = ns("login-signup-options"),
    tagList(
       actionButton(ns("login_choice_button"), tagList(shiny::icon("circle-check"), "I have a Group ID"), class = "fun-choice-button"),
       actionButton(ns("sign_up_choice_button"), tagList(shiny::icon("circle-xmark"), "I do not have a Group ID"), class = "fun-choice-button")
    )
    )
  })
})

  credentials <- reactiveValues(
  user_auth = FALSE,
  info = list(group = NULL, role = NULL, date = NULL, protocol = NULL, data = NULL),
  session_folder = NULL,
  session_folder_url = NULL,
  session_folder_id = NULL
  )

observeEvent(input$generate_random_ID, {
  existing_ids <- user_base()$group
  
  repeat {
    new_group_id <- sample(1000:9999, 1)
    if (!(new_group_id %in% existing_ids)) {
      break
    }
  }
  updateTextInput(session, "sign_up_group_name", value = new_group_id)
})
    
    observeEvent(input$login_button, {
      req(input$group_name)
      shinyjs::disable("login_button")

      
      user <- user_base() %>% 
        filter(group == input$group_name) %>% 
        slice(1)
      
      if (nrow(user) == 1) {
  credentials$user_auth <- TRUE
  credentials$info <- user
  credentials$info$role <- user$Role
  credentials$info$data <- user$Data
  credentials$info$protocol <- user$Protocol
  output$error <- renderText("")

  parent_folder_name <- "BlinkR_text_results"
  parent_folder <- googledrive::drive_get(parent_folder_name)

  if (nrow(parent_folder) == 0) {
    parent_folder <- googledrive::drive_mkdir(parent_folder_name)
  }

  if (credentials$info$role != "admin") {
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
    credentials$session_folder <- parent_folder
    credentials$session_folder_id <- parent_folder$id 
    credentials$session_folder_url <- paste0(base_group_files_url, parent_folder$id)
  }

  output$error <- renderText("Logged in successfully.")
} else {
  output$error <- renderText("Invalid Group ID. Please try again.")
}

      
    })

    observeEvent(input$sign_up_button, {
      req(input$sign_up_group_name, input$name)

      shinyjs::disable("sign_up_button")

      current_users <- all_users()

      user <- current_users %>%
        filter(group == input$sign_up_group_name) %>%
        slice(1)

      if (nrow(user) < 1) {
        credentials$user_auth <- TRUE
        credentials$info <- list(
          group = input$sign_up_group_name,
          role = "group",
          date = format(Sys.Date(), "%d/%m/%y"),
          protocol = "FALSE",
          data = "FALSE"
        )
        output$error <- renderText("")

        new_user <- data.frame(
          group = as.character(input$sign_up_group_name),
          Role = as.character("group"),
          Name = as.character(input$name),
          Date = credentials$info$date,
          Protocol = "FALSE",
          Data = "FALSE",
          stringsAsFactors = FALSE
        )

        tryCatch({
          googlesheets4::sheet_append(user_base_sheet_id, new_user)


          updated_df <- googlesheets4::read_sheet(user_base_sheet_id)
          all_users(updated_df)

          output$sign_up_status <- renderUI("Group successfully signed up!")
        },
        error = function(e) {
          output$sign_up_status <- renderUI(paste("Error: ", e$message))
        })


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

          drive_share_anyone(googledrive::as_id(folder_id))

          credentials$session_folder <- new_folder

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

        user_base(read_sheet(user_base_sheet_id))

    })
    
   observeEvent(external_logout_button(), ignoreInit = TRUE, {
      req(credentials$user_auth)
      
      tryCatch({
      folder_files <- googledrive::drive_ls(credentials$session_folder)
      if (nrow(folder_files) == 0) {
        googledrive::drive_rm(credentials$session_folder)
        output$error <- renderText("Session folder was empty and has been deleted.")
      } else {
        output$error <- renderText("Session folder contains files and was not deleted.")
      }
    }, error = function(e) {
      output$error <- renderText(paste("Error checking or deleting session folder:", e$message))
    })
      
      credentials$user_auth <- FALSE
      credentials$info <- list(group = NULL, role = NULL, date = NULL, protocol = NULL, data = NULL)
      credentials$session_folder <- NULL
      credentials$session_folder_id <- NULL
      credentials$session_folder_url <- NULL

      session$reload()

      output$error <- renderText("Logged out successfully.")
        user_base(read_sheet(user_base_sheet_id))

    })

    return(
      reactive({
        list(
          user_auth = credentials$user_auth,
          user_info = credentials$info,
          session_folder = credentials$session_folder,
          session_folder_url = credentials$session_folder_url,
          session_folder_id = credentials$session_folder_id
        )
      })
    )
  
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