#  login_module.R
# 
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
    # Logout UI
    uiOutput(ns("logout_ui"))
  )
}

custom_login_server <- function(id, user_base) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to hold authentication state
    credentials <- reactiveValues(user_auth = FALSE, info = NULL)
    
    # Login logic
    observeEvent(input$login_button, {
      req(input$user_name)
      
      # Check if entered group ID exists in the user_base
      user <- user_base %>% 
        filter(user == input$user_name) %>% 
        slice(1)
      
      if (nrow(user) == 1) {
        # Successful authentication
        credentials$user_auth <- TRUE
        credentials$info <- user
        output$error <- renderText("")  # Clear error message
      } else {
        # Invalid group ID
        output$error <- renderText("Invalid Group ID. Please try again.")
      }
    })
    
    # Logout UI
    output$logout_ui <- renderUI({
      req(credentials$user_auth)
      tagList(
        p(paste("Logged in as:", credentials$info$name)),
        actionButton(ns("logout_button"), "Log out", class = "btn btn-danger btn-block")
      )
    })
    
    # Logout logic
    observeEvent(input$logout_button, {
      credentials$user_auth <- FALSE
      credentials$info <- NULL
      output$error <- renderText("")  # Clear error message
    })
    
    # Return reactive authentication status and info
    reactive({
      list(
        user_auth = credentials$user_auth,
        user_info = credentials$info
      )
    })
  })
}

# 
# # UI for the login module
# 
# # login_module.R
# 
# library(shinyauthr)
# library(sodium)
# 
# # UI for the login module
# custom_login_ui <- function(id, title = "Welcome", subtitle = "Please enter your credentials to continue") {
#   ns <- NS(id)
#   tagList(
#     div(
#       id = ns("login-panel"),
#       class = "panel panel-primary",
#       div(class = "panel-heading", h3(class = "panel-title", title)),
#       div(class = "panel-body",
#           p(subtitle), 
#           textInput(ns("user_name"), "Group Id", placeholder = "Enter group ID"),
#           passwordInput(ns("password"), "Password", placeholder = "Enter password"),
#           div(class = "text-danger", textOutput(ns("error"), inline = TRUE)),
#           actionButton(ns("login_button"), "Log in", class = "btn btn-primary btn-block")
#       )
#     ),
#     # Logout UI
#     uiOutput(ns("logout_ui"))
#   )
# }
# 
# # Server logic for the login module
# 
# custom_login_server <- function(id, user_base) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     credentials <- reactiveValues(user_auth = FALSE, info = NULL)
#     
#     observeEvent(input$login_button, {
#       req(input$user_name, input$password)
#       
#       user <- user_base %>% 
#         filter(user == input$user_name) %>% 
#         slice(1)
#       
#       if (nrow(user) == 1 && sodium::password_verify(user$password, input$password)) {
#         credentials$user_auth <- TRUE
#         credentials$info <- user
#         output$error <- renderText("")
#       } else {
#         output$error <- renderText("Invalid username or password")
#       }
#     })
#     
#     output$logout_ui <- renderUI({
#       req(credentials$user_auth)
#       shinyauthr::logoutUI(id = ns("logout"))
#     })
#     
#     observeEvent(input$logout, {
#       credentials$user_auth <- FALSE
#       credentials$info <- NULL
#       output$error <- renderText("") 
#     })
#     
#     reactive({
#       list(
#         user_auth = credentials$user_auth,
#         user_info = credentials$info
#       )
#     })
#   })
# }
# 
