create_groups_admin_module_ui <- function(id, i18n) {
  ns <- NS(id)
    tagList(
    numericInput(
      ns("num_groups"),
      label = i18n$t("Number of New Groups to Create:"),
      value = 1,
      min = 1,
      max = 30,
      step = 1),
      actionButton(
        ns("create_new_groups"),
        label = tagList(icon("user-plus"), i18n$t("Create New Group")),
        class = "btn-primary"
      ),
      uiOutput(ns("new_groups_ui"))
    )
}


create_groups_admin_module_server <- function(id, user_base_sheet_id, base_group_files_url) {
  moduleServer(id, function(input, output, session) {

  ### helper ---
create_group_folder <- function(group_id,
                                parent_folder_name = "BlinkR_text_results",
                                base_group_files_url_name = base_group_files_url) {
  parent <- googledrive::drive_get(parent_folder_name)
  if (nrow(parent) == 0) {
    parent <- googledrive::drive_mkdir(parent_folder_name)
  }

  existing <- googledrive::drive_ls(googledrive::as_id(parent$id), pattern = paste0("^", group_id, "$"))
  if (nrow(existing) > 0) {
    folder <- existing[1, ]
  } else {
    folder <- googledrive::drive_mkdir(
      name = as.character(group_id),
      path = googledrive::as_id(parent$id)
    )
    googledrive::drive_share_anyone(googledrive::as_id(folder$id))
  }

  list(
    id  = folder$id,
    url = paste0(base_group_files_url_name, folder$id)
  )
}
  ### ---
    vars <- get_experiment_vars()


    user_base <- reactiveVal()

    observe({
      user_base(read_sheet(user_base_sheet_id))
    })

    ns <- session$ns

    new_group_ids_rv <- reactiveVal(NULL)

observeEvent(input$create_new_groups, ignoreInit = TRUE, {
  req(input$num_groups)
  req(user_base())

  existing_group_ids <- as.integer(user_base()$group)

  n <- suppressWarnings(as.integer(input$num_groups))
  if (is.na(n) || n < 1) {
    showNotification("Enter a positive whole number for groups.", type = "error")
    return()
  }
  if (n > 9000) {
    showNotification("Cannot create more than 9000 unique 4-digit IDs.", type = "error")
    return()
  }

   possible_ids <- setdiff(1000:9999, existing_group_ids)

  if (length(possible_ids) < n) {
    showNotification(
      sprintf("Only %d unused IDs remain; cannot create %d new groups.", length(possible_ids), n),
      type = "error"
    )
    return()
  }

  new_group_ids <- sample(possible_ids, size = n, replace = FALSE)

  new_group_ids_rv(new_group_ids)

  showNotification(
    paste("Generated IDs:", paste(new_group_ids, collapse = ", ")),
    type = "message", duration = 3
  )
    
    }
    )

  submitted_names <- reactiveValues()

  output$new_groups_ui <- renderUI({
  ids <- new_group_ids_rv()
  req(length(ids) > 0)

  tagList(lapply(ids, function(id) {
    name_input_id  <- paste0("group_name_", id)
    submit_btn_id  <- paste0("submit_group_", id)
    row_id         <- paste0("row_", id)

    tags$div(id = ns(row_id),
      fluidRow(
        class = "align-items-center mb-2",
        column(
          width = 3,
          tags$div(
            style = "padding:6px 10px;border-radius:8px;background:#f1f3f5;display:inline-block;font-weight:600;",
            sprintf("ID: %s", id)
          )
        ),
        column(
          width = 6,
          textInput(
            inputId = ns(name_input_id),
            label = NULL,
            placeholder = "Enter name of one of the group members..."
          )
        ),
        column(
          width = 3,
          div(
            style = "margin-top: 4px;",
            actionButton(
              inputId = ns(submit_btn_id),
              label = NULL,
              icon = icon("check"),
              class = "btn-success",
              title = "Save this Group ID"
            )
          )
        )
      )
    )
  }))
})



###
observers_created_for <- reactiveVal(integer(0))

observeEvent(new_group_ids_rv(), ignoreInit = FALSE, {
  ids <- new_group_ids_rv()
  already <- observers_created_for()
  to_make <- setdiff(ids, already)
  if (!length(to_make)) return()

  cat("ids", ids, "\n")
  cat("already", already, "\n")
  cat("to_make", to_make, "\n")

  lapply(to_make, function(id) {
    local({
      id_ <- id
      btn_id  <- paste0("submit_group_", id_)
      name_id <- paste0("group_name_", id_)

      observeEvent(input[[btn_id]], ignoreInit = TRUE, {
        nm <- input[[name_id]]
        # if (is.null(nm) || !nzchar(trimws(nm))) {
        #   showNotification(sprintf("Please enter a name for ID %s.", id_), type = "error")
        #   return()
        # }

        cat("button for", id_, "clicked; name =", nm, "\n")

        if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::disable(ns(btn_id))

        new_row <- data.frame(
          group    = as.character(id_),    
          Role     = "group",
          Name     = trimws(nm),
          Date     = format(Sys.Date(), "%d/%m/%y"),
          Protocol = "FALSE",
          Data     = "FALSE",
          stringsAsFactors = FALSE
        )

        tryCatch({
        googlesheets4::sheet_append(user_base_sheet_id, new_row)

         updated <- googlesheets4::read_sheet(user_base_sheet_id)
          user_base(updated)

             if (requireNamespace("shinyjs", quietly = TRUE)) {
                shinyjs::hide(ns(paste0("row_", id_)))  # hide the row wrapper
              }

          new_group_ids_rv(isolate(setdiff(new_group_ids_rv(), id_)))

          showNotification(sprintf("Saved group %s (%s).", id_, nm),
                          type = "message", duration = 3)
        }, error = function(e) {
          showNotification(paste("Error saving group", id_, ":", e$message),
                          type = "error", duration = 5)
          if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::enable(ns(btn_id))
          return()
        })


        tryCatch({
                
                res <- create_group_folder(id_)


                }, error = function(e) {
                  showNotification(paste("Error saving group", id_, ":", e$message),
                                  type = "error", duration = 5)
                })

              })
            })
          })

  observers_created_for(unique(c(already, to_make)))
})
  
  })}