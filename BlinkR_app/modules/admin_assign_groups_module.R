assign_groups_admin_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("refresh"), tagList(icon("arrows-rotate"),"Refresh"), class = "btn-primary"),
    div(style = "margin-top: 15px;",
    uiOutput(ns("assign_list"))
    )
  )
}

assign_groups_admin_module_server <- function(id, user_base_sheet_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    users_rv <- reactiveVal(NULL)

    read_users <- function() {
      tryCatch(
        googlesheets4::read_sheet(user_base_sheet_id),
        error = function(e) {
          showNotification(paste("Could not read sheet:", e$message), type = "error", duration = 5)
          NULL
        }
      )
    }

    observeEvent(TRUE, { users_rv(read_users()) }, once = TRUE)
    observeEvent(input$refresh, { users_rv(read_users()) }, ignoreInit = TRUE)

    unassigned_ids <- reactive({
      df <- users_rv(); req(!is.null(df))
      nm <- df$Name
      empty <- is.na(nm) | !nzchar(trimws(nm))
      as.character(df$group[empty])
    })

output$assign_list <- renderUI({
  ids <- unassigned_ids()
  req(length(ids) > 0)

  tagList(lapply(ids, function(id) {
    name_input_id <- paste0("assign_name_", id)
    submit_btn_id <- paste0("assign_ok_", id)

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
          placeholder = "Enter group member name..."
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
            title = "Assign this ID"
          )
        )
      )
    )
  }))
})

# Track which IDs already have observers
created_obs <- reactiveVal(character(0))

observe({
  ids <- unassigned_ids()
  new_ids <- setdiff(ids, created_obs())
  if (!length(new_ids)) return()

  lapply(new_ids, function(gid) {
    local({
      gid_ <- as.character(gid)
      btn_id  <- ns(paste0("assign_ok_",  gid_))
      name_id <- ns(paste0("assign_name_", gid_))

      observeEvent(input[[paste0("assign_ok_", gid_)]], ignoreInit = TRUE, {
        nm <- trimws(input[[paste0("assign_name_", gid_)]])
        if (!nzchar(nm)) {
          showNotification(paste("Please enter a name for ID", gid_), type = "error")
          return()
        }

        # Current snapshot of the sheet
        df <- users_rv(); req(!is.null(df))

        # Locate the row for this group (coerce both sides to character for safety)
        row_idx <- which(as.character(df$group) == gid_)[1]
        if (is.na(row_idx)) {
          showNotification(paste("Group", gid_, "not found."), type = "error")
          return()
        }

        # Safety: ensure still unassigned
        if (!is.na(df$Name[row_idx]) && nzchar(trimws(df$Name[row_idx]))) {
          showNotification(paste("Group", gid_, "already has a name."), type = "warning")
          return()
        }

        # Write to Google Sheet: "Name" is column C -> row index + 1 (header in row 1)
        cell_a1 <- paste0("C", row_idx + 1)

        ok <- tryCatch({
          googlesheets4::range_write(
            ss        = user_base_sheet_id,
            data      = data.frame(nm, stringsAsFactors = FALSE),
            range     = cell_a1,
            col_names = FALSE
          )
          TRUE
        }, error = function(e) {
          showNotification(paste("Write failed:", e$message), type = "error", duration = 6)
          FALSE
        })

        if (!ok) return()

        # Update local copy (no re-read needed)
        df$Name[row_idx] <- nm
        users_rv(df)

        # Disable this row (optional, requires shinyjs in UI)
        if (requireNamespace("shinyjs", quietly = TRUE)) {
          shinyjs::disable(name_id)
          shinyjs::disable(btn_id)
        }

        showNotification(sprintf("Assigned %s → %s", gid_, nm), type = "message", duration = 3)
      })
    })
  })

  created_obs(unique(c(created_obs(), new_ids)))
})


    return(list(unassigned_ids = unassigned_ids))
  })
}