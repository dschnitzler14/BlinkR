view_groups_admin_module_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns("refresh_table"),
      tagList(icon("arrows-rotate"), "Refresh"),
      class = "btn-primary"
    ),
    div(style = "margin-top: 15px;",
        DT::DTOutput(ns("user_table"))
    )
  )
}

view_groups_admin_module_server <- function(id, user_base_sheet_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    user_base_data_reactive <- reactiveVal(NULL)

    read_user_base <- function() {
      tryCatch(
        googlesheets4::read_sheet(user_base_sheet_id),
        error = function(e) {
          showNotification(paste("Could not read sheet:", e$message),
                           type = "error", duration = 5)
          NULL
        }
      )
    }

    prep_df <- function(df) {
      if (is.null(df)) return(NULL)
      if ("Date" %in% names(df)) {
        df$Date <- as.Date(df$Date, format = "%d/%m/%y")
      }
      df
    }

    observeEvent(TRUE, {
      df0 <- prep_df(read_user_base())
      user_base_data_reactive(df0)
    }, once = TRUE)

    output$user_table <- DT::renderDT({
      df <- user_base_data_reactive()
      req(!is.null(df))
      DT::datatable(
        df,
        editable = FALSE,
        rownames = TRUE,
        options = list(
          pageLength = 10,
          order = list(list(which(names(df) == "Date") - 1, "desc"))
        )
      )
    })

    proxy <- DT::dataTableProxy("user_table", session = session)

    observeEvent(input$refresh_table, ignoreInit = TRUE, {
      df_new <- prep_df(read_user_base())
      req(!is.null(df_new))

      df_cur <- user_base_data_reactive()

      if (isTRUE(all.equal(df_cur, df_new))) {
        DT::replaceData(proxy, df_new, resetPaging = FALSE, rownames = TRUE)
      } else {
        user_base_data_reactive(df_new)
      }
    })
  })
}
