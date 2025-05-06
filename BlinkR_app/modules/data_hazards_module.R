
bucketListModuleUI <- function(id, i18n) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
    ),
    fluidRow(
      column(
        width = 12,
        tags$b(i18n$t("Select the appropriate data hazards for this experiment")),
        tags$br(),
        tags$small(i18n$t("Drag the hazards from the list to the box on the right to select them for this experiment.")),
        bucket_list(
          header = NULL,
          group_name = ns("bucket_list_group"),
          orientation = "horizontal",
          
          add_rank_list(
            text = "❓",
            labels = list(
              "automates-decision-making" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/automates-decision-making.png", height = "50px"),
                htmltools::tags$b("Automates Decision Making"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`automates-decision-making`)
              ),
              "classifies-people" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/classifies-people.png", height = "50px"),
                htmltools::tags$b("Ranks or Classifies People"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`classifies-people`)
              ),
              "difficult-to-understand" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/difficult-to-understand.png", height = "50px"),
                htmltools::tags$b("Difficult To Understand"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`difficult-to-understand`)
              ),
              "direct-harm" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/direct-harm.png", height = "50px"),
                htmltools::tags$b("May Cause Direct Harm"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`direct-harm`)
              ),
              "ecological-harm" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/ecological-harm.png", height = "50px"),
                htmltools::tags$b("Synthetic Biology: Capable of Ecological Harm"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`ecological-harm`)
              ),
              "environment" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/environment.png", height = "50px"),
                htmltools::tags$b("High Environment Cost"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`environment`)
              ),
              "experimental-hazard" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/experimental-hazard.png", height = "50px"),
                htmltools::tags$b("Synthetic Biology: Potential Experimental Hazard"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`experimental-hazard`)
              ),
              "general-hazard" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/general-hazard.png", height = "50px"),
                htmltools::tags$b("General Hazard"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`general-hazard`)
              ),
              "incompatible-data" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/incompatible-data.png", height = "50px"),
                htmltools::tags$b("Synthetic Biology: Integration of Incompatible Data"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`incompatible-data`)
              ),
              "lacks-community" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/lacks-community.png", height = "50px"),
                htmltools::tags$b("Lacks Community Involvement"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`lacks-community`)
              ),
              "lacks-informed-consent" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/lacks-informed-consent.png", height = "50px"),
                htmltools::tags$b("Lacks Informed Consent"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`lacks-informed-consent`)
              ),
              "misuse" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/misuse.png", height = "50px"),
                htmltools::tags$b("Danger of Misuse"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`misuse`)
              ),
              "privacy" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/privacy.png", height = "50px"),
                htmltools::tags$b("Risk To Privacy"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`privacy`)
              ),
              "reinforce-bias" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/reinforce-bias.png", height = "50px"),
                htmltools::tags$b("Reinforces Existing Biases"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`reinforce-bias`)
              ),
              "uncertain-accuracy" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/uncertain-accuracy.png", height = "50px"),
                htmltools::tags$b("Synthetic Biology: Uncertain Accuracy of Source Data"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`uncertain-accuracy`)
              ),
              "uncertain-completeness" = htmltools::tags$div(
                htmltools::tags$img(src = "hazards/uncertain-completeness.png", height = "50px"),
                htmltools::tags$b("Synthetic Biology: Uncertain Completeness of Source Data"),
                htmltools::tags$br(),
                htmltools::tags$small(data_hazards_list$`uncertain-completeness`)
              )
            ),
            input_id = ns("rank_list_1")
          ),
          
          add_rank_list(
            text = "✅",
            labels = NULL,
            input_id = ns("rank_list_2")
          )
        ),
        div(
        style = "text-align: center;",
          actionButton(
          inputId = ns("submit_hazards"),
          label   = i18n$t("Submit Hazards"),
          class   = "fun-submit-button"
        )
        ),
        
      )
    ),
  )
}

bucketListModuleServer <- function(id, i18n, auth, session_folder_id) {
  moduleServer(id, function(input, output, session) {

    output$results_1 <- renderPrint(input$rank_list_1)
    output$results_2 <- renderPrint(input$rank_list_2)
    output$results_3 <- renderPrint(input$bucket_list_group)
    

    observeEvent(input$submit_hazards, {
         observeEvent(input$submit_hazards, {
      if (length(input$rank_list_2) == 0) {
        showNotification(i18n$t("Error: Please Select Data Hazards"), type = "error")
      } else {
        showNotification(i18n$t("Hazards submitted!"), type = "message")
        
        out_file <- file.path(tempdir(), "selected_hazards.pdf")
       pdf(out_file, width = 21/2.54, height = 29.7/2.54)

        image_list <- list()
        for (hazard in input$rank_list_2) {
          img_path <- file.path("www", "hazards", paste0(hazard, ".png"))
          image_list[[hazard]] <- rasterGrob(
            readPNG(img_path)
        )
        }
        
        num_images <- min(max(length(image_list), 1), 16)
    
    num_cols <- 2
    num_rows <- ceiling(num_images / num_cols)

title_grob <- textGrob(
      i18n$t("Data Hazards For This Project"),
      gp = gpar(fontsize = 14, fontface = "bold")
    )

    grid.arrange(
      title_grob,
      arrangeGrob(
        grobs = image_list, 
        ncol = num_cols, 
        nrow = num_rows, 
        padding = unit(2, "cm")
      ),
      ncol = 1, 
      heights = c(0.15, 0.85)
    )       
        dev.off()
        
        path <- drive_get(as_id(session_folder_id))

        drive_upload(
          media = out_file,
          path = path,
          name = "selected_hazards.pdf",
          overwrite = TRUE
        )

      }
    })
    })
  })
}
