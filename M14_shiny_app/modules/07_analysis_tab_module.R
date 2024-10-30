analysis_module_ui <- function(id) {
  ns <- NS(id)
  analysis_tab <- tabItem(tabName = "Analysis",
                          fluidPage(
                            fluidRow(
                            column(6,
                            ),
                            column(6, aceEditor("analysis_tab",
                                                mode = "r",
                                                theme = "solarized_dark",
                                                value = "#type your code in here!",
                                                height = "200px",
                                                fontSize = 14,
                                                showLineNumbers = TRUE,
                                                autoComplete = "live",
                                                autoCompleters = c("rlang", "text", "snippet", "keyword"),
                                                showPrintMargin = FALSE,
                                                setBehavioursEnabled = TRUE,
                            ),)
                            )
                            
                          )
  )
}

analysis_module_server <- function(id){
  moduleServer(
    id,
    function(input, output, server){
      #function logic here
    }
  )
}
