required_packages <- c(
  "shiny",
  "bslib",
  "shinydashboard",
  "markdown",
  "DT",
  "shinyAce",
  "shinyauthr",
  "dplyr",
  "ggplot2",
  "car",
  "tidyr",
  "utils",
  "googlesheets4",
  "googledrive",
  "readr",
  "markdownInput",
  "knitr",
  "shinycssloaders",
  "future",
  "promises",
  "shinyWidgets",
  "tibble",
  "stringr",
  "shinyjs",
  "jsonlite",
  "datasets",
  "evaluate",
  "rstatix",
  "coin",
  "rsconnect",
  "cookies",
  "whisker",
  "sortable",
  "grid",
  "png",
  "gridExtra",
  "stats",
  "shiny.i18n",
  "htmltools",
  "stats",
  "lubridate",
  "shinyvalidate",
  "digest",
  "mailtoR"
)

installed_packages <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed_packages)

if (length(missing_packages)) {
  install.packages(missing_packages)
}

lapply(required_packages, library, character.only = TRUE)