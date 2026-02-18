# app.R
# Shiny app entrypoint for Draw My Timeline.

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(hms)
library(cowplot)

invisible(lapply(sort(list.files("R", pattern = "\\.R$", full.names = TRUE)), source))
#set up dependencies
logo_src <- "logo.png"
repo_url <- "https://github.com/JZauner/Draw_my_timeline"
issues_url <- paste0(repo_url, "/issues")
by <- "https://orcid.org/0000-0003-2171-4566"

ui <- app_ui(logo_src = logo_src, repo_url = repo_url, issues_url = issues_url, by_url = by)
server <- app_server

shinyApp(ui, server)
