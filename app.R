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

logo_src <- if (file.exists("www/logo.png")) "logo.png" else "logo.svg"
repo_url <- "https://github.com/JZauner/Draw_my_timeline"
issues_url <- paste0(repo_url, "/issues")

ui <- app_ui(logo_src = logo_src, repo_url = repo_url, issues_url = issues_url)
server <- app_server

shinyApp(ui, server)
