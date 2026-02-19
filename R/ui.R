# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

app_ui <- function(logo_src, repo_url, issues_url, by_url) {
  bslib::page_fillable(
    theme = bslib::bs_theme(version = 5, bootswatch = "cosmo"),
    title = "Draw My Timeline",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 360,
        shiny::div(
          style = "display:flex; justify-content:center;",
          shiny::tags$img(src = logo_src, alt = "Application logo", style = "height:220px; width:auto;")
        ),
        bslib::card(
          bslib::card_header(shiny::tags$strong("About")),
          bslib::card_body(
            shiny::tags$p(
              shiny::icon("user"),
              "Created by:",
              shiny::tags$a(
                href = by_url,
                target = "_blank",
                " Johannes Zauner"
              )
            ),
            shiny::tags$p("License: ",
                          shiny::tags$a(
                            href = "https://interoperable-europe.ec.europa.eu/licence/mit-license",
                            target = "_blank",
                            " MIT"
                          )
            ),
            shiny::tags$p(
              shiny::tags$a(
                href = repo_url,
                .noWS = "after",
                target = "_blank",
                shiny::icon("github"),
                " GitHub repository"
              ), " or ",
              shiny::tags$a(
                href = issues_url,
                target = "_blank",
                shiny::icon("bug"),
                " Report a bug"
              )
            )
          )
        ),
        bslib::card(
          bslib::card_header(shiny::tags$strong("Data source")),
          bslib::card_body(
            shiny::radioButtons(
              "data_source",
              "Use data from",
              choices = c("Excel file" = "excel", "Custom timeline" = "manual"),
              selected = "excel"
            ),
            shiny::conditionalPanel(
              condition = "input.data_source === 'excel'",
              shiny::fileInput("xlsx", "Choose Excel file", accept = c(".xlsx")),
              shiny::uiOutput("sheet_ui"),
              shiny::div(
                style = "margin-top:8px;",
                downloadButton("download_example", "Download example Excel", style = "width:100%;")
              )
            ),
            shiny::conditionalPanel(
              condition = "input.data_source === 'manual'",
              shiny::div(style = "margin-top:8px;", shiny::actionButton("manual-open_modal", "Create custom timeline", class = "btn-secondary", width = "100%"))
            ),
            shiny::div(style = "margin-top:12px;", shiny::textInput("project_name", "Project name", value = ""))
          )
        ),
        bslib::card(
          bslib::card_header(shiny::tags$strong("Plot settings")),
          bslib::card_body(
            shiny::checkboxInput("work1_on", "Show block 1", value = TRUE),
            shiny::fluidRow(
              shiny::column(6, shiny::textInput("work1_start", "Start 1 (HH:MM)", value = "00:00")),
              shiny::column(6, shiny::textInput("work1_end", "End 1 (HH:MM)", value = "07:00"))
            ),
            shiny::checkboxInput("work2_on", "Show block 2", value = TRUE),
            shiny::fluidRow(
              shiny::column(6, shiny::textInput("work2_start", "Start 2 (HH:MM)", value = "20:00")),
              shiny::column(6, shiny::textInput("work2_end", "End 2 (HH:MM)", value = "23:59"))
            ),
            shiny::tags$hr(),
            shiny::textInput("y_axis_label", "Y-axis label", value = "Measure value (lx)"),
            shiny::fluidRow(
              shiny::column(6, shiny::numericInput("y_axis_min", "Y-axis min", value = NA_real_)),
              shiny::column(6, shiny::numericInput("y_axis_max", "Y-axis max", value = NA_real_))
            ),
            shiny::numericInput(
              "scene_label_height",
              "Scene label height (optional)",
              value = NA_real_,
              step = 1
            ),
            shiny::textInput("legend_title", "Legend title", value = "Measure"),
            shiny::radioButtons(
              "line_geom",
              "Scene transition",
              choices = c("Step changes" = "step", "Gliding changes" = "path"),
              selected = "path",
              inline = TRUE
            ),
            shiny::tags$hr(),
            shiny::uiOutput("color_ui")
          )
        ),
        bslib::card(
          bslib::card_header(shiny::tags$strong("PDF export")),
          bslib::card_body(
            shiny::numericInput("pdf_w", "Width (inch)", value = 15, min = 3, step = 0.5),
            shiny::numericInput("pdf_h", "Height (inch)", value = 5, min = 2, step = 0.5),
            downloadButton("dl_pdf", "Download plot as PDF"),
            downloadButton("dl_png", "Download plot as PNG")
          )
        ),
      ),
      bslib::card(
        bslib::card_header(shiny::tags$strong("Timeline preview")),
        bslib::card_body(
          shiny::tags$p(
            shiny::tags$strong("How to use:"),
            " Upload an Excel file and choose a sheet, or click 'Create custom timeline' to enter CSV data manually.",
            " Then adjust work blocks and colors, and export the result as PDF.",
            " This app runs 100% in your browser. I.e., all data remain on your device."
          ),
          shiny::plotOutput("plot", height = "560px"),
          shiny::verbatimTextOutput("status")
        )
      )
    )
  )
}

color_inputs_ui <- function(measure_cols) {
  default_palette <- c(normalize_hex_color("gold1"), normalize_hex_color("#1D63DC"))
  defaults <- rep(default_palette, length.out = length(measure_cols))

  shiny::tagList(
    shiny::h5("Measure colors"),
    lapply(seq_along(measure_cols), function(i) {
      measure <- measure_cols[i]
      input_id <- paste0("col_", i)
      shiny::div(
        style = "margin-bottom:8px;",
        shiny::tags$label(`for` = input_id, paste0(measure, ":")),
        shiny::tags$input(
          id = input_id,
          type = "color",
          value = defaults[i],
          oninput = "Shiny.setInputValue(this.id, this.value, {priority: 'event'})",
          style = "margin-left:8px; width:48px; height:32px; vertical-align:middle;"
        )
      )
    })
  )
}
