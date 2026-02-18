# app.R
# Shiny app to create timeline plots from Excel sheets or manually entered data.

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(hms)
library(cowplot)


# Return default when x is NULL.
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Convert common color names to hex values for HTML color inputs.
normalize_hex_color <- function(color_name) {
  rgb_matrix <- grDevices::col2rgb(color_name)
  grDevices::rgb(rgb_matrix[1], rgb_matrix[2], rgb_matrix[3], maxColorValue = 255)
}

# Robustly parse time values from hms/POSIX/character/numeric inputs.
parse_time_hms <- function(x) {
  if (inherits(x, "hms")) return(x)
  if (inherits(x, "POSIXt")) return(hms::as_hms(format(x, "%H:%M:%S")))

  if (is.numeric(x)) {
    secs <- round(x * 24 * 60 * 60)
    return(hms::as_hms(secs))
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN", "-", "–")] <- NA_character_
  x_chr <- ifelse(
    !is.na(x_chr) & str_detect(x_chr, "^\\d{1,2}:\\d{2}$"),
    paste0(x_chr, ":00"),
    x_chr
  )

  suppressWarnings(hms::as_hms(x_chr))
}

# Extract numeric values from text such as "505 lx".
parse_numeric_from_text <- function(x) {
  if (is.numeric(x)) return(x)

  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN", "-", "–")] <- NA_character_

  num_chr <- str_extract(x_chr, "[0-9]+([\\.,][0-9]+)?")
  num_chr <- str_replace_all(num_chr, ",", ".")

  as.numeric(num_chr)
}

# Expand support points and time intervals for plotting.
expand_support_points <- function(df,
                                  col_support = "Stützstelle / Lichtszene",
                                  col_begin = "Beginn",
                                  col_end = "Ende") {
  if (!all(c(col_support, col_begin, col_end) %in% names(df))) {
    stop(
      "Expected columns were not found. Required: ",
      col_support, ", ", col_begin, ", ", col_end
    )
  }

  df <- df %>%
    mutate(
      .row_id = row_number(),
      Support = .data[[col_support]],
      Begin = parse_time_hms(.data[[col_begin]]),
      End = parse_time_hms(.data[[col_end]])
    ) %>%
    filter(!is.na(Support)) %>%
    mutate(Support = as.character(Support))

  drop_cols <- c(
    col_support, col_begin, col_end,
    "Zielwert", ".row_id", "Support", "Begin", "End"
  )
  measure_cols <- setdiff(names(df), drop_cols)

  df <- df %>%
    mutate(across(all_of(measure_cols), parse_numeric_from_text))

  begin_sec <- as.numeric(df$Begin)
  end_sec <- as.numeric(df$End)

  out_list <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {
    r <- df[i, , drop = FALSE]

    b <- begin_sec[i]
    e <- end_sec[i]

    t0 <- hms::as_hms(0)
    t_last <- hms::as_hms(23 * 3600 + 59 * 60)

    if (is.na(e)) {
      out_list[[i]] <- bind_rows(
        r %>% mutate(Time = r$Begin, .part = "point")
      )
    } else if (!is.na(b) && !is.na(e) && e == b) {
      # Identical start/end timestamps represent an instantaneous jump.
      out_list[[i]] <- bind_rows(
        r %>% mutate(Time = r$Begin, .part = "jump")
      )
    } else if (!is.na(b) && e > b) {
      out_list[[i]] <- bind_rows(
        r %>% mutate(Time = r$Begin, .part = "from"),
        r %>% mutate(Time = r$End, .part = "to")
      )
    } else if (!is.na(b) && !is.na(e) && e < b) {
      out_list[[i]] <- bind_rows(
        r %>% mutate(Time = t0, .part = "midnight_from"),
        r %>% mutate(Time = r$End, .part = "midnight_to"),
        r %>% mutate(Time = r$Begin, .part = "from"),
        r %>% mutate(Time = t_last, .part = "to")
      )
    } else {
      out_list[[i]] <- bind_rows(
        r %>% mutate(Time = r$Begin, .part = "fallback")
      )
    }
  }

  expanded <- bind_rows(out_list) %>%
    mutate(.time_sec = as.numeric(Time)) %>%
    arrange(.time_sec, .row_id, .part) %>%
    select(-Begin, -End) %>%
    rename(Support = Support)

  list(data = expanded, measure_cols = measure_cols)
}

# Create timeline plot using user-selected colors.
make_timeline_plot <- function(expanded_df, measure_cols,
                               color_map,
                               work1_on = TRUE,
                               work1_start = hms::as_hms("00:00:00"),
                               work1_end = hms::as_hms("07:00:00"),
                               work2_on = TRUE,
                               work2_start = hms::as_hms("20:00:00"),
                               work2_end = hms::as_hms("23:59:00")) {
  long <- expanded_df %>%
    pivot_longer(all_of(measure_cols), names_to = "Measure", values_to = "Value")

  label_y <- mean(range(long$Value, na.rm = TRUE))
  label_df <- expanded_df %>%
    mutate(.grp = cumsum(Support != dplyr::lag(Support, default = first(Support)))) %>%
    group_by(.grp) %>%
    summarise(
      Support = paste0(unique(Support), "."),
      Time = hms::as_hms(mean(as.numeric(Time), na.rm = TRUE)),
      y = label_y,
      .groups = "drop"
    )

  p <- ggplot(long, aes(x = Time, y = Value, color = Measure, group = Measure)) +
    {
      if (work1_on) {
        annotate(
          geom = "rect",
          xmin = as.numeric(work1_start), xmax = as.numeric(work1_end),
          ymin = -Inf, ymax = Inf,
          col = NA, fill = "grey85"
        )
      }
    } +
    {
      if (work2_on) {
        annotate(
          geom = "rect",
          xmin = as.numeric(work2_start), xmax = as.numeric(work2_end),
          ymin = -Inf, ymax = Inf,
          col = NA, fill = "grey85"
        )
      }
    } +
    geom_step(direction = "vh", linewidth = 1.4, na.rm = TRUE) +
    geom_point(size = 3.2, na.rm = TRUE) +
    labs(x = NULL, y = "Measure value (lx)") +
    theme_cowplot(font_size = 16) +
    theme(
      legend.position = "bottom",
      plot.margin = margin(10, 20, 10, 10)
    ) +
    scale_color_manual(values = color_map, breaks = names(color_map)) +
    scale_x_time(
      breaks = seq(0, 24, by = 2) * 3600,
      labels = function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%H:%M")
    ) +
    coord_cartesian(clip = "off", xlim = c(0, 24 * 3600), expand = FALSE) +
    geom_label(
      data = label_df,
      aes(x = Time, y = y, label = Support),
      size = 4.4,
      label.r = unit(0.5, "lines"),
      fontface = "bold",
      inherit.aes = FALSE
    )

  ymax <- suppressWarnings(max(long$Value, na.rm = TRUE))
  if (is.finite(ymax)) {
    p <- p + coord_cartesian(
      clip = "off",
      xlim = c(0, 24 * 3600),
      ylim = c(0, ymax * 1.05),
      expand = FALSE
    )
    p <- p + scale_y_continuous(breaks = pretty(c(0, ymax * 1.05), n = 7))
  }

  p
}

# Shiny module that captures user-defined timeline data in a modal dialog.
manual_timeline_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    manual_data <- reactiveVal(NULL)

    default_csv <- paste(
      "Support,Begin,End,Ambient,Task",
      "Scene A,00:00,07:00,10,500",
      "Scene B,07:00,20:00,50,700",
      "Scene C,20:00,23:59,10,500",
      sep = "\n"
    )

    observeEvent(input$open_modal, {
      showModal(modalDialog(
        title = "Create a custom timeline",
        textInput(session$ns("manual_project"), "Project name", value = "Manual timeline"),
        tags$p(
          "Provide comma-separated values with at least the columns:",
          tags$code("Support, Begin, End"),
          ". Additional numeric columns are plotted as measures."
        ),
        textAreaInput(
          session$ns("manual_csv"),
          label = "Timeline table (CSV format)",
          value = default_csv,
          rows = 12,
          width = "100%"
        ),
        footer = tagList(
          actionButton(session$ns("load_example"), "Load example"),
          modalButton("Cancel"),
          actionButton(session$ns("save_manual"), "Use timeline", class = "btn-primary")
        ),
        easyClose = TRUE,
        size = "l"
      ))
    })

    observeEvent(input$load_example, {
      updateTextAreaInput(session, "manual_csv", value = default_csv)
    })

    observeEvent(input$save_manual, {
      csv_text <- input$manual_csv
      parsed <- tryCatch(
        read.csv(text = csv_text, check.names = FALSE, stringsAsFactors = FALSE),
        error = function(e) e
      )

      if (inherits(parsed, "error")) {
        showNotification("Could not parse CSV text. Please check separators and headers.", type = "error")
        return()
      }

      required_cols <- c("Support", "Begin", "End")
      if (!all(required_cols %in% names(parsed))) {
        showNotification("CSV must contain columns: Support, Begin, End.", type = "error")
        return()
      }

      prepared <- tryCatch(
        expand_support_points(
          parsed,
          col_support = "Support",
          col_begin = "Begin",
          col_end = "End"
        ),
        error = function(e) e
      )

      if (inherits(prepared, "error")) {
        showNotification(prepared$message, type = "error")
        return()
      }

      manual_data(list(
        project_name = input$manual_project,
        raw = parsed,
        prepared = prepared
      ))
      removeModal()
      showNotification("Custom timeline loaded.", type = "message")
    })

    manual_data
  })
}

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Draw My Timeline",
  layout_sidebar(
    sidebar = sidebar(
      width = 360,
      card(
        card_header(tags$strong("Data source")),
        card_body(
          div(
            style = "display:flex; align-items:center; gap:12px; margin-bottom: 10px;",
            tags$img(src = "logo.svg", alt = "Application logo", style = "height:48px; width:auto;"),
            tags$div(tags$strong("Draw My Timeline"))
          ),
          radioButtons(
            "data_source",
            "Use data from",
            choices = c("Excel file" = "excel", "Custom timeline" = "manual"),
            selected = "excel"
          ),
          conditionalPanel(
            condition = "input.data_source === 'excel'",
            fileInput("xlsx", "Choose Excel file", accept = c(".xlsx")),
            uiOutput("sheet_ui")
          ),
          actionButton("open_manual_modal", "Create custom timeline", class = "btn-secondary"),
          br(), br(),
          downloadButton("download_example", "Download example Excel"),
          br(), br(),
          textInput("project_name", "Project name", value = "")
        )
      ),
      card(
        card_header(tags$strong("Plot settings")),
        card_body(
          checkboxInput("work1_on", "Show work block 1", value = TRUE),
          fluidRow(
            column(6, textInput("work1_start", "Start 1 (HH:MM)", value = "00:00")),
            column(6, textInput("work1_end", "End 1 (HH:MM)", value = "07:00"))
          ),
          checkboxInput("work2_on", "Show work block 2", value = TRUE),
          fluidRow(
            column(6, textInput("work2_start", "Start 2 (HH:MM)", value = "20:00")),
            column(6, textInput("work2_end", "End 2 (HH:MM)", value = "23:59"))
          ),
          tags$hr(),
          uiOutput("color_ui")
        )
      ),
      card(
        card_header(tags$strong("PDF export")),
        card_body(
          numericInput("pdf_w", "Width (inch)", value = 15, min = 3, step = 0.5),
          numericInput("pdf_h", "Height (inch)", value = 5, min = 2, step = 0.5),
          downloadButton("dl_pdf", "Download plot as PDF")
        )
      )
    ),
    card(
      card_header(tags$strong("Timeline preview")),
      card_body(
        tags$p(
          tags$strong("How to use:"),
          " Upload an Excel file and choose a sheet, or click 'Create custom timeline' to enter CSV data manually.",
          " Then adjust work blocks and colors, and export the result as PDF."
        ),
        plotOutput("plot", height = "560px"),
        verbatimTextOutput("status")
      )
    )
  )
)

server <- function(input, output, session) {
  manual_data <- manual_timeline_server("manual")

  xlsx_path <- reactive({
    req(input$xlsx)
    input$xlsx$datapath
  })

  sheets <- reactive({
    req(xlsx_path())
    excel_sheets(xlsx_path())
  })

  output$sheet_ui <- renderUI({
    req(sheets())
    selectInput("sheet", "Project (Excel sheet)", choices = sheets(), selected = sheets()[1])
  })

  observeEvent(input$sheet, {
    if (isTRUE(input$data_source == "excel") && !is.null(input$sheet)) {
      updateTextInput(session, "project_name", value = input$sheet)
    }
  }, ignoreInit = FALSE)

  observeEvent(manual_data(), {
    md <- manual_data()
    if (!is.null(md)) {
      updateTextInput(session, "project_name", value = md$project_name)
    }
  })

  excel_prepared <- reactive({
    req(input$data_source == "excel", xlsx_path(), input$sheet)
    df <- read_excel(xlsx_path(), sheet = input$sheet)
    expand_support_points(df)
  })

  active_prepared <- reactive({
    if (identical(input$data_source, "manual")) {
      req(manual_data())
      manual_data()$prepared
    } else {
      excel_prepared()
    }
  })

  output$color_ui <- renderUI({
    prep <- active_prepared()
    req(prep$measure_cols)

    default_palette <- c(normalize_hex_color("gold1"), normalize_hex_color("#1D63DC"))
    defaults <- rep(default_palette, length.out = length(prep$measure_cols))

    tagList(
      h5("Measure colors"),
      lapply(seq_along(prep$measure_cols), function(i) {
        measure <- prep$measure_cols[i]
        input_id <- paste0("col_", make.names(measure))
        div(
          style = "margin-bottom:8px;",
          tags$label(`for` = input_id, paste0(measure, ":")),
          tags$input(
            id = input_id,
            type = "color",
            value = defaults[i],
            style = "margin-left:8px; width:48px; height:32px; vertical-align:middle;"
          )
        )
      })
    )
  })

  plot_obj <- reactive({
    prep <- active_prepared()

    w1s <- parse_time_hms(input$work1_start)
    w1e <- parse_time_hms(input$work1_end)
    w2s <- parse_time_hms(input$work2_start)
    w2e <- parse_time_hms(input$work2_end)

    color_map <- stats::setNames(
      vapply(prep$measure_cols, function(measure) {
        input[[paste0("col_", make.names(measure))]] %||% normalize_hex_color("gold1")
      }, FUN.VALUE = character(1)),
      prep$measure_cols
    )

    make_timeline_plot(
      expanded_df = prep$data,
      measure_cols = prep$measure_cols,
      color_map = color_map,
      work1_on = isTRUE(input$work1_on) && !is.na(w1s) && !is.na(w1e),
      work1_start = w1s,
      work1_end = w1e,
      work2_on = isTRUE(input$work2_on) && !is.na(w2s) && !is.na(w2e),
      work2_start = w2s,
      work2_end = w2e
    )
  })

  output$plot <- renderPlot({
    req(plot_obj())
    plot_obj()
  }, res = 120)

  output$status <- renderText({
    prep <- active_prepared()
    source_name <- if (identical(input$data_source, "manual")) "Manual timeline" else paste("Sheet", input$sheet)

    paste0(
      "Source: ", source_name, "\n",
      "Rows (expanded): ", nrow(prep$data), "\n",
      "Measures: ", paste(prep$measure_cols, collapse = ", ")
    )
  })

  output$dl_pdf <- downloadHandler(
    filename = function() {
      proj <- trimws(input$project_name)
      if (proj == "") proj <- if (identical(input$data_source, "manual")) "manual_timeline" else input$sheet
      proj <- str_replace_all(proj, "[^A-Za-z0-9_\\-]+", "_")
      paste0(proj, "_timelinePlot.pdf")
    },
    content = function(file) {
      req(plot_obj())
      ggsave(
        filename = file,
        plot = plot_obj(),
        width = input$pdf_w,
        height = input$pdf_h,
        dpi = 300
      )
    }
  )

  output$download_example <- downloadHandler(
    filename = function() "Examples.xlsx",
    content = function(file) {
      file.copy("Examples.xlsx", file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
