# app.R
# Shiny-App: Timeline-Plot aus Excel (jede Arbeitsmappe = Projekt/Sheet)

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(hms)
library(cowplot)

# -----------------------------
# Helper: Zeit robust parsen
# -----------------------------
parse_time_hms <- function(x) {
  # akzeptiert: hms, POSIXt, character ("HH:MM" / "HH:MM:SS"), numeric (Excel Tagesanteil)
  if (inherits(x, "hms")) return(x)
  if (inherits(x, "POSIXt")) return(hms::as_hms(format(x, "%H:%M:%S")))
  
  if (is.numeric(x)) {
    # Excel speichert Zeit oft als Anteil eines Tages (0..1)
    secs <- round(x * 24 * 60 * 60)
    return(hms::as_hms(secs))
  }
  
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN", "-", "–")] <- NA_character_
  # erlaubt HH:MM oder HH:MM:SS
  x_chr <- ifelse(!is.na(x_chr) & str_detect(x_chr, "^\\d{1,2}:\\d{2}$"),
                  paste0(x_chr, ":00"),
                  x_chr)
  h <- suppressWarnings(hms::as_hms(x_chr))
  return(h)
}

# -----------------------------
# Helper: numeric aus "505 lx" etc.
# -----------------------------
parse_numeric_from_text <- function(x) {
  if (is.numeric(x)) return(x)
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN", "-", "–")] <- NA_character_
  # erste Zahl (inkl. Dezimal mit Komma oder Punkt)
  num_chr <- str_extract(x_chr, "[0-9]+([\\.,][0-9]+)?")
  num_chr <- str_replace_all(num_chr, ",", ".")
  as.numeric(num_chr)
}

# -----------------------------
# Daten je Sheet aufbereiten
# - Beginn/Ende -> Uhrzeit-Stützpunkte
# - Midnight-Split
# -----------------------------
expand_support_points <- function(df,
                                  col_support = "Stützstelle / Lichtszene",
                                  col_begin = "Beginn",
                                  col_end   = "Ende") {
  
  if (!all(c(col_support, col_begin, col_end) %in% names(df))) {
    stop("Erwartete Spalten nicht gefunden. Benötigt: ",
         col_support, ", ", col_begin, ", ", col_end)
  }
  # browser()
  df <- df %>%
    mutate(
      .row_id = row_number(),
      Support = .data[[col_support]],
      Beginn  = parse_time_hms(.data[[col_begin]]),
      Ende    = parse_time_hms(.data[[col_end]])
    ) %>%
    filter(!is.na(Support)) %>%
    mutate(Support = as.character(Support))
  
  # Messspalten: alles außer Support/Beginn/Ende/Zielwert/row_id
  drop_cols <- c(col_support, col_begin, col_end, "Zielwert", ".row_id", "Support", "Beginn", "Ende")
  measure_cols <- setdiff(names(df), drop_cols)
  
  # Messwerte numerisch machen
  df <- df %>%
    mutate(across(all_of(measure_cols), parse_numeric_from_text))
  
  # Zeit in Sekunden für Vergleiche
  begin_sec <- as.numeric(df$Beginn)
  end_sec   <- as.numeric(df$Ende)
  
  # Fälle:
  # A) Ende NA -> Punkt (nur Beginn)
  # B) Ende >= Beginn -> Intervall: Beginn + Ende
  # C) Ende < Beginn -> über Mitternacht: (00:00 + Ende) UND (Beginn + 23:59)
  out_list <- vector("list", nrow(df))
  
  for (i in seq_len(nrow(df))) {
    r <- df[i, , drop = FALSE]
    
    b <- begin_sec[i]
    e <- end_sec[i]
    
    # 23:59 als letzter Punkt, analog Beispielskript (23.95h ~ 23:57; wir nehmen 23:59)
    t0 <- hms::as_hms(0)
    t_last <- hms::as_hms(23*3600 + 59*60)
    
    if (is.na(e)) {
      # Punkt
      out_list[[i]] <- bind_rows(
        r %>% mutate(Uhrzeit = r$Beginn, .part = "point")
      )
    } else if (!is.na(b) && e >= b) {
      # Intervall normal
      out_list[[i]] <- bind_rows(
        r %>% mutate(Uhrzeit = r$Beginn, .part = "from"),
        r %>% mutate(Uhrzeit = r$Ende,   .part = "to")
      )
    } else if (!is.na(b) && !is.na(e) && e < b) {
      # Über Mitternacht: split in zwei Intervalle
      out_list[[i]] <- bind_rows(
        r %>% mutate(Uhrzeit = t0,      .part = "midnight_from"),
        r %>% mutate(Uhrzeit = r$Ende,  .part = "midnight_to"),
        r %>% mutate(Uhrzeit = r$Beginn,.part = "from"),
        r %>% mutate(Uhrzeit = t_last,  .part = "to")
      )
    } else {
      # Fallback: wenn Beginn fehlt etc.
      out_list[[i]] <- bind_rows(
        r %>% mutate(Uhrzeit = r$Beginn, .part = "fallback")
      )
    }
  }
  
  expanded <- bind_rows(out_list) %>%
    # Sortierung: primär nach Uhrzeit; sekundär nach ursprünglicher Reihenfolge,
    # damit bei gleichen Zeiten die Eingabereihenfolge stabil bleibt
    mutate(.time_sec = as.numeric(Uhrzeit)) %>%
    arrange(.time_sec, .row_id, .part) %>%
    select(-Beginn, -Ende) %>%
    rename(Stützstelle = Support)
  
  list(data = expanded, measure_cols = measure_cols)
}

# -----------------------------
# Plot-Erstellung
# -----------------------------
make_timeline_plot <- function(expanded_df, measure_cols,
                               work1_on = TRUE, work1_start = hms::as_hms("00:00:00"), work1_end = hms::as_hms("07:00:00"),
                               work2_on = TRUE, work2_start = hms::as_hms("20:00:00"), work2_end = hms::as_hms("23:59:00")) {
  
  long <- expanded_df %>%
    pivot_longer(all_of(measure_cols), names_to = "Kenngröße", values_to = "Value")
  
  # Label-Positionen je zusammenhängendem Block gleicher Stützstelle
  label_y <- mean(range(long$Value, na.rm = TRUE))
  label_df <- expanded_df %>%
    mutate(.grp = cumsum(Stützstelle != dplyr::lag(Stützstelle, default = first(Stützstelle)))) %>%
    group_by(.grp) %>%
    summarise(
      Stützstelle = paste0(unique(Stützstelle), "."),
      Uhrzeit = hms::as_hms(mean(as.numeric(Uhrzeit), na.rm = TRUE)),
      y = label_y,
      .groups = "drop"
    )
  
  p <- ggplot(long, aes(x = Uhrzeit, ymax = Value, color = Kenngröße, fill = Kenngröße)) +
    # Arbeitszeit-Hinterlegung (grau)
    { if (work1_on) annotate(geom = "rect", xmin = as.numeric(work1_start), xmax = as.numeric(work1_end),
                                  ymin = -Inf, ymax = Inf,
                              col = NA, fill = "grey75") } +
    { if (work2_on) annotate(geom = "rect", xmin = as.numeric(work2_start), xmax = as.numeric(work2_end),
                                  ymin = -Inf, ymax = Inf,
                              col = NA, fill = "grey75") } +
    geom_ribbon(aes(ymin = -Inf, group = Kenngröße),
                linewidth = 2.5, alpha = 0.25, outline.type = "upper") +
    geom_point(aes(y = Value), size = 4) +
    labs(x = NULL, y = "Kenngröße (lx)") +
    theme_cowplot(font_size = 20) +
    theme(
      legend.position = "bottom",
      plot.margin = margin(10, 20, 10, 10)
    ) +
    scale_x_time(
      breaks = seq(0, 24, by = 2) * 3600,
      labels = function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%H:%M")
    ) +
    coord_cartesian(
      clip = "off",
      xlim = c(0, 24 * 3600),
      expand = FALSE
    ) +
    geom_label(
      data = label_df,
      aes(x = Uhrzeit, y = y, label = Stützstelle),
      size = 6, label.r = unit(0.5, "lines"),
      fontface = "bold", inherit.aes = FALSE
    ) +
    scale_color_manual(values = c("gold1", "#1D63DC")) +
    scale_fill_manual(values = c("gold1", "#1D63DC"))
  
  # y-Achse sinnvoll begrenzen (mit etwas Luft)
  ymax <- suppressWarnings(max(long$Value, na.rm = TRUE))
  if (is.finite(ymax)) {
    p <- p + coord_cartesian(clip = "off",
                             xlim = c(0, 24 * 3600),
                             ylim = c(0, ymax * 1.05),
                             expand = FALSE)
    p <- p + scale_y_continuous(breaks = pretty(c(0, ymax * 1.05), n = 7))
  }
  
  p
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("Timeline-Plot aus Projekt-Excel"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("xlsx", "Excel-Datei auswählen", accept = c(".xlsx")),
      uiOutput("sheet_ui"),
      textInput("project_name", "Projektname (für Dateiname)", value = ""),
      
      tags$hr(),
      
      checkboxInput("work1_on", "Arbeitszeit-Block 1 anzeigen", value = TRUE),
      fluidRow(
        column(6, textInput("work1_start", "Start 1 (HH:MM)", value = "00:00")),
        column(6, textInput("work1_end",   "Ende 1 (HH:MM)",  value = "07:00"))
      ),
      
      checkboxInput("work2_on", "Arbeitszeit-Block 2 anzeigen", value = TRUE),
      fluidRow(
        column(6, textInput("work2_start", "Start 2 (HH:MM)", value = "20:00")),
        column(6, textInput("work2_end",   "Ende 2 (HH:MM)",  value = "23:59"))
      ),
      
      tags$hr(),
      
      h4("PDF-Download"),
      numericInput("pdf_w", "Breite (inch)", value = 15, min = 3, step = 0.5),
      numericInput("pdf_h", "Höhe (inch)",   value = 5,  min = 2, step = 0.5),
      
      downloadButton("dl_pdf", "Plot als PDF herunterladen")
    ),
    
    mainPanel(
      plotOutput("plot", height = "520px"),
      verbatimTextOutput("status")
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
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
    selectInput("sheet", "Projekt (Excel-Sheet)", choices = sheets(), selected = sheets()[1])
  })
  
  observeEvent(input$sheet, {
    # Projektname vorbelegen, aber user-änderbar lassen
    if (isTRUE(input$project_name == "")) {
      updateTextInput(session, "project_name", value = input$sheet)
    }
  }, ignoreInit = TRUE)
  
  prepared <- reactive({
    req(xlsx_path(), input$sheet)
    df <- read_excel(xlsx_path(), sheet = input$sheet)
    expand_support_points(df)
  })
  
  plot_obj <- reactive({
    prep <- prepared()
    
    # Arbeitszeiten parsen
    w1s <- parse_time_hms(input$work1_start)
    w1e <- parse_time_hms(input$work1_end)
    w2s <- parse_time_hms(input$work2_start)
    w2e <- parse_time_hms(input$work2_end)

    make_timeline_plot(
      expanded_df = prep$data,
      measure_cols = prep$measure_cols,
      work1_on = isTRUE(input$work1_on) && !is.na(w1s) && !is.na(w1e),
      work1_start = w1s, work1_end = w1e,
      work2_on = isTRUE(input$work2_on) && !is.na(w2s) && !is.na(w2e),
      work2_start = w2s, work2_end = w2e
    )
  })
  
  output$plot <- renderPlot({
    req(plot_obj())
    plot_obj()
  }, res = 120)
  
  output$status <- renderText({
    req(prepared())
    prep <- prepared()
    paste0(
      "Sheet: ", input$sheet, "\n",
      "Zeilen (expandiert): ", nrow(prep$data), "\n",
      "Kenngrößen: ", paste(prep$measure_cols, collapse = ", ")
    )
  })
  
  output$dl_pdf <- downloadHandler(
    filename = function() {
      proj <- input$project_name
      if (is.null(proj) || trimws(proj) == "") proj <- input$sheet
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
}

shinyApp(ui, server)