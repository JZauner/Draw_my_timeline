# Utility helpers for timeline parsing and plotting.

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
    !is.na(x_chr) & stringr::str_detect(x_chr, "^\\d{1,2}:\\d{2}$"),
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

  num_chr <- stringr::str_extract(x_chr, "[0-9]+([\\.,][0-9]+)?")
  num_chr <- stringr::str_replace_all(num_chr, ",", ".")

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
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      Support = .data[[col_support]],
      Begin = parse_time_hms(.data[[col_begin]]),
      End = parse_time_hms(.data[[col_end]])
    ) %>%
    dplyr::filter(!is.na(Support)) %>%
    dplyr::mutate(Support = as.character(Support))

  drop_cols <- c(
    col_support, col_begin, col_end,
    "Zielwert", ".row_id", "Support", "Begin", "End"
  )
  measure_cols <- setdiff(names(df), drop_cols)

  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(measure_cols), parse_numeric_from_text))

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
      out_list[[i]] <- dplyr::bind_rows(
        r %>% dplyr::mutate(Time = r$Begin, .part = "point")
      )
    } else if (!is.na(b) && !is.na(e) && e == b) {
      # Identical start/end timestamps represent an instantaneous jump.
      out_list[[i]] <- dplyr::bind_rows(
        r %>% dplyr::mutate(Time = r$Begin, .part = "jump")
      )
    } else if (!is.na(b) && e > b) {
      out_list[[i]] <- dplyr::bind_rows(
        r %>% dplyr::mutate(Time = r$Begin, .part = "from"),
        r %>% dplyr::mutate(Time = r$End, .part = "to")
      )
    } else if (!is.na(b) && !is.na(e) && e < b) {
      out_list[[i]] <- dplyr::bind_rows(
        r %>% dplyr::mutate(Time = t0, .part = "midnight_from"),
        r %>% dplyr::mutate(Time = r$End, .part = "midnight_to"),
        r %>% dplyr::mutate(Time = r$Begin, .part = "from"),
        r %>% dplyr::mutate(Time = t_last, .part = "to")
      )
    } else {
      out_list[[i]] <- dplyr::bind_rows(
        r %>% dplyr::mutate(Time = r$Begin, .part = "fallback")
      )
    }
  }

  expanded <- dplyr::bind_rows(out_list) %>%
    dplyr::mutate(.time_sec = as.numeric(Time)) %>%
    dplyr::arrange(.time_sec, .row_id, .part) %>%
    dplyr::select(-Begin, -End)

  list(data = expanded, measure_cols = measure_cols)
}

# Use first three Excel columns as Support/Begin/End, independent of header names.
prepare_excel_by_position <- function(df) {
  if (ncol(df) < 3) {
    stop("Excel sheet must contain at least three columns (Support, Begin, End by position).")
  }

  col_ids <- names(df)[seq_len(3)]

  expand_support_points(
    df,
    col_support = col_ids[[1]],
    col_begin = col_ids[[2]],
    col_end = col_ids[[3]]
  )
}

# Build a named color map from dynamic color inputs.
build_color_map <- function(input, measure_cols) {
  default_palette <- c(normalize_hex_color("gold1"), normalize_hex_color("#1D63DC"))
  defaults <- rep(default_palette, length.out = length(measure_cols))

  stats::setNames(
    vapply(seq_along(measure_cols), function(i) {
      input[[paste0("col_", i)]] %||% defaults[i]
    }, FUN.VALUE = character(1)),
    measure_cols
  )
}

# Create timeline plot using user-selected colors.
make_timeline_plot <- function(expanded_df, measure_cols,
                               color_map,
                               y_axis_label = "Measure value (lx)",
                               legend_title = "Measure",
                               work1_on = TRUE,
                               work1_start = hms::as_hms("00:00:00"),
                               work1_end = hms::as_hms("07:00:00"),
                               work2_on = TRUE,
                               work2_start = hms::as_hms("20:00:00"),
                               work2_end = hms::as_hms("23:59:00")) {
  long <- expanded_df %>%
    tidyr::pivot_longer(dplyr::all_of(measure_cols), names_to = "Measure", values_to = "Value")

  label_y <- mean(range(long$Value, na.rm = TRUE))
  label_df <- expanded_df %>%
    dplyr::mutate(.grp = cumsum(Support != dplyr::lag(Support, default = dplyr::first(Support)))) %>%
    dplyr::group_by(.grp) %>%
    dplyr::summarise(
      Support = paste0(unique(Support), "."),
      Time = hms::as_hms(mean(as.numeric(Time), na.rm = TRUE)),
      y = label_y,
      .groups = "drop"
    )

  p <- ggplot2::ggplot(long, ggplot2::aes(x = Time, y = Value, color = Measure, group = Measure)) +
    {
      if (work1_on) {
        ggplot2::annotate(
          geom = "rect",
          xmin = as.numeric(work1_start), xmax = as.numeric(work1_end),
          ymin = -Inf, ymax = Inf,
          col = NA, fill = "grey85"
        )
      }
    } +
    {
      if (work2_on) {
        ggplot2::annotate(
          geom = "rect",
          xmin = as.numeric(work2_start), xmax = as.numeric(work2_end),
          ymin = -Inf, ymax = Inf,
          col = NA, fill = "grey85"
        )
      }
    } +
    ggplot2::geom_line(direction = "vh", linewidth = 1.4, na.rm = TRUE) +
    ggplot2::geom_point(size = 3.2, na.rm = TRUE) +
    ggplot2::labs(x = NULL, y = y_axis_label, color = legend_title) +
    cowplot::theme_cowplot(font_size = 16) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.margin = ggplot2::margin(10, 20, 10, 10)
    ) +
    ggplot2::scale_color_manual(values = color_map, breaks = names(color_map)) +
    ggplot2::scale_x_time(
      breaks = seq(0, 24, by = 2) * 3600,
      labels = function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%H:%M")
    ) +
    ggplot2::coord_cartesian(clip = "off", xlim = c(0, 24 * 3600), expand = FALSE) +
    ggplot2::geom_label(
      data = label_df,
      ggplot2::aes(x = Time, y = y, label = Support),
      size = 4.4,
      label.r = grid::unit(0.5, "lines"),
      fontface = "bold",
      inherit.aes = FALSE
    )

  ymax <- suppressWarnings(max(long$Value, na.rm = TRUE))
  if (is.finite(ymax)) {
    p <- p + ggplot2::coord_cartesian(
      clip = "off",
      xlim = c(0, 24 * 3600),
      ylim = c(0, ymax * 1.05),
      expand = FALSE
    )
    p <- p + ggplot2::scale_y_continuous(breaks = pretty(c(0, ymax * 1.05), n = 7))
  }

  p
}
