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
        r %>% dplyr::mutate(Time = r$Begin, .part = "point", .is_original_time = TRUE)
      )
    } else if (!is.na(b) && !is.na(e) && e == b) {
      # Identical start/end timestamps represent an instantaneous jump.
      out_list[[i]] <- dplyr::bind_rows(
        r %>% dplyr::mutate(Time = r$Begin, .part = "jump", .is_original_time = TRUE)
      )
    } else if (!is.na(b) && e > b) {
      out_list[[i]] <- dplyr::bind_rows(
        r %>% dplyr::mutate(Time = r$Begin, .part = "from", .is_original_time = TRUE),
        r %>% dplyr::mutate(Time = r$End, .part = "to", .is_original_time = TRUE)
      )
    } else if (!is.na(b) && !is.na(e) && e < b) {
      out_list[[i]] <- dplyr::bind_rows(
        r %>% dplyr::mutate(Time = t0, .part = "midnight_from", .is_original_time = FALSE),
        r %>% dplyr::mutate(Time = r$End, .part = "midnight_to", .is_original_time = TRUE),
        r %>% dplyr::mutate(Time = r$Begin, .part = "from", .is_original_time = TRUE),
        r %>% dplyr::mutate(Time = t_last, .part = "to", .is_original_time = FALSE)
      )
    } else {
      out_list[[i]] <- dplyr::bind_rows(
        r %>% dplyr::mutate(Time = r$Begin, .part = "fallback", .is_original_time = TRUE)
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

# Build line coordinates for gliding changes and split transitions at midnight.
build_gliding_plot_data <- function(long_df, anchor_time = NULL) {
  day_seconds <- 24 * 3600

  if (is.null(anchor_time) || !is.finite(anchor_time)) {
    anchor_time <- suppressWarnings(min(long_df$.time_sec, na.rm = TRUE))
  }

  long_df %>%
    dplyr::mutate(
      # Keep post-midnight fragments (`midnight_*`) after the anchor point,
      # even when their clock time equals the anchor (e.g., 11:00 -> next day).
      .adj_time = dplyr::if_else(
        .time_sec < anchor_time | (.time_sec == anchor_time & .part %in% c("midnight_from", "midnight_to")),
        .time_sec + day_seconds,
        .time_sec
      )
    ) %>%
    dplyr::arrange(Measure, .adj_time, .row_id, .part) %>%
    dplyr::group_by(Measure) %>%
    dplyr::group_modify(function(df_measure, ...) {
      if (nrow(df_measure) == 0) {
        return(df_measure)
      }

      times <- df_measure$.adj_time
      values <- df_measure$Value

      out_time <- numeric(0)
      out_value <- numeric(0)
      out_segment <- integer(0)
      segment_id <- 1L

      append_point <- function(time_value, y_value, segment_value) {
        out_time <<- c(out_time, time_value)
        out_value <<- c(out_value, y_value)
        out_segment <<- c(out_segment, segment_value)
      }

      append_point(times[[1]], values[[1]], segment_id)

      append_transition <- function(t1, t2, v1, v2) {
        crosses_midnight <- floor(t1 / day_seconds) < floor(t2 / day_seconds)

        if (crosses_midnight) {
          midnight_time <- day_seconds
          fraction <- (midnight_time - t1) / (t2 - t1)
          value_midnight <- v1 + (v2 - v1) * fraction

          append_point(midnight_time, value_midnight, segment_id)
          segment_id <<- segment_id + 1L
          append_point(0, value_midnight, segment_id)

          # Avoid creating a synthetic 0 -> 24:00 horizontal segment when
          # the next point is exactly midnight (shifted to 24:00).
          if (!isTRUE(all.equal(t2, midnight_time))) {
            append_point(t2, v2, segment_id)
          }
        } else {
          append_point(t2, v2, segment_id)
        }
      }

      if (length(times) >= 2) {
        for (i in seq_len(length(times) - 1)) {
          append_transition(
            t1 = times[[i]],
            t2 = times[[i + 1]],
            v1 = values[[i]],
            v2 = values[[i + 1]]
          )
        }
      }

      # Close the daily loop for gliding lines by connecting the final point
      # to the first point on the following day.
      if (length(times) >= 2) {
        append_transition(
          t1 = times[[length(times)]],
          t2 = times[[1]] + day_seconds,
          v1 = values[[length(values)]],
          v2 = values[[1]]
        )
      }

      wrapped_time <- dplyr::if_else(
        out_time > day_seconds,
        out_time %% day_seconds,
        out_time
      )
      segment_label <- paste0("seg_", out_segment)

      # Keep the right-edge midnight endpoint (24:00) for the segment that
      # leads into midnight, but avoid pairing 0 and 24:00 in the same segment.
      # When a segment already contains 0, its 24:00 points represent that same
      # instant on the wrapped timeline and should be drawn at 0.
      wrapped_time <- dplyr::if_else(
        wrapped_time == day_seconds & ave(wrapped_time == 0, segment_label, FUN = any),
        0,
        wrapped_time
      )

      tibble::tibble(
        Time = hms::as_hms(wrapped_time),
        Value = out_value,
        .segment = segment_label
      )
    }) %>%
    dplyr::ungroup()
}

# Build step coordinates that carry values through midnight without interpolation.
build_step_plot_data <- function(long_df, anchor_time = NULL) {
  day_seconds <- 24 * 3600

  if (is.null(anchor_time) || !is.finite(anchor_time)) {
    anchor_time <- suppressWarnings(min(long_df$.time_sec, na.rm = TRUE))
  }

  long_df %>%
    dplyr::mutate(
      .adj_time = dplyr::if_else(
        .time_sec < anchor_time | (.time_sec == anchor_time & .part %in% c("midnight_from", "midnight_to")),
        .time_sec + day_seconds,
        .time_sec
      )
    ) %>%
    dplyr::arrange(Measure, .adj_time, .row_id, .part) %>%
    dplyr::group_by(Measure) %>%
    dplyr::group_modify(function(df_measure, ...) {
      if (nrow(df_measure) == 0) {
        return(df_measure)
      }

      times <- df_measure$.adj_time
      values <- df_measure$Value

      out_time <- numeric(0)
      out_value <- numeric(0)
      out_segment <- integer(0)
      segment_id <- 1L

      append_point <- function(time_value, y_value, segment_value) {
        out_time <<- c(out_time, time_value)
        out_value <<- c(out_value, y_value)
        out_segment <<- c(out_segment, segment_value)
      }

      append_point(times[[1]], values[[1]], segment_id)

      append_transition <- function(t1, t2, v1, v2) {
        crosses_midnight <- floor(t1 / day_seconds) < floor(t2 / day_seconds)

        if (crosses_midnight) {
          midnight_time <- day_seconds

          append_point(midnight_time, v1, segment_id)
          segment_id <<- segment_id + 1L
          append_point(0, v1, segment_id)

          # Avoid creating a synthetic 0 -> 24:00 horizontal segment when
          # the next point is exactly midnight (shifted to 24:00).
          if (!isTRUE(all.equal(t2, midnight_time))) {
            append_point(t2, v2, segment_id)
          }
        } else {
          append_point(t2, v2, segment_id)
        }
      }

      if (length(times) >= 2) {
        for (i in seq_len(length(times) - 1)) {
          append_transition(
            t1 = times[[i]],
            t2 = times[[i + 1]],
            v1 = values[[i]],
            v2 = values[[i + 1]]
          )
        }
      }

      if (length(times) >= 2) {
        append_transition(
          t1 = times[[length(times)]],
          t2 = times[[1]] + day_seconds,
          v1 = values[[length(values)]],
          v2 = values[[1]]
        )
      }

      wrapped_time <- dplyr::if_else(
        out_time > day_seconds,
        out_time %% day_seconds,
        out_time
      )
      segment_label <- paste0("seg_", out_segment)

      wrapped_time <- dplyr::if_else(
        wrapped_time == day_seconds & ave(wrapped_time == 0, segment_label, FUN = any),
        0,
        wrapped_time
      )

      tibble::tibble(
        Time = hms::as_hms(wrapped_time),
        Value = out_value,
        .segment = segment_label
      )
    }) %>%
    dplyr::ungroup()
}

# Expand step points into explicit horizontal-then-vertical corners for ribbons.
build_step_ribbon_data <- function(step_df) {
  step_df %>%
    dplyr::mutate(.time_sec = as.numeric(Time)) %>%
    dplyr::arrange(Measure, .segment, .time_sec) %>%
    dplyr::group_by(Measure, .segment) %>%
    dplyr::group_modify(function(df_segment, ...) {
      if (nrow(df_segment) <= 1) {
        return(df_segment[, c("Time", "Value", ".time_sec")])
      }

      idx <- seq_len(nrow(df_segment) - 1L)
      corner_rows <- lapply(idx, function(i) {
        tibble::tibble(
          Time = c(df_segment$Time[i + 1L], df_segment$Time[i + 1L]),
          Value = c(df_segment$Value[i], df_segment$Value[i + 1L]),
          .time_sec = c(df_segment$.time_sec[i + 1L], df_segment$.time_sec[i + 1L])
        )
      })

      dplyr::bind_rows(
        tibble::tibble(
          Time = df_segment$Time[1],
          Value = df_segment$Value[1],
          .time_sec = df_segment$.time_sec[1]
        ),
        dplyr::bind_rows(corner_rows)
      )
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.time_sec)
}

# Create timeline plot using user-selected colors.
make_timeline_plot <- function(expanded_df, measure_cols,
                               color_map,
                               y_axis_label = "Measure value (lx)",
                               legend_title = "Measure",
                               scene_label_height = NULL,
                               y_axis_min = NULL,
                               y_axis_max = NULL,
                               work1_on = TRUE,
                               work1_start = hms::as_hms("00:00:00"),
                               work1_end = hms::as_hms("07:00:00"),
                               work2_on = TRUE,
                               work2_start = hms::as_hms("20:00:00"),
                               work2_end = hms::as_hms("23:59:00"),
                               line_geom = c("step", "path")) {
  line_geom <- rlang::arg_match(line_geom)

  long <- expanded_df %>%
    tidyr::pivot_longer(dplyr::all_of(measure_cols), names_to = "Measure", values_to = "Value")

  first_start <- expanded_df %>%
    dplyr::filter(.row_id == min(.row_id, na.rm = TRUE), .part %in% c("from", "point", "jump", "fallback")) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(.time_sec)
  anchor_time <- if (length(first_start) == 1 && is.finite(first_start)) first_start else suppressWarnings(min(long$.time_sec, na.rm = TRUE))

  gliding_long <- build_gliding_plot_data(long, anchor_time = anchor_time)
  step_long <- build_step_plot_data(long, anchor_time = anchor_time)
  step_ribbon_long <- build_step_ribbon_data(step_long)
  point_long <- if (".is_original_time" %in% names(long)) {
    long %>% dplyr::filter(.is_original_time)
  } else {
    long %>% dplyr::filter(!.part %in% c("midnight_from", "midnight_to"))
  }

  label_y_default <- mean(range(long$Value, na.rm = TRUE))
  label_y <- if (is.null(scene_label_height) || !is.finite(scene_label_height)) {
    label_y_default
  } else {
    scene_label_height
  }
  label_df <- expanded_df %>%
    dplyr::mutate(.grp = cumsum(Support != dplyr::lag(Support, default = dplyr::first(Support)))) %>%
    dplyr::group_by(.grp) %>%
    dplyr::summarise(
      Support = paste0(unique(Support), "."),
      Time = hms::as_hms(mean(as.numeric(Time), na.rm = TRUE)),
      y = label_y,
      .groups = "drop"
    )

  p <- ggplot2::ggplot() +
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
    {
      ribbon_data <- if (identical(line_geom, "step")) step_ribbon_long else gliding_long

      ggplot2::geom_ribbon(
        data = ribbon_data,
        ggplot2::aes(
          x = Time,
          ymax = Value,
          ymin = -Inf,
          fill = Measure,
          group = interaction(Measure, .segment)
        ),
        alpha = 0.25,
        colour = NA,
        show.legend = FALSE,
        na.rm = TRUE
      )
    } +
    {
      if (identical(line_geom, "step")) {
        ggplot2::geom_step(
          data = step_long,
          ggplot2::aes(x = Time, y = Value, color = Measure, group = interaction(Measure, .segment)),
          direction = "hv",
          linewidth = 1.4,
          na.rm = TRUE
        )
      } else {
        ggplot2::geom_line(
          data = gliding_long,
          ggplot2::aes(x = Time, y = Value, color = Measure, group = interaction(Measure, .segment)),
          linewidth = 1.4,
          na.rm = TRUE
        )
      }
    } +
    ggplot2::geom_point(
      data = point_long,
      ggplot2::aes(x = Time, y = Value, color = Measure, group = Measure),
      size = 3.2,
      na.rm = TRUE
    ) +
    ggplot2::labs(x = NULL, y = y_axis_label, color = legend_title) +
    cowplot::theme_cowplot(font_size = 16) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.margin = ggplot2::margin(10, 20, 10, 10)
    ) +
    ggplot2::scale_fill_manual(values = color_map, breaks = names(color_map), guide = "none") +
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
  ymin <- suppressWarnings(min(long$Value, na.rm = TRUE))

  if (is.finite(ymax) && is.finite(ymin)) {
    y_min_use <- if (is.null(y_axis_min) || is.na(y_axis_min)) min(0, ymin) else y_axis_min
    y_max_default <- ymax * 1.05
    y_max_use <- if (is.null(y_axis_max) || is.na(y_axis_max)) y_max_default else y_axis_max

    if (is.finite(y_min_use) && is.finite(y_max_use) && y_min_use < y_max_use) {
      y_limits <- c(y_min_use, y_max_use)
    } else {
      y_limits <- c(min(0, ymin), y_max_default)
    }

    p <- p + ggplot2::coord_cartesian(
      clip = "off",
      xlim = c(0, 24 * 3600),
      ylim = y_limits,
      expand = FALSE
    )
    p <- p + ggplot2::scale_y_continuous(breaks = pretty(y_limits, n = 7))
  }

  p
}
