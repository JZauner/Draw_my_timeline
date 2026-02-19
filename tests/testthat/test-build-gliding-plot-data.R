testthat::test_that("build_gliding_plot_data avoids artificial 0-24h segment at midnight", {
  long_df <- tibble::tibble(
    Measure = "mel EDI",
    .time_sec = c(21.5 * 3600, 23.98 * 3600, 0, 2 * 3600),
    .row_id = 1:4,
    .part = c("from", "to", "midnight_from", "midnight_to"),
    Value = c(120, 120, 90, 90)
  )

  gliding <- build_gliding_plot_data(long_df, anchor_time = 21.5 * 3600)

  segment_span <- gliding %>%
    dplyr::mutate(.time_sec = as.numeric(Time)) %>%
    dplyr::group_by(.segment) %>%
    dplyr::summarise(span = diff(range(.time_sec)), .groups = "drop")

  testthat::expect_true(all(segment_span$span < 24 * 3600))
})

testthat::test_that("build_gliding_plot_data keeps midnight_to points after the anchor tie", {
  long_df <- tibble::tibble(
    Measure = "mel EDI",
    .time_sec = c(11 * 3600, 15 * 3600, 0, 11 * 3600),
    .row_id = c(1L, 1L, 2L, 2L),
    .part = c("from", "to", "midnight_from", "midnight_to"),
    Value = c(500, 500, 300, 300)
  )

  gliding <- build_gliding_plot_data(long_df, anchor_time = 11 * 3600)

  ordered <- gliding %>%
    dplyr::mutate(.time_sec = as.numeric(Time)) %>%
    dplyr::arrange(.segment, .time_sec)

  # The pre-midnight support (500) should be drawn before the post-midnight
  # continuation (300), avoiding a diagonal from 00:00 to 11:00.
  testthat::expect_equal(head(ordered$Value, 2), c(500, 500))
})

testthat::test_that("build_gliding_plot_data keeps 24:00 at midnight without creating a path gap", {
  long_df <- tibble::tibble(
    Measure = "mel EDI",
    .time_sec = c(21 * 3600, 24 * 3600, 0, 2 * 3600),
    .row_id = 1:4,
    .part = c("from", "to", "midnight_from", "midnight_to"),
    Value = c(500, 500, 300, 300)
  )

  gliding <- build_gliding_plot_data(long_df, anchor_time = 21 * 3600) %>%
    dplyr::mutate(.time_sec = as.numeric(Time))

  midnight_segment <- gliding %>%
    dplyr::filter(.segment == "seg_1")

  testthat::expect_true(any(midnight_segment$.time_sec == 24 * 3600))
  testthat::expect_false(any(midnight_segment$.time_sec == 0))

  # The continuation segment should not include both 0 and 24:00, otherwise
  # geom_path can draw a full-width line or create a visual discontinuity.
  segment_span <- gliding %>%
    dplyr::group_by(.segment) %>%
    dplyr::summarise(span = diff(range(.time_sec)), .groups = "drop")

  testthat::expect_true(all(segment_span$span < 24 * 3600))
})

testthat::test_that("build_gliding_plot_data closes the final segment back to the first point", {
  long_df <- tibble::tibble(
    Measure = "mel EDI",
    .time_sec = c(6 * 3600, 12 * 3600, 18 * 3600),
    .row_id = 1:3,
    .part = c("from", "from", "from"),
    Value = c(100, 200, 50)
  )

  gliding <- build_gliding_plot_data(long_df, anchor_time = 6 * 3600) %>%
    dplyr::mutate(.time_sec = as.numeric(Time))

  # The closing transition should create both midnight boundary points.
  testthat::expect_true(any(gliding$.time_sec == 24 * 3600))
  testthat::expect_true(any(gliding$.time_sec == 0))

  # No segment should be reduced to a single point after wrapping.
  point_count <- gliding %>%
    dplyr::count(.segment, name = "n_points")

  testthat::expect_true(all(point_count$n_points >= 2))
})

testthat::test_that("make_timeline_plot accepts explicit scene label height", {
  expanded <- tibble::tibble(
    .row_id = c(1L, 1L, 2L, 2L),
    Support = c("1", "1", "2", "2"),
    Time = hms::as_hms(c(0, 6 * 3600, 8 * 3600, 12 * 3600)),
    .part = c("from", "to", "from", "to"),
    .time_sec = as.numeric(Time),
    `mel EDI` = c(100, 100, 200, 200)
  )

  p <- make_timeline_plot(
    expanded_df = expanded,
    measure_cols = "mel EDI",
    color_map = c("mel EDI" = "#1D63DC"),
    scene_label_height = 333
  )

  label_idx <- which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomLabel"), logical(1)))[1]
  testthat::expect_true(is.finite(label_idx))
  testthat::expect_equal(unique(p$layers[[label_idx]]$data$y), 333)
})

testthat::test_that("build_step_plot_data carries the pre-midnight value across midnight", {
  long_df <- tibble::tibble(
    Measure = "mel EDI",
    .time_sec = c(21 * 3600, 23 * 3600, 0, 2 * 3600),
    .row_id = c(1L, 1L, 2L, 2L),
    .part = c("from", "to", "midnight_from", "midnight_to"),
    Value = c(500, 500, 300, 300)
  )

  step_data <- build_step_plot_data(long_df, anchor_time = 21 * 3600) %>%
    dplyr::mutate(.time_sec = as.numeric(Time))

  midnight_values <- step_data %>%
    dplyr::filter(.time_sec %in% c(0, 24 * 3600)) %>%
    dplyr::pull(Value)

  testthat::expect_true(length(midnight_values) >= 2)
  testthat::expect_equal(unique(midnight_values), 500)
})

testthat::test_that("make_timeline_plot only draws points from original timestamps", {
  expanded <- tibble::tibble(
    .row_id = c(1L, 1L, 2L, 2L),
    Support = c("1", "1", "2", "2"),
    Time = hms::as_hms(c(22 * 3600, 23 * 3600, 0, 2 * 3600)),
    .part = c("from", "to", "midnight_from", "midnight_to"),
    .time_sec = as.numeric(Time),
    `mel EDI` = c(100, 100, 200, 200)
  )

  p <- make_timeline_plot(
    expanded_df = expanded,
    measure_cols = "mel EDI",
    color_map = c("mel EDI" = "#1D63DC"),
    line_geom = "step"
  )

  point_idx <- which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))[1]
  testthat::expect_true(is.finite(point_idx))

  point_times <- as.numeric(p$layers[[point_idx]]$data$Time)
  testthat::expect_setequal(point_times, c(22 * 3600, 23 * 3600))
})

testthat::test_that("expand_support_points marks synthetic overnight boundary times", {
  df <- tibble::tibble(
    `Stützstelle / Lichtszene` = "1",
    Beginn = "22:00",
    Ende = "06:30",
    `mel EDI` = 500
  )

  expanded <- expand_support_points(df)$data

  testthat::expect_true(".is_original_time" %in% names(expanded))

  time_map <- expanded %>%
    dplyr::transmute(
      .time_sec = as.numeric(Time),
      .is_original_time
    )

  testthat::expect_true(all(time_map$.is_original_time[time_map$.time_sec %in% c(22 * 3600, 6.5 * 3600)]))
  testthat::expect_true(all(!time_map$.is_original_time[time_map$.time_sec %in% c(0, 23 * 3600 + 59 * 60)]))
})

testthat::test_that("make_timeline_plot uses .is_original_time when available", {
  expanded <- tibble::tibble(
    .row_id = c(1L, 1L, 1L, 1L),
    Support = rep("1", 4),
    Time = hms::as_hms(c(0, 6.5 * 3600, 22 * 3600, 23 * 3600 + 59 * 60)),
    .part = c("midnight_from", "midnight_to", "from", "to"),
    .time_sec = as.numeric(Time),
    .is_original_time = c(FALSE, TRUE, TRUE, FALSE),
    `mel EDI` = c(100, 100, 200, 200)
  )

  p <- make_timeline_plot(
    expanded_df = expanded,
    measure_cols = "mel EDI",
    color_map = c("mel EDI" = "#1D63DC"),
    line_geom = "step"
  )

  point_idx <- which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))[1]
  testthat::expect_true(is.finite(point_idx))

  point_times <- as.numeric(p$layers[[point_idx]]$data$Time)
  testthat::expect_setequal(point_times, c(6.5 * 3600, 22 * 3600))
})

testthat::test_that("make_timeline_plot adds a ribbon from -Inf for step transitions", {
  expanded <- tibble::tibble(
    .row_id = c(1L, 1L, 2L, 2L),
    Support = c("1", "1", "2", "2"),
    Time = hms::as_hms(c(0, 6 * 3600, 8 * 3600, 12 * 3600)),
    .part = c("from", "to", "from", "to"),
    .time_sec = as.numeric(Time),
    `mel EDI` = c(100, 100, 200, 200)
  )

  p <- make_timeline_plot(
    expanded_df = expanded,
    measure_cols = "mel EDI",
    color_map = c("mel EDI" = "#1D63DC"),
    line_geom = "step"
  )

  ribbon_idx <- which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomRibbon"), logical(1)))[1]
  testthat::expect_true(is.finite(ribbon_idx))
  testthat::expect_equal(p$layers[[ribbon_idx]]$aes_params$alpha, 0.25)
  testthat::expect_true(is.infinite(p$layers[[ribbon_idx]]$mapping$ymin))
})

testthat::test_that("make_timeline_plot adds a ribbon from -Inf for gliding transitions", {
  expanded <- tibble::tibble(
    .row_id = c(1L, 1L, 2L, 2L),
    Support = c("1", "1", "2", "2"),
    Time = hms::as_hms(c(0, 6 * 3600, 8 * 3600, 12 * 3600)),
    .part = c("from", "to", "from", "to"),
    .time_sec = as.numeric(Time),
    `mel EDI` = c(100, 100, 200, 200)
  )

  p <- make_timeline_plot(
    expanded_df = expanded,
    measure_cols = "mel EDI",
    color_map = c("mel EDI" = "#1D63DC"),
    line_geom = "path"
  )

  ribbon_idx <- which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomRibbon"), logical(1)))[1]
  testthat::expect_true(is.finite(ribbon_idx))
  testthat::expect_equal(p$layers[[ribbon_idx]]$aes_params$alpha, 0.25)
  testthat::expect_true(is.infinite(p$layers[[ribbon_idx]]$mapping$ymin))
})


testthat::test_that("build_step_ribbon_data adds horizontal corner points for jumps", {
  step_df <- tibble::tibble(
    Measure = "mel EDI",
    Time = hms::as_hms(c(0, 8 * 3600, 14 * 3600)),
    Value = c(100, 300, 150),
    .segment = "seg_1"
  )

  ribbon_df <- build_step_ribbon_data(step_df) %>%
    dplyr::mutate(.time_sec = as.numeric(Time))

  testthat::expect_equal(
    ribbon_df$.time_sec,
    c(0, 8 * 3600, 8 * 3600, 14 * 3600, 14 * 3600)
  )
  testthat::expect_equal(ribbon_df$Value, c(100, 100, 300, 300, 150))
})

testthat::test_that("make_timeline_plot step ribbon follows hv transitions", {
  expanded <- tibble::tibble(
    .row_id = c(1L, 1L, 2L, 2L),
    Support = c("1", "1", "2", "2"),
    Time = hms::as_hms(c(0, 8 * 3600, 14 * 3600, 18 * 3600)),
    .part = c("from", "to", "from", "to"),
    .time_sec = as.numeric(Time),
    `mel EDI` = c(100, 100, 300, 300)
  )

  p <- make_timeline_plot(
    expanded_df = expanded,
    measure_cols = "mel EDI",
    color_map = c("mel EDI" = "#1D63DC"),
    line_geom = "step"
  )

  ribbon_idx <- which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomRibbon"), logical(1)))[1]
  testthat::expect_true(is.finite(ribbon_idx))

  ribbon_data <- p$layers[[ribbon_idx]]$data %>%
    dplyr::mutate(.time_sec = as.numeric(Time))

  jump_rows <- ribbon_data %>%
    dplyr::filter(.time_sec == 14 * 3600)

  testthat::expect_equal(jump_rows$Value, c(100, 300))
})
