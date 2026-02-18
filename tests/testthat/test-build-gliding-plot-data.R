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
