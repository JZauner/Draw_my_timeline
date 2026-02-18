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
