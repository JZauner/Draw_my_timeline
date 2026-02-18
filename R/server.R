app_server <- function(input, output, session) {
  manual_data <- manual_timeline_server("manual")
  example_xlsx_path <- "Examples.xlsx"
  excel_enabled <- is_excel_data_source_available(example_xlsx_path)

  shiny::observe({
    if (!isTRUE(excel_enabled) && !identical(input$data_source, "manual")) {
      shiny::updateRadioButtons(session, "data_source", selected = "manual")
    }
  })

  xlsx_path <- shiny::reactive({
    if (!is.null(input$xlsx$datapath) && nzchar(input$xlsx$datapath)) {
      return(input$xlsx$datapath)
    }

    shiny::req(file.exists(example_xlsx_path))
    example_xlsx_path
  })

  sheets <- shiny::reactive({
    shiny::req(isTRUE(excel_enabled), xlsx_path())
    readxl::excel_sheets(xlsx_path())
  })

  output$sheet_ui <- shiny::renderUI({
    shiny::req(sheets())
    shiny::selectInput("sheet", "Project (Excel sheet)", choices = sheets(), selected = sheets()[1])
  })

  shiny::observeEvent(input$sheet, {
    if (isTRUE(input$data_source == "excel") && !is.null(input$sheet)) {
      shiny::updateTextInput(session, "project_name", value = input$sheet)
    }
  }, ignoreInit = FALSE)

  shiny::observeEvent(manual_data(), {
    md <- manual_data()
    if (!is.null(md)) {
      shiny::updateTextInput(session, "project_name", value = md$project_name)
    }
  })

  excel_prepared <- shiny::reactive({
    shiny::req(isTRUE(excel_enabled), input$data_source == "excel", xlsx_path(), input$sheet)
    df <- readxl::read_excel(xlsx_path(), sheet = input$sheet)
    prepare_excel_by_position(df)
  })

  active_prepared <- shiny::reactive({
    if (!isTRUE(excel_enabled) || identical(input$data_source, "manual")) {
      shiny::req(manual_data())
      manual_data()$prepared
    } else {
      excel_prepared()
    }
  })

  output$color_ui <- shiny::renderUI({
    prep <- active_prepared()
    shiny::req(prep$measure_cols)
    color_inputs_ui(prep$measure_cols)
  })

  plot_obj <- shiny::reactive({
    prep <- active_prepared()

    w1s <- parse_time_hms(input$work1_start)
    w1e <- parse_time_hms(input$work1_end)
    w2s <- parse_time_hms(input$work2_start)
    w2e <- parse_time_hms(input$work2_end)

    color_map <- build_color_map(input, prep$measure_cols)

    y_axis_label <- trimws(input$y_axis_label)
    legend_title <- trimws(input$legend_title)

    make_timeline_plot(
      expanded_df = prep$data,
      measure_cols = prep$measure_cols,
      color_map = color_map,
      y_axis_label = if (nzchar(y_axis_label)) y_axis_label else "Measure value (lx)",
      legend_title = if (nzchar(legend_title)) legend_title else "Measure",
      scene_label_height = input$scene_label_height,
      y_axis_min = input$y_axis_min,
      y_axis_max = input$y_axis_max,
      work1_on = isTRUE(input$work1_on) && !is.na(w1s) && !is.na(w1e),
      work1_start = w1s,
      work1_end = w1e,
      work2_on = isTRUE(input$work2_on) && !is.na(w2s) && !is.na(w2e),
      work2_start = w2s,
      work2_end = w2e,
      line_geom = input$line_geom %||% "step"
    )
  })

  output$plot <- shiny::renderPlot({
    shiny::req(plot_obj())
    plot_obj()
  }, res = 120)

  output$status <- shiny::renderText({
    prep <- active_prepared()
    source_name <- if (identical(input$data_source, "manual")) "Manual timeline" else paste("Sheet", input$sheet)

    paste0(
      "Source: ", source_name, "\n",
      "Rows (expanded): ", nrow(prep$data), "\n",
      "Measures: ", paste(prep$measure_cols, collapse = ", ")
    )
  })

  output$dl_pdf <- shiny::downloadHandler(
    filename = function() {
      proj <- trimws(input$project_name)
      if (proj == "") proj <- if (identical(input$data_source, "manual")) "manual_timeline" else input$sheet
      proj <- stringr::str_replace_all(proj, "[^A-Za-z0-9_\\-]+", "_")
      paste0(proj, "_timelinePlot.pdf")
    },
    content = function(file) {
      shiny::req(plot_obj())
      ggplot2::ggsave(
        filename = file,
        plot = plot_obj(),
        width = input$pdf_w,
        height = input$pdf_h,
        dpi = 300
      )
    }
  )

  output$download_example <- shiny::downloadHandler(
    filename = function() "Examples.xlsx",
    content = function(file) {
      shiny::req(isTRUE(excel_enabled))
      file.copy(example_xlsx_path, file, overwrite = TRUE)
    }
  )

  output$dl_png <- shiny::downloadHandler(
    filename = function() {
      proj <- trimws(input$project_name)
      if (proj == "") proj <- if (identical(input$data_source, "manual")) "manual_timeline" else input$sheet
      proj <- stringr::str_replace_all(proj, "[^A-Za-z0-9_\\-]+", "_")
      paste0(proj, "_timelinePlot.png")
    },
    content = function(file) {
      shiny::req(plot_obj())
      ggplot2::ggsave(
        filename = file,
        plot = plot_obj(),
        width = input$pdf_w,
        height = input$pdf_h,
        dpi = 300
      )
    }
  )
}
