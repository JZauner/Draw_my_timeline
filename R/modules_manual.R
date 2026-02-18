# Shiny module that captures user-defined timeline data in a modal dialog.
manual_timeline_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    manual_data <- shiny::reactiveVal(NULL)

    default_csv <- paste(
      "Scene,Begin,End,mel EDI,Task illuminance",
      "Scene A,00:00,07:00,100,500",
      "Scene B,07:30,20:00,140,700",
      "Scene C,20:15,23:59,100,500",
      sep = "\n"
    )

    shiny::observeEvent(input$open_modal, {
      shiny::showModal(shiny::modalDialog(
        title = "Create a custom timeline",
        shiny::textInput(session$ns("manual_project"), "Project name", value = "Manual timeline"),
        shiny::tags$p(
          "Provide comma-separated values with at least the columns:",
          shiny::tags$code("Scene, Begin, End"),
          ". Additional numeric columns are plotted as measures."
        ),
        shiny::textAreaInput(
          session$ns("manual_csv"),
          label = "Timeline table (CSV format)",
          value = default_csv,
          rows = 12,
          width = "100%"
        ),
        footer = shiny::tagList(
          shiny::actionButton(session$ns("load_example"), "Load example"),
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("save_manual"), "Use timeline", class = "btn-primary")
        ),
        easyClose = TRUE,
        size = "l"
      ))
    })

    shiny::observeEvent(input$load_example, {
      shiny::updateTextAreaInput(session, "manual_csv", value = default_csv)
    })

    shiny::observeEvent(input$save_manual, {
      csv_text <- input$manual_csv
      parsed <- tryCatch(
        read.csv(text = csv_text, check.names = FALSE, stringsAsFactors = FALSE),
        error = function(e) e
      )

      if (inherits(parsed, "error")) {
        shiny::showNotification("Could not parse CSV text. Please check separators and headers.", type = "error")
        return()
      }

      support_col <- if ("Scene" %in% names(parsed)) "Scene" else "Support"
      required_cols <- c(support_col, "Begin", "End")
      if (!all(required_cols %in% names(parsed))) {
        shiny::showNotification("CSV must contain columns: Scene, Begin, End.", type = "error")
        return()
      }

      prepared <- tryCatch(
        expand_support_points(
          parsed,
          col_support = support_col,
          col_begin = "Begin",
          col_end = "End"
        ),
        error = function(e) e
      )

      if (inherits(prepared, "error")) {
        shiny::showNotification(prepared$message, type = "error")
        return()
      }

      manual_data(list(
        project_name = input$manual_project,
        raw = parsed,
        prepared = prepared
      ))
      shiny::removeModal()
      shiny::showNotification("Custom timeline loaded.", type = "message")
    })

    manual_data
  })
}
