# ---- Server: Period review (Edit > Period) ----
# Loads every trip and visit of up to PERIOD_MAX_DAYS days (utils_period.R;
# month by month, cached in REVIEW_SCRATCH_DIR/period_cache)
# and sends them to the client (PERIOD_JS), which draws the map and the list
# and switches between the two views (now / recorded) without the server.
period_server <- function(input, output, session, tr, current_lang) {
  notify <- function(msg, type = "message", duration = 5) shiny::showNotification(msg, type = type, duration = duration)

  # The range by its own length, back or forward (the buttons by the dates), then a load
  shiny::observeEvent(input$period_step, {
    r <- review_step_range(input$period_from, input$period_to, input$period_step$dir)
    if (is.null(r)) return()
    shiny::updateDateInput(session, "period_from", value = r$from)
    shiny::updateDateInput(session, "period_to", value = r$to)
    session$sendCustomMessage("period_autoload", list())
  })

  shiny::observeEvent(input$period_load, {
    from <- as.Date(input$period_from)
    to <- as.Date(input$period_to)
    if (length(from) == 0 || length(to) == 0 || is.na(from) || is.na(to) || from > to) {
      notify(tr("Invalid date range."), type = "error")
      return()
    }
    days <- as.integer(to - from) + 1L
    if (days > PERIOD_MAX_DAYS) {
      notify(sprintf(tr("Choose at most %d days."), PERIOD_MAX_DAYS), type = "error")
      return()
    }
    if (isTRUE(input$period_fetch_icloud)) {
      res <- shiny::withProgress(message = tr("Fetching from iCloud..."), value = 0, {
        icloud_fetch_all(lang = current_lang(), on_step = function(f, label) shiny::setProgress(f, detail = label))
      })
      notify(res$message, type = if (isTRUE(res$ok)) "message" else "error", duration = if (isTRUE(res$ok)) 6 else NULL)
      if (!isTRUE(res$ok)) return()
    }
    tl <- shiny::withProgress(message = tr("Loading the period..."), value = 0, {
      period_load(from, to, progress = function(f, d) shiny::setProgress(0.95 * f, detail = d))
    })
    if (nrow(tl) == 0) notify(tr("No items in this period (local backup)."), type = "warning")
    taps <- tryCatch(period_taps(from, to), error = function(e) NULL)
    session$sendCustomMessage("period_data", list(cols = period_payload(tl), taps = taps, from = format(from), to = format(to),
                                                  tz = Sys.timezone()))
  })

  # A gap in the timeline filled from Review: Create > Visit or Create > Manual
  # route with its start and end (and the work date), the segment shown
  shiny::observeEvent(input$period_fill, {
    o <- input$period_fill
    s <- as.POSIXct(as.numeric(o$s), origin = "1970-01-01", tz = Sys.timezone())
    e <- as.POSIXct(as.numeric(o$e), origin = "1970-01-01", tz = Sys.timezone())
    if (length(s) != 1 || length(e) != 1 || is.na(s) || is.na(e) || e < s) return()
    if (identical(o$kind, "route")) {
      # a Jaé tap without a trip: Create > Route on its day (the day's boardings are listed there)
      shiny::updateDateInput(session, "work_date", value = as.Date(s, tz = Sys.timezone()))
      session$sendCustomMessage("period_to_create", list(segment = "OSRM"))
      return()
    }
    kind <- if (identical(o$kind, "trip")) "manual" else "visit"
    shiny::updateDateInput(session, "work_date", value = as.Date(s, tz = Sys.timezone()))
    shiny::updateDateInput(session, paste0(kind, "_date_start"), value = as.Date(s, tz = Sys.timezone()))
    shiny::updateTextInput(session, paste0(kind, "_time_start"), value = format(s, "%H:%M:%S"))
    shiny::updateDateInput(session, paste0(kind, "_date_end"), value = as.Date(e, tz = Sys.timezone()))
    shiny::updateTextInput(session, paste0(kind, "_time_end"), value = format(e, "%H:%M:%S"))
    session$sendCustomMessage("period_to_create", list(segment = if (kind == "manual") "Manual" else "Visit"))
  })

  # Sends: the GPX files sent, each track against the items backup (utils_sends.R)
  shiny::observeEvent(input$period_sends_req, {
    st <- tryCatch(shiny::withProgress(message = tr("Checking the GPX files sent..."), value = 0, {
      gpx_sends_status(progress = function(f, d) shiny::setProgress(f, detail = d))
    }), error = function(e) { notify(conditionMessage(e), type = "error"); NULL })
    if (!is.null(st)) session$sendCustomMessage("period_sends", sends_payload(st))
  })

  # Send a GPX again (its local copy), after a confirmation: Arc imports it over what is there
  resend_file <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$period_resend, {
    f <- basename(as.character(input$period_resend$file %||% ""))
    if (!nzchar(f) || !file.exists(file.path(GPX_ARCHIVE_DIR, f))) {
      notify(tr("No local copy of this file."), type = "error")
      return()
    }
    resend_file(f)
    shiny::showModal(shiny::modalDialog(
      title = tr("Send this GPX to iCloud again?"),
      shiny::p(shiny::tags$code(f)),
      shiny::p(tr("In Arc, Import GPX File puts it over what is there now.")),
      footer = shiny::tagList(shiny::modalButton(tr("Cancel")),
                              shiny::actionButton("period_resend_ok", tr("Send"), class = "btn-primary"))
    ))
  })
  shiny::observeEvent(input$period_resend_ok, {
    shiny::removeModal()
    f <- resend_file()
    if (is.null(f)) return()
    tmp <- file.path(tempfile("resend_"), f)
    dir.create(dirname(tmp))
    file.copy(file.path(GPX_ARCHIVE_DIR, f), tmp)
    res <- shiny::withProgress(message = tr("Sending..."), value = 0.5, icloud_push_file(tmp, "GPX", lang = current_lang()))
    notify(res$message, type = if (isTRUE(res$ok)) "message" else "error", duration = if (isTRUE(res$ok)) 6 else NULL)
  })
}
