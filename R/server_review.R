# ---- Batch review (Edit → Batch review) ----
# Called from server(). Finds trips whose recorded path doesn't follow its
# network (R/utils_review.R), shows each one with a proposed clean path, and
# exports the accepted fixes as one GPX with one <trk> per trip.
review_server <- function(input, output, session, tr, current_lang) {
  REVIEW_GROUPS <- c("review_ref", "review_recorded", "review_proposed", "review_markers", "review_samples")
  SAMPLE_POINT <- "__sample__"  # station select: keep the recorded point

  rv <- shiny::reactiveValues(
    trips = NULL,           # review_trips() result, without samples
    matches = list(),       # OSRM matches by trip id (road activities)
    status = character(0),  # by trip id: flagged, ok, fixed, accepted, skipped
    accepted = list(),      # by trip id: list(activity, path = data.frame(lon, lat, t), method)
    checked = character(0),
    selected = NULL,
    proposal = NULL,        # fix for the selected trip
    params = NULL
  )
  samples_cache <- new.env(parent = emptyenv())  # trip samples (this session)
  proposals <- new.env(parent = emptyenv())      # fixes by trip id: no repeated Google calls
  road_base <- new.env(parent = emptyenv())      # default road proposals by trip id (from the inventory)
  road_alt <- new.env(parent = emptyenv())       # road match + proposal by "id profile", other OSRM profiles
  overrides <- new.env(parent = emptyenv())      # by trip id: origin_idx, dest_idx, dep, arr, offsets, reroute, pick, profile
  bulk_pending <- new.env(parent = emptyenv())
  resume_state <- shiny::reactiveVal(NULL)

  notify <- function(msg, type = "message", duration = 6) shiny::showNotification(msg, type = type, duration = duration)
  trip_row <- function(id) {
    trips <- shiny::isolate(rv$trips)
    trips[trips$id == id, , drop = FALSE]
  }
  family_of <- function(activity) REVIEW_ACTIVITIES[[activity]]$family
  # OSRM profile of a road trip: chosen in the profile select, or the activity's
  road_profile <- function(id) get_ov(id)$profile %||% REVIEW_ACTIVITIES[[trip_row(id)$activity]]$osrm
  local_time <- function(t, fmt) format(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"), fmt, tz = Sys.timezone())

  trip_samples <- function(id) {
    if (!is.null(samples_cache[[id]])) return(samples_cache[[id]])
    s <- review_clean_samples(trip_samples_for(trip_row(id))[[1]])
    assign(id, s, envir = samples_cache)
    s
  }
  get_ov <- function(id) overrides[[id]] %||% list(start_offset = 0, end_offset = 0)
  set_ov <- function(id, values) {
    o <- get_ov(id)
    for (nm in names(values)) o[nm] <- list(values[[nm]])
    assign(id, o, envir = overrides)
  }

  save_state <- function() {
    shiny::isolate(save_review_state(list(
      params = rv$params, trips = rv$trips, status = rv$status,
      accepted = rv$accepted, checked = rv$checked
    )))
  }

  ensure_rail_reference <- function(force = FALSE) {
    if (!force && !is.null(rail_reference())) return(TRUE)
    res <- shiny::withProgress(message = tr("Building the rail reference..."), value = 0, {
      build_rail_reference(on_step = function(f, label) shiny::setProgress(f, detail = label))
    })
    if (isTRUE(res$ok)) {
      notify(sprintf(tr("Rail reference built: %s"), res$message), duration = 10)
    } else {
      notify(res$message, type = "error", duration = NULL)
    }
    isTRUE(res$ok)
  }
  shiny::observeEvent(input$rebuild_rail_reference, ensure_rail_reference(force = TRUE))

  # ---- Proposals ----

  # Fix for one trip. Rail: Google transit between the stations (one call per
  # endpoint choice, cached), then times. Road: the default proposal (OSRM
  # match, built by the inventory); after "Re-route" / "Set as start/end", an
  # OSRM route between the endpoints, or, when the default is a possible
  # detour, a route through the recorded path offered next to the match
  # (fx$candidates; o$pick chooses). Another OSRM profile (profile select)
  # matches the trip again with it.
  compute_proposal <- function(id, refresh_route = FALSE) {
    row <- trip_row(id)
    if (nrow(row) == 0) return(NULL)
    act <- row$activity
    s <- trip_samples(id)
    if (nrow(s) < 2) {
      notify(tr("Not enough samples to build a path for this trip."), type = "warning")
      return(NULL)
    }
    o <- get_ov(id)
    prev <- proposals[[id]]

    if (family_of(act) == "rail") {
      if (!ensure_rail_reference()) return(NULL)
      plan <- rail_plan(s, act, o$origin_idx, o$dest_idx, o$dep, o$arr)
      key <- paste(round(unlist(plan$points), 6), collapse = ",")
      route <- if (!refresh_route && !is.null(prev$route) && identical(prev$route_key, key)) prev$route else NULL
      if (is.null(route)) {
        if (!check_google_api()) {
          notify(tr("Google Maps API key not configured or invalid. Set GOOGLE_MAPS_API_KEY in your .Renviron."),
                 type = "error", duration = 8)
          return(NULL)
        }
        r <- shiny::withProgress(message = tr("Calculating transit route..."), value = 0.5, {
          calculate_google_transit_route(plan$points, REVIEW_ACTIVITIES[[act]]$google, ref_time_utc = plan$ref_time_utc)
        })
        if (is.null(r)) {
          notify(sprintf(tr("No %s route found between these points. The API did not return routes with this transport mode."),
                         tr(REVIEW_ACTIVITIES[[act]]$label)), type = "error", duration = 8)
          return(NULL)
        }
        route <- list(coords = r$coords, steps = r$steps)  # steps: for the piecewise timing
      }
      fx <- rail_fix(plan, route, s, act, start_offset_s = o$start_offset, end_offset_s = o$end_offset)
      fx$plan <- plan
      fx$route_key <- key
    } else {
      profile <- o$profile %||% REVIEW_ACTIVITIES[[act]]$osrm
      if (identical(profile, REVIEW_ACTIVITIES[[act]]$osrm)) {
        m <- shiny::isolate(rv$matches[[id]])
        base <- road_base[[id]]
        if (is.null(base)) {
          base <- road_proposal(s, m, act)
          if (!is.null(base)) assign(id, base, envir = road_base)
        }
      } else {
        alt <- road_alt[[paste(id, profile)]]
        if (is.null(alt)) {
          alt <- shiny::withProgress(message = tr("Matching to the roads..."), value = 0.5, {
            m <- osrm_match(cbind(s$lon, s$lat), s$t, match_radiuses(s$acc), profile = profile)
            list(match = m, base = road_proposal(s, m, act, profile = profile))
          })
          assign(paste(id, profile), alt, envir = road_alt)
        }
        m <- alt$match
        base <- alt$base
      }
      from <- o$origin_idx %||% 1L
      to <- min(max(o$dest_idx %||% nrow(s), from + 1L), nrow(s))
      fx <- base
      if (isTRUE(o$reroute) && isTRUE(base$detour)) {
        via <- if (!refresh_route && identical(prev$via_key, c(from, to, profile))) prev$candidates$via else NULL
        if (is.null(via)) {
          via <- shiny::withProgress(message = tr("Routing through the recorded path..."), value = 0.5, {
            via_timed_path(s, from, to, profile)
          })
          if (!is.null(via)) via <- road_proposal_check(via, s, act, from, to, profile = profile)
        }
        if (is.null(via)) {
          notify(tr("Could not route through the recorded path (OSRM)."), type = "error")
        } else {
          pick <- if (identical(o$pick, "match")) "match" else "via"
          fx <- if (pick == "via") via else base
          fx$candidates <- list(match = base, via = via)
          fx$pick <- pick
          fx$via_key <- c(from, to, profile)
        }
      } else if (isTRUE(o$reroute) || is.null(base)) {
        fx <- route_timed_path(s, from, to, profile)
        if (!is.null(fx)) {
          if (!isTRUE(o$reroute)) fx$method <- "route_fallback"
          fx <- road_proposal_check(fx, s, act, from, to, profile = profile)
        }
      }
      if (is.null(fx)) {
        notify(tr("Could not build a path for this trip (OSRM)."), type = "error")
        return(NULL)
      }
      matched <- startsWith(fx$method, "match")
      fx$plan <- list(origin_idx = if (matched) 1L else from, dest_idx = if (matched) nrow(s) else to)
      fx$confidence <- m$confidence %||% NA_real_
      fx$profile <- profile
    }
    fx$id <- id
    fx$activity <- act
    fx$recorded_km <- fx$recorded_km %||% row$path_km
    assign(id, fx, envir = proposals)
    fx
  }

  select_trip <- function(id, refresh_route = FALSE) {
    rv$selected <- id
    fx <- if (!refresh_route) proposals[[id]] else NULL
    if (is.null(fx)) fx <- compute_proposal(id, refresh_route)
    rv$proposal <- fx
  }

  # Default road proposals from the inventory (summaries with their paths)
  seed_road_base <- function(ids, props) {
    rm(list = ls(road_base), envir = road_base)
    for (k in seq_along(ids)) {
      p <- props[[k]]
      if (is.null(p$path) || isTRUE(p$failed)) next
      assign(ids[k], c(p, road_proposal_status(p)), envir = road_base)
    }
  }

  recompute_selected <- function(refresh_route = FALSE) {
    id <- shiny::isolate(rv$selected)
    if (is.null(id)) return()
    rv$proposal <- compute_proposal(id, refresh_route)
  }

  # ---- Find ----

  shiny::observeEvent(input$review_find, {
    acts <- intersect(input$review_activities, names(REVIEW_ACTIVITIES))
    if (length(acts) == 0) {
      notify(tr("Choose at least one activity."), type = "error")
      return()
    }
    from <- input$review_from
    to <- input$review_to
    if (length(from) == 0 || length(to) == 0 || is.na(from) || is.na(to) || from > to) {
      notify(tr("Invalid date range."), type = "error")
      return()
    }

    if (isTRUE(input$review_fetch_icloud)) {
      res <- shiny::withProgress(message = tr("Fetching from iCloud..."), value = 0, {
        icloud_fetch_all(lang = current_lang(), on_step = function(f, label) shiny::setProgress(f, detail = label))
      })
      notify(res$message, type = if (isTRUE(res$ok)) "message" else "error", duration = if (isTRUE(res$ok)) 6 else NULL)
      if (!isTRUE(res$ok)) return()
    }
    if (any(vapply(acts, family_of, "") == "rail") && !ensure_rail_reference()) return()

    items <- load_items(ITEMS_BACKUP_DIR, from, to)
    if (nrow(items) == 0) {
      notify(tr("No timeline items found for these dates. Fetch everything from iCloud first."), type = "error", duration = 10)
      return()
    }
    codes <- vapply(acts, function(a) REVIEW_ACTIVITIES[[a]]$code, integer(1))
    trips <- trip_inventory(items, codes, with_samples = FALSE)
    res <- shiny::withProgress(message = tr("Measuring trips..."), value = 0, {
      review_trips(trips, progress = function(f, detail) shiny::setProgress(f, detail = detail))
    })
    matches <- attr(res, "matches")
    names(matches) <- res$id
    seed_road_base(res$id, attr(res, "proposals"))
    attr(res, "matches") <- NULL
    attr(res, "proposals") <- NULL
    res$samples <- NULL

    status <- ifelse(res$already_fixed, "fixed", ifelse(res$flagged, "flagged", "ok"))
    names(status) <- res$id
    prev <- shiny::isolate(rv$status)
    kept <- intersect(names(prev)[prev %in% c("accepted", "skipped")], res$id)
    status[kept] <- prev[kept]

    rv$trips <- res
    rv$matches <- matches
    rv$status <- status
    rv$selected <- NULL
    rv$proposal <- NULL
    rv$params <- list(activities = acts, from = from, to = to)
    resume_state(NULL)
    save_state()
    bslib::accordion_panel_open("review_steps", "review")
  })

  output$review_summary <- shiny::renderUI({
    current_lang()
    trips <- rv$trips
    if (is.null(trips)) return(NULL)
    acts <- intersect(names(REVIEW_ACTIVITIES), unique(trips$activity))
    if (length(acts) == 0) return(shiny::div(class = "review-summary", tr("No trips of these types in this period.")))
    shiny::div(
      class = "review-summary",
      lapply(acts, function(a) {
        sub <- trips[trips$activity == a, ]
        shiny::div(sprintf(tr("%s: %d trips · %d off-network · %d already fixed"),
                           tr(REVIEW_ACTIVITIES[[a]]$label), nrow(sub), sum(sub$flagged), sum(sub$already_fixed)))
      })
    )
  })

  # ---- Resume a previous review ----

  shiny::observeEvent(input$mode, {
    if (!identical(input$mode, "BatchReview") || !is.null(rv$trips) || !is.null(resume_state())) return()
    st <- load_review_state()
    if (!is.null(st) && length(st$accepted) > 0 && !is.null(st$trips)) resume_state(st)
  })

  output$review_resume <- shiny::renderUI({
    current_lang()
    st <- resume_state()
    if (is.null(st)) return(NULL)
    shiny::actionButton("review_resume_btn", sprintf(tr("Resume previous review (%d accepted)"), length(st$accepted)),
                        class = "btn-sm btn-outline-primary w-100 mb-2")
  })

  shiny::observeEvent(input$review_resume_btn, {
    st <- resume_state()
    if (is.null(st)) return()
    cache <- if (file.exists(review_cache_file())) readRDS(review_cache_file()) else list()
    matches <- lapply(st$trips$cache_key, function(k) cache[[k]]$match)
    names(matches) <- st$trips$id
    seed_road_base(st$trips$id, lapply(st$trips$cache_key, function(k) cache[[k]]$proposal))

    rv$trips <- st$trips
    rv$matches <- matches
    rv$status <- st$status
    rv$accepted <- st$accepted
    rv$checked <- st$checked %||% character(0)
    rv$params <- st$params
    rv$selected <- NULL
    rv$proposal <- NULL
    if (!is.null(st$params)) {
      shiny::updateSelectizeInput(session, "review_activities", selected = st$params$activities)
      shiny::updateDateInput(session, "review_from", value = st$params$from)
      shiny::updateDateInput(session, "review_to", value = st$params$to)
    }
    resume_state(NULL)
    bslib::accordion_panel_open("review_steps", "review")
    notify(sprintf(tr("Previous review resumed: %d accepted."), length(st$accepted)))
  })

  # ---- List ----

  visible_trips <- shiny::reactive({
    trips <- rv$trips
    if (is.null(trips) || nrow(trips) == 0) return(trips)
    status <- rv$status[trips$id]
    if (!isFALSE(input$review_only_flagged)) {
      trips <- trips[trips$flagged | status %in% c("accepted", "skipped"), , drop = FALSE]
    }
    ord <- if (identical(input$review_sort, "severity")) order(-trips$severity, trips$start_utc) else order(trips$start_utc)
    trips[ord, , drop = FALSE]
  })

  status_badge <- function(status) {
    switch(status,
      flagged  = c(tr("off-network"), "bg-danger-subtle text-danger-emphasis"),
      ok       = c(tr("OK"), "bg-success-subtle text-success-emphasis"),
      fixed    = c(tr("already fixed"), "bg-secondary-subtle text-secondary-emphasis"),
      accepted = c(tr("accepted"), "bg-primary-subtle text-primary-emphasis"),
      skipped  = c(tr("skipped"), "bg-light text-secondary"),
      c(status, "")
    )
  }

  detour_text <- function(proposed_km, recorded_km, ratio) {
    sprintf(tr("Possible detour: the proposal is %.2f× the recorded length (%.1f vs %.1f km). Re-route offers a path through the recording."),
            ratio, proposed_km, recorded_km)
  }
  short_text <- function(proposed_km, recorded_km, ratio) {
    sprintf(tr("Possibly skips part of the trip: the proposal is %.2f× the recorded length (%.1f vs %.1f km)."),
            ratio, proposed_km, recorded_km)
  }
  fragment_text <- function(path_km) {
    sprintf(tr("Fragment: only %.2f km recorded. A GPX import can't remove data: delete this trip or mark it as bogus in Arc."),
            path_km)
  }

  output$review_list <- shiny::renderUI({
    lang <- current_lang()
    trips <- visible_trips()
    if (is.null(trips)) return(shiny::div(class = "review-empty", tr("Find trips first (step 1).")))
    if (nrow(trips) == 0) return(shiny::div(class = "review-empty", tr("No trips to show.")))
    selected <- rv$selected %||% ""
    checked <- rv$checked
    status <- rv$status
    esc <- htmltools::htmlEscape
    day_fmt <- if (lang == "pt") "%d/%m" else "%m-%d"

    col <- function(nm) trips[[nm]] %||% rep(FALSE, nrow(trips))
    fragment <- col("fragment")
    detour <- col("detour")
    short <- col("short_proposal")
    rows <- vapply(seq_len(nrow(trips)), function(i) {
      id <- trips$id[i]
      st_i <- unname(status[id])
      badge <- status_badge(if (is.na(st_i)) "ok" else st_i)
      tip_i <- NULL
      # Flagged trips: fragment, then the proposal's length verdicts
      if (identical(st_i, "flagged")) {
        if (isTRUE(fragment[i])) {
          badge <- c(tr("fragment"), "rv-fragment")
          tip_i <- fragment_text(trips$path_km[i])
        } else if (isTRUE(detour[i])) {
          badge <- c(tr("detour?"), "rv-detour")
          tip_i <- detour_text(trips$proposed_km[i], trips$path_km[i], trips$len_ratio[i])
        } else if (isTRUE(short[i])) {
          badge <- c(tr("skips part?"), "rv-detour")
          tip_i <- short_text(trips$proposed_km[i], trips$path_km[i], trips$len_ratio[i])
        }
      }
      badge_tip <- if (is.null(tip_i)) "" else sprintf(' title="%s"', esc(tip_i))
      reasons <- format_reasons(trips$reasons[[i]], lang)
      tip <- paste0(fmt_datetime(lubridate::with_tz(trips$start_utc[i], Sys.timezone()), lang),
                    if (nzchar(reasons)) paste0(" · ", reasons) else "")
      longest <- trips$longest_off_m[i]
      sprintf(
        paste0('<div class="review-row%s" data-id="%s" title="%s"><input type="checkbox" class="rv-check"%s>',
               '<span>%s</span><span>%s</span><span>%s</span><span class="rv-num">%s</span>',
               '<span class="rv-num">%s</span><span class="rv-badge %s"%s>%s</span></div>'),
        if (id == selected) " selected" else "", esc(id), esc(tip), if (id %in% checked) " checked" else "",
        REVIEW_ACTIVITIES[[trips$activity[i]]]$icon,
        local_time(trips$start_utc[i], day_fmt),
        paste0(local_time(trips$start_utc[i], "%H:%M"), "–", local_time(trips$end_utc[i], "%H:%M")),
        if (is.na(trips$off_share[i])) "–" else sprintf("%.0f%%", 100 * trips$off_share[i]),
        if (is.na(longest)) "–" else if (longest >= 1000) sprintf("%.1fk", longest / 1000) else sprintf("%.0f", longest),
        badge[2], badge_tip, esc(badge[1])
      )
    }, character(1))
    shiny::HTML(paste(rows, collapse = ""))
  })

  shiny::observeEvent(input$review_select, {
    id <- input$review_select
    if (is.null(rv$trips) || !id %in% rv$trips$id) return()
    select_trip(id)
  })

  shiny::observeEvent(input$review_check, {
    ev <- input$review_check
    rv$checked <- if (isTRUE(ev$checked)) union(rv$checked, ev$id) else setdiff(rv$checked, ev$id)
    save_state()
  })

  # ---- Detail of the selected trip ----

  output$review_detail <- shiny::renderUI({
    lang <- current_lang()
    id <- rv$selected
    if (is.null(id)) return(shiny::div(class = "review-detail mode-hint", tr("Select a trip in the list.")))
    row <- trip_row(id)
    if (nrow(row) == 0) return(NULL)
    fx <- rv$proposal
    act <- row$activity
    reasons <- format_reasons(row$reasons[[1]], lang)
    when <- paste0(local_time(row$start_utc, if (lang == "pt") "%d/%m" else "%m-%d"), " ",
                   local_time(row$start_utc, "%H:%M"), "–", local_time(row$end_utc, "%H:%M"))
    # Rail: the station names share the title line (full text in the tooltip)
    stations <- if (!is.null(fx) && identical(fx$id, id) && family_of(act) == "rail") {
      name_of <- function(st) if (is.null(st)) tr("recorded point") else st$name
      paste(name_of(fx$plan$dep), "→", name_of(fx$plan$arr))
    }
    title <- shiny::div(
      class = "rv-title", title = paste(c(fmt_datetime(lubridate::with_tz(row$start_utc, Sys.timezone()), lang), stations), collapse = " · "),
      paste(REVIEW_ACTIVITIES[[act]]$icon, when),
      if (!is.null(stations)) shiny::span(class = "rv-stations", paste0("· ", stations))
    )
    # Road trips: their OSRM profile, on the title line
    if (family_of(act) == "road") {
      title <- shiny::div(
        class = "rv-title-row", title,
        shiny::div(class = "review-profile", title = tr("Routing profile for this trip"),
                   shiny::selectInput("review_profile", NULL, choices = tr_choices(profile_choices(REVIEW_PROFILE_CHOICES), lang),
                                      selected = road_profile(id), width = "84px"))
      )
    }
    fragment <- isTRUE(row$fragment)
    parts <- list(
      title,
      if (nzchar(reasons)) shiny::div(class = "text-secondary rv-clamp", title = reasons, reasons),
      if (fragment) {
        shiny::div(class = "rv-clamp", title = fragment_text(row$path_km),
                   shiny::span(class = "rv-badge rv-fragment", tr("fragment")),
                   tr("Delete it or mark it as bogus in Arc: a GPX import can't remove data."))
      }
    )

    if (is.null(fx) || !identical(fx$id, id)) {
      return(shiny::div(class = "review-detail", parts, shiny::div(class = "rv-warn", tr("No proposal for this trip."))))
    }
    length_line <- sprintf(tr("Proposed %.1f km · recorded %.1f km"),
                           fx$proposed_km %||% (path_cumulative_m(cbind(fx$path$lon, fx$path$lat))[nrow(fx$path)] / 1000),
                           fx$recorded_km)

    if (family_of(act) == "rail") {
      plan <- fx$plan
      end_row <- function(which, label, input_id, picked, options, time) {
        choices <- c(stats::setNames(options$name, sprintf("%s (%.0f m)", options$name, options$dist_m)),
                     stats::setNames(SAMPLE_POINT, tr("recorded point")))
        adj <- function(delta, text) {
          shiny::tags$button(type = "button", class = "btn btn-outline-secondary",
                             `data-review-adj` = paste0(which, ":", delta), text)
        }
        shiny::div(
          class = "review-end",
          shiny::span(class = "dt-label", label),
          shiny::selectInput(input_id, NULL, choices = choices,
                             selected = if (is.null(picked)) SAMPLE_POINT else picked$name, width = "100%"),
          shiny::span(class = "rv-num", local_time(time, "%H:%M:%S")),
          shiny::div(class = "btn-group btn-group-sm", adj(-60, "−1m"), adj(-30, "−30s"), adj(30, "+30s"), adj(60, "+1m"))
        )
      }
      parts <- c(parts, list(
        end_row("start", tr("Start"), "review_dep", plan$dep, plan$dep_options, fx$t_start),
        end_row("end", tr("End"), "review_arr", plan$arr, plan$arr_options, fx$t_end),
        # Legs (or the fallback note); the lengths go in its tooltip to save a line
        if (identical(fx$timing, "steps")) {
          tip <- sprintf(tr("Google: %s. Scaled ×%.2f to fit the recorded times."),
                         format_rail_legs(fx$legs, lang, google = TRUE), fx$scale)
          shiny::div(class = "rv-clamp", title = paste0(length_line, "\n", tip), format_rail_legs(fx$legs, lang))
        } else {
          note <- tr("Timing by distance: Google returned no step durations.")
          shiny::div(class = "text-secondary rv-clamp", title = paste0(length_line, "\n", note), note)
        },
        if (!isTRUE(fx$on_network)) {
          warn <- paste(tr("Route off the rails:"), format_reasons(fx$route_reasons, lang))
          shiny::div(class = "rv-warn rv-clamp", title = warn, warn)
        }
      ))
    } else {
      method <- switch(fx$method,
        match = sprintf(tr("Matched to the roads (confidence %.2f)"), fx$confidence),
        "match+gaps" = sprintf(tr("Matched to the roads (confidence %.2f), gaps routed"), fx$confidence),
        route = tr("Routed between start and end"),
        route_fallback = tr("No road match: routed between start and end"),
        via = sprintf(tr("Routed through the recorded path (%d waypoints)"), fx$n_waypoints),
        fx$method
      )
      ratio <- if (is.na(fx$len_ratio %||% NA)) "" else sprintf(" (%.2f×)", fx$len_ratio)
      rematch <- if (is.na(fx$rematch_off_share %||% NA)) "" else
        sprintf(tr("Re-matched: %.0f%% off the roads"), 100 * fx$rematch_off_share)
      cand_btn <- function(key, label, cand) {
        shiny::tags$button(
          type = "button", `data-review-pick` = key,
          class = paste("btn", if (identical(fx$pick, key)) "btn-primary" else "btn-outline-secondary"),
          title = sprintf("%s: %.1f km (%.2f×)", label, cand$proposed_km, cand$len_ratio),
          sprintf("%s · %.1f km", label, cand$proposed_km)
        )
      }
      parts <- c(parts, list(
        shiny::div(class = "rv-clamp", title = method, method),
        if (!isTRUE(fx$on_network)) shiny::div(class = "rv-warn", tr("Part of the path is a straight line (OSRM route failed).")),
        shiny::div(
          class = "text-secondary rv-clamp", title = rematch,
          if (!fragment && isTRUE(fx$detour)) {
            shiny::span(class = "rv-badge rv-detour", title = detour_text(fx$proposed_km, fx$recorded_km, fx$len_ratio),
                        tr("possible detour"))
          },
          if (!fragment && isTRUE(fx$short)) {
            shiny::span(class = "rv-badge rv-detour", title = short_text(fx$proposed_km, fx$recorded_km, fx$len_ratio),
                        tr("possibly skips part of the trip"))
          },
          paste0(length_line, ratio, " · ", local_time(fx$path$t[1], "%H:%M:%S"), "–",
                 local_time(fx$path$t[nrow(fx$path)], "%H:%M:%S"))
        ),
        if (!is.null(fx$candidates)) {
          shiny::div(class = "btn-group btn-group-sm review-cands",
                     cand_btn("match", tr("Matched"), fx$candidates$match),
                     cand_btn("via", tr("Via the recording"), fx$candidates$via))
        }
      ))
    }
    shiny::div(class = "review-detail", parts)
  })

  # Station selects, time adjustments, start/end from the map
  station_changed <- function(which, value) {
    id <- rv$selected
    fx <- rv$proposal
    if (is.null(id) || is.null(fx) || is.null(value) || family_of(trip_row(id)$activity) != "rail") return()
    current <- if (which == "dep") fx$plan$dep else fx$plan$arr
    current_name <- if (is.null(current)) SAMPLE_POINT else current$name
    if (identical(value, current_name)) return()
    set_ov(id, stats::setNames(list(if (identical(value, SAMPLE_POINT)) "" else value), which))
    recompute_selected()
  }
  shiny::observeEvent(input$review_dep, station_changed("dep", input$review_dep), ignoreInit = TRUE)
  shiny::observeEvent(input$review_arr, station_changed("arr", input$review_arr), ignoreInit = TRUE)

  shiny::observeEvent(input$review_time_adj, {
    id <- rv$selected
    ev <- input$review_time_adj
    if (is.null(id) || is.null(rv$proposal)) return()
    key <- if (identical(ev$which, "start")) "start_offset" else "end_offset"
    o <- get_ov(id)
    set_ov(id, stats::setNames(list((o[[key]] %||% 0) + as.numeric(ev$delta)), key))
    recompute_selected()
  })

  shiny::observeEvent(input$review_set_endpoint, {
    id <- rv$selected
    ev <- input$review_set_endpoint
    if (is.null(id)) return()
    idx <- as.integer(ev$idx)
    s <- trip_samples(id)
    if (is.na(idx) || idx < 1 || idx > nrow(s)) return()
    rail <- family_of(trip_row(id)$activity) == "rail"
    if (identical(ev$which, "start")) {
      set_ov(id, list(origin_idx = idx, dep = NULL, reroute = !rail))
    } else {
      set_ov(id, list(dest_idx = idx, arr = NULL, reroute = !rail))
    }
    recompute_selected()
  })

  shiny::observeEvent(input$review_pick, {
    id <- rv$selected
    if (is.null(id) || is.null(rv$proposal$candidates)) return()
    set_ov(id, list(pick = input$review_pick$key))
    recompute_selected()
  })

  # ---- Accept / skip / re-route / keep ----

  next_pending <- function(after_id) {
    trips <- visible_trips()
    if (is.null(trips) || nrow(trips) == 0) return(NULL)
    status <- rv$status[trips$id]
    pos <- match(after_id, trips$id)
    if (is.na(pos)) pos <- 0
    cand <- which(status == "flagged" & seq_along(status) > pos)
    if (length(cand) == 0) cand <- which(status == "flagged")
    if (length(cand) == 0) NULL else trips$id[cand[1]]
  }

  set_status_and_next <- function(id, status) {
    st <- rv$status
    st[[id]] <- status
    rv$status <- st
    save_state()
    nxt <- next_pending(id)
    if (!is.null(nxt) && !identical(nxt, id)) select_trip(nxt)
  }

  shiny::observeEvent(input$review_accept, {
    id <- rv$selected
    fx <- rv$proposal
    if (is.null(id) || is.null(fx) || !identical(fx$id, id)) {
      notify(tr("No proposal to accept for this trip."), type = "warning")
      return()
    }
    acc <- rv$accepted
    acc[[id]] <- list(activity = fx$activity, path = fx$path[, c("lon", "lat", "t")], method = fx$method)
    rv$accepted <- acc
    set_status_and_next(id, "accepted")
  })

  shiny::observeEvent(input$review_skip, {
    id <- rv$selected
    if (is.null(id)) return()
    acc <- rv$accepted
    acc[[id]] <- NULL
    rv$accepted <- acc
    set_status_and_next(id, "skipped")
  })

  shiny::observeEvent(input$review_keep, {
    id <- rv$selected
    if (is.null(id)) return()
    acc <- rv$accepted
    acc[[id]] <- NULL
    rv$accepted <- acc
    set_status_and_next(id, "ok")
  })

  shiny::observeEvent(input$review_reroute, {
    id <- rv$selected
    if (is.null(id)) return()
    if (family_of(trip_row(id)$activity) == "road") set_ov(id, list(reroute = TRUE))
    recompute_selected(refresh_route = TRUE)
  })

  # OSRM profile of the selected road trip (select on the detail's title line)
  shiny::observeEvent(input$review_profile, {
    id <- rv$selected
    if (is.null(id) || nrow(trip_row(id)) == 0 || family_of(trip_row(id)$activity) != "road") return()
    if (identical(input$review_profile, road_profile(id))) return()
    set_ov(id, list(profile = input$review_profile, pick = NULL))
    recompute_selected()
  }, ignoreInit = TRUE)

  # ---- Bulk accept (road activities only) ----

  # Flagged road trips whose default proposal passes road_proposal_status()
  bulk_candidates <- function() {
    trips <- rv$trips
    if (is.null(trips) || is.null(trips$bulk_ok)) return(character(0))
    status <- rv$status[trips$id]
    trips$id[vapply(trips$activity, family_of, "") == "road" & status %in% "flagged" & trips$bulk_ok]
  }

  output$review_bulk <- shiny::renderUI({
    current_lang()
    trips <- rv$trips
    if (is.null(trips) || !any(vapply(trips$activity, family_of, "") == "road")) return(NULL)
    lim <- REVIEW_THRESHOLDS$road
    shiny::div(
      class = "review-bulk",
      shiny::tags$small(
        title = sprintf(tr("Flagged road trips whose proposal is %.1f–%.1f× the recorded length and stays on the roads when matched again. Fragments, possible detours, proposals that may skip part of the trip and trips re-routed by hand are left out."),
                        lim$short_ratio, lim$bulk_max_ratio),
        sprintf(tr("Roads: %d safe proposals (%.1f–%.1f× the recorded length)"),
                length(bulk_candidates()), lim$short_ratio, lim$bulk_max_ratio)
      ),
      shiny::actionButton("review_bulk_accept", tr("Accept all"), class = "btn-sm btn-outline-success")
    )
  })

  shiny::observeEvent(input$review_bulk_accept, {
    ids <- bulk_candidates()
    lim <- REVIEW_THRESHOLDS$road
    fixes <- list()
    if (length(ids)) {
      shiny::withProgress(message = tr("Building proposals..."), value = 0, {
        for (k in seq_along(ids)) {
          shiny::setProgress(k / length(ids))
          # The current proposal: trips re-routed by hand (method route/via) don't pass
          fx <- proposals[[ids[k]]] %||% tryCatch(compute_proposal(ids[k]), error = function(e) NULL)
          if (isTRUE(fx$bulk_ok)) fixes[[ids[k]]] <- fx
        }
      })
    }
    assign("fixes", fixes, envir = bulk_pending)
    shiny::showModal(shiny::modalDialog(
      title = tr("Accept in bulk"),
      sprintf(tr("Accept %d flagged road trips? Each proposal is %.1f–%.1f× the recorded length and stays on the roads when matched again (≤ %.0f%% off). Fragments, possible detours, proposals that may skip part of the trip and trips re-routed by hand are left for review."),
              length(fixes), lim$short_ratio, lim$bulk_max_ratio, 100 * lim$bulk_max_off_share),
      footer = shiny::tagList(
        shiny::modalButton(tr("Cancel")),
        if (length(fixes)) shiny::actionButton("review_bulk_confirm", tr("Accept"), class = "btn-success")
      )
    ))
  })

  shiny::observeEvent(input$review_bulk_confirm, {
    fixes <- bulk_pending$fixes %||% list()
    shiny::removeModal()
    if (length(fixes) == 0) return()
    acc <- rv$accepted
    st <- rv$status
    for (id in names(fixes)) {
      acc[[id]] <- list(activity = fixes[[id]]$activity, path = fixes[[id]]$path[, c("lon", "lat", "t")],
                        method = fixes[[id]]$method)
      st[[id]] <- "accepted"
    }
    rv$accepted <- acc
    rv$status <- st
    save_state()
    notify(sprintf(tr("%d trips accepted."), length(fixes)))
  })

  # ---- Map ----

  draw_selected <- function() {
    proxy <- leaflet::leafletProxy("map")
    for (g in REVIEW_GROUPS) proxy <- leaflet::clearGroup(proxy, g)
    id <- rv$selected
    if (is.null(id) || !identical(input$mode, "BatchReview")) return()
    row <- trip_row(id)
    if (nrow(row) == 0) return()
    act <- row$activity
    s <- trip_samples(id)
    if (nrow(s) < 2) return()
    color <- REVIEW_ACTIVITIES[[act]]$color
    fx <- rv$proposal
    lang <- shiny::isolate(current_lang())

    # Reference network: rails (thin grey) or the matched roads (dashed)
    if (family_of(act) == "rail") {
      ref <- rail_lines_near(act, c(min(s$lon), min(s$lat), max(s$lon), max(s$lat)), 4326, buffer_m = 2000)
      if (length(ref)) proxy <- leaflet::addPolylines(proxy, data = ref, color = "#757575", weight = 2, opacity = 0.6,
                                                      group = "review_ref")
      cf <- conformity(s, act)
    } else {
      m <- rv$matches[[id]]
      if (isTRUE(m$ok)) {
        for (p in m$pieces) {
          proxy <- leaflet::addPolylines(proxy, lng = p$coords[, 1], lat = p$coords[, 2], color = "#455A64", weight = 3,
                                         opacity = 0.5, dashArray = "4 4", group = "review_ref")
        }
      }
      cf <- if (isTRUE(m$ok)) conformity(s, act, match = m) else list(dens = NULL)
    }

    # Recorded path coloured by offset: grey within tol_m, red beyond
    if (!is.null(cf$dens)) {
      ll <- from_metric(cf$dens$x, cf$dens$y, cf$crs)
      runs <- rle(cf$dens$off)
      ends <- cumsum(runs$lengths)
      starts <- ends - runs$lengths + 1
      for (off in c(FALSE, TRUE)) {
        k <- which(runs$values == off)
        if (length(k) == 0) next
        idx <- unlist(lapply(k, function(j) c(seq(max(1, starts[j] - 1), ends[j]), NA)))
        proxy <- leaflet::addPolylines(
          proxy, lng = ll[idx, 1], lat = ll[idx, 2],
          color = if (off) "#D32F2F" else "#9E9E9E", weight = if (off) 4 else 3, opacity = 0.9,
          group = "review_recorded"
        )
      }
    } else {
      proxy <- leaflet::addPolylines(proxy, lng = s$lon, lat = s$lat, color = "#9E9E9E", weight = 3, group = "review_recorded")
    }

    # Recorded samples (right-click: set as start / end)
    proxy <- leaflet::addCircleMarkers(
      proxy, lng = s$lon, lat = s$lat, radius = 3, stroke = FALSE, fillColor = "#616161", fillOpacity = 0.7,
      layerId = paste0("rvs_", seq_len(nrow(s))), group = "review_samples",
      label = paste0(local_time(s$t, "%H:%M:%S"), " · ±", round(s$acc), " m")
    )

    # Proposed clean path and its ends
    if (!is.null(fx) && identical(fx$id, id)) {
      p <- fx$path
      if (!is.null(fx$candidates)) {
        other <- fx$candidates[[if (identical(fx$pick, "via")) "match" else "via"]]$path
        proxy <- leaflet::addPolylines(proxy, lng = other$lon, lat = other$lat, color = color, weight = 3, opacity = 0.6,
                                       dashArray = "6 6", group = "review_proposed")
      }
      proxy <- leaflet::addPolylines(proxy, lng = p$lon, lat = p$lat, color = color, weight = 5, opacity = 0.85,
                                     group = "review_proposed")
      plan <- fx$plan
      label_end <- function(st, t) paste0(if (!is.null(st)) paste0(st$name, " · ") else "", local_time(t, "%H:%M:%S"))
      proxy <- leaflet::addCircleMarkers(
        proxy, lng = c(p$lon[1], p$lon[nrow(p)]), lat = c(p$lat[1], p$lat[nrow(p)]), radius = 7,
        color = "white", weight = 2, fillColor = c("#2E7D32", "#C62828"), fillOpacity = 1,
        label = c(label_end(plan$dep, p$t[1]), label_end(plan$arr, p$t[nrow(p)])), group = "review_markers"
      )
      oi <- plan$origin_idx %||% 1L
      di <- plan$dest_idx %||% nrow(s)
      proxy <- leaflet::addCircleMarkers(
        proxy, lng = s$lon[c(oi, di)], lat = s$lat[c(oi, di)], radius = 5, color = c("#2E7D32", "#C62828"),
        weight = 2, fill = FALSE, label = c(tr("Start sample"), tr("End sample")), group = "review_markers"
      )
    }

    lons <- c(s$lon, if (!is.null(fx)) fx$path$lon)
    lats <- c(s$lat, if (!is.null(fx)) fx$path$lat)
    leaflet::fitBounds(proxy, min(lons), min(lats), max(lons), max(lats))
  }

  shiny::observe({
    rv$selected
    rv$proposal
    input$mode
    shiny::isolate(draw_selected())
  })

  # ---- Export ----

  export_set <- function() {
    acc <- rv$accepted
    if (isTRUE(input$review_only_checked)) acc <- acc[intersect(names(acc), rv$checked)]
    acc
  }

  output$review_export_count <- shiny::renderUI({
    current_lang()
    n <- length(rv$accepted)
    txt <- sprintf(tr("%d accepted"), n)
    if (isTRUE(input$review_only_checked)) {
      txt <- paste(txt, sprintf(tr("(%d of them checked)"), length(intersect(names(rv$accepted), rv$checked))))
    }
    shiny::div(class = "review-summary mb-1", txt)
  })

  write_batch_gpx <- function(acc, file) {
    inp <- review_gpx_input(acc)
    writeLines(build_gpx(inp$items, inp$samples), file)
  }

  output$review_download <- shiny::downloadHandler(
    filename = function() {
      acc <- export_set()
      if (length(acc) == 0) "arc_batch.gpx" else review_batch_filename(acc)
    },
    content = function(file) {
      acc <- export_set()
      if (length(acc) == 0) {
        notify(tr("No accepted trips to export."), type = "error")
        return()
      }
      write_batch_gpx(acc, file)
      notify(sprintf(tr("Batch GPX: %d tracks."), length(acc)))
    }
  )

  shiny::observeEvent(input$review_send_icloud, {
    if (!icloud_available()) {
      notify(tr("iCloud (rclone) is not available."), type = "error")
      return()
    }
    acc <- export_set()
    if (length(acc) == 0) {
      notify(tr("No accepted trips to export."), type = "error")
      return()
    }
    gpx_name <- review_batch_filename(acc)
    gpx_path <- file.path(tempfile("arc_batch_"), gpx_name)
    dir.create(dirname(gpx_path))
    write_batch_gpx(acc, gpx_path)
    res <- shiny::withProgress(message = tr("Sending GPX to iCloud..."), value = 0.3, {
      icloud_push_file(gpx_path, subdir = "GPX", lang = current_lang())
    })
    unlink(dirname(gpx_path), recursive = TRUE)
    if (!isTRUE(res$ok)) {
      notify(res$message, type = "error", duration = NULL)
      return()
    }
    notify(sprintf(tr("GPX sent to iCloud: %s/GPX/%s. On the iPhone: Arc → timeline → Import GPX File"),
                   ICLOUD_ARC_DIR, gpx_name), duration = 15)
  })

  # Collapsed steps keep their text current (and the download link set)
  for (id in c("review_download", "review_summary", "review_export_count")) {
    shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  }

  # Select labels follow the language (values unchanged)
  shiny::observeEvent(current_lang(), {
    lang <- current_lang()
    shiny::updateSelectizeInput(session, "review_activities", choices = tr_choices(REVIEW_ACTIVITY_CHOICES, lang),
                                selected = shiny::isolate(input$review_activities))
    shiny::updateSelectInput(session, "review_sort", choices = tr_choices(REVIEW_SORT_CHOICES, lang),
                             selected = shiny::isolate(input$review_sort) %||% "date")
  }, ignoreInit = TRUE)

  invisible(NULL)
}
