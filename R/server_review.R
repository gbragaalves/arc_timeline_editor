# ---- Batch review (Edit → Batch review) ----
# Called from server(). Finds trips whose recorded path doesn't follow its
# network (R/utils_review.R), shows each one with a proposed clean path, and
# exports the accepted fixes as one GPX with one <trk> per trip.
review_server <- function(input, output, session, tr, current_lang) {
  REVIEW_GROUPS <- c("review_ref", "review_recorded", "review_proposed", "review_markers", "review_samples", "review_split",
                     "review_pins")
  SAMPLE_POINT <- "__sample__"  # station select: keep the recorded point

  rv <- shiny::reactiveValues(
    trips = NULL,           # review_trips() result, without samples
    matches = list(),       # OSRM matches by trip id (road activities)
    status = character(0),  # by trip id: flagged, ok, fixed, accepted, skipped
    accepted = list(),      # by trip id: list(activity, path = data.frame(lon, lat, t), method)
    checked = character(0),
    pins = list(),          # control pins by trip id: data.frame(id, lon, lat, after) (road trips)
    route_ends = list(),    # moved route ends by trip id: list(start = c(lon, lat), end = c(lon, lat))
    exclusions = list(),    # samples kept out of /match by trip id: their times (epoch s)
    pin_mode = FALSE,       # route editing: drag the route, its ends and the pins
    lasso_mode = FALSE,     # lasso on the map: exclude samples from /match
    selected = NULL,
    proposal = NULL,        # fix for the selected trip
    params = NULL
  )
  samples_cache <- new.env(parent = emptyenv())  # trip samples (this session)
  proposals <- new.env(parent = emptyenv())      # fixes by trip id: no repeated Google calls
  road_base <- new.env(parent = emptyenv())      # default road proposals by trip id (from the inventory)
  road_alt <- new.env(parent = emptyenv())       # road match + proposal by "id profile", other OSRM profiles
  overrides <- new.env(parent = emptyenv())      # by trip id: origin_idx, dest_idx, dep, arr, offsets, reroute, pick, profile,
                                                 # split = list(on, alight_idx, after, osrm_walk)
  jae_all <- read_jae_boardings()                # Jaé taps: split a trip at its boarding
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
  # Jaé taps of the trip's mode inside its time range
  trip_taps <- function(id) {
    row <- trip_row(id)
    if (nrow(row) == 0) return(jae_all[0, ])
    multimodal_taps(jae_all, row$activity, row$start_utc, row$end_utc)
  }
  split_on <- function(id) isTRUE(get_ov(id)$split$on)
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
      accepted = rv$accepted, checked = rv$checked, pins = rv$pins, route_ends = rv$route_ends,
      exclusions = rv$exclusions
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
      # Jaé gate tap vs the computed start (without the manual offsets); while
      # split, a tap shortly after it moves the start to the tap
      taps <- trip_taps(id)
      gate <- if (nrow(taps)) multimodal_rail_gate(taps$datetime_utc[1], fx$t_start - (o$start_offset %||% 0))
      if (isTRUE(o$split$on) && identical(gate$action, "adjust")) {
        fx <- rail_fix(plan, route, s, act, start_offset_s = o$start_offset, end_offset_s = o$end_offset,
                       start_at = gate$start_at)
      }
      fx$gate <- gate
      fx$plan <- plan
      fx$route_key <- key
    } else {
      profile <- o$profile %||% REVIEW_ACTIVITIES[[act]]$osrm
      excl <- shiny::isolate(rv$exclusions[[id]])
      if (length(excl)) {
        # Samples excluded by lasso: matched again without them (cached per set)
        key <- paste(id, profile, paste(sort(round(excl)), collapse = ","))
        alt <- road_alt[[key]]
        if (is.null(alt)) {
          alt <- shiny::withProgress(message = tr("Matching to the roads..."), value = 0.5, {
            m <- review_match(s, profile, exclude = excl)
            list(match = m, base = road_proposal(s, m, act, profile = profile))
          })
          assign(key, alt, envir = road_alt)
        }
        m <- alt$match
        base <- alt$base
      } else if (identical(profile, REVIEW_ACTIVITIES[[act]]$osrm)) {
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
            m <- review_match(s, profile)
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
      pins <- shiny::isolate(rv$pins[[id]])
      ends <- shiny::isolate(rv$route_ends[[id]])
      if ((!is.null(pins) && nrow(pins) > 0) || length(ends)) {
        # Control pins / moved ends: OSRM /route start -> pins -> end, timed by the recording
        pf <- shiny::withProgress(message = tr("Routing through the pins..."), value = 0.5, {
          pinned_route_fix(s, pins, profile, start = ends$start, end = ends$end)
        })
        if (is.null(pf)) {
          notify(tr("Could not route through the pins (OSRM)."), type = "error")
        } else {
          fx <- road_proposal_check(pf, s, act, 1L, nrow(s), profile = profile)
        }
      } else if (isTRUE(o$reroute) && isTRUE(base$detour)) {
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
      fx$ref_match <- m
    }
    fx$id <- id
    fx$activity <- act
    fx$recorded_km <- fx$recorded_km %||% row$path_km
    fx$split <- split_proposal(id, act, s, fx, o$split)
    assign(id, fx, envir = proposals)
    fx
  }

  # Split at the first Jaé tap (when switched on): walk -> wait -> bus / rail
  # leg -> after. Bus: needs the alighting sample (map click). Rail: the wait
  # runs until the rail fix's departure; a tap after it is ignored.
  split_proposal <- function(id, act, s, fx, sp) {
    if (!isTRUE(sp$on)) return(NULL)
    taps <- trip_taps(id)
    if (nrow(taps) == 0) return(NULL)
    tap <- taps[1, ]
    walk <- if (isTRUE(sp$osrm_walk)) "osrm" else "recorded"
    out <- list(tap = tap, after = sp$after %||% "walk", osrm_walk = isTRUE(sp$osrm_walk), alight_idx = sp$alight_idx)
    rail <- family_of(act) == "rail"
    if (rail && identical(fx$gate$action, "ignore")) {
      out$ignored <- TRUE
      return(out)
    }
    # The wait's stop: the rail leg's departure station, else from the tap point (see stop_name_for)
    dep <- if (rail) fx$plan$dep$name
    out$stop <- if (!is.null(dep) && nzchar(dep)) list(name = dep, source = "station") else
      stop_name_for(tap$longitude, tap$latitude, lang = current_lang())
    if (!rail && is.null(sp$alight_idx)) return(out)
    out$segments <- tryCatch(
      shiny::withProgress(message = tr("Splitting the trip..."), value = 0.5, {
        if (rail) {
          build_rail_multimodal_segments(s, tap, fx, act, walk)
        } else {
          build_multimodal_segments(s, tap, c(s$lon[sp$alight_idx], s$lat[sp$alight_idx]), out$after, walk)
        }
      }),
      error = function(e) {
        notify(paste(tr("Could not split the trip:"), conditionMessage(e)), type = "error")
        NULL
      }
    )
    out
  }

  select_trip <- function(id, refresh_route = FALSE) {
    if (!identical(shiny::isolate(rv$selected), id) && shiny::isolate(rv$pin_mode)) set_pin_mode(FALSE)
    if (!identical(shiny::isolate(rv$selected), id) && shiny::isolate(rv$lasso_mode)) set_lasso_mode(FALSE)
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
    rv$pins <- st$pins %||% list()
    rv$route_ends <- st$route_ends %||% list()
    rv$exclusions <- st$exclusions %||% list()
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

  # Jaé tap line with the split button; while split: the alighting (bus), the
  # OSRM foot fallback, the after-alighting choice (bus) and the segments
  split_parts <- function(id, act, fx, lang) {
    taps <- trip_taps(id)
    if (nrow(taps) == 0) return(list())
    tap <- taps[1, ]
    rail <- family_of(act) == "rail"
    what <- if (rail) tap$label else paste(tr("line"), trimws(tap$linha_ou_estacao))
    jae_txt <- paste0("Jaé: ", jae_local_time(tap$datetime_utc), " · ", what,
                      if (nrow(taps) > 1) sprintf(" (+%d)", nrow(taps) - 1) else "")
    jae_span <- shiny::span(class = "rv-clamp", title = paste0(jae_local_time(tap$datetime_utc, "%H:%M:%S"), " · ", tap$label),
                            jae_txt)
    if (rail && identical(fx$gate$action, "ignore")) {
      msg <- sprintf(tr("The Jaé tap (%s) is more than %d min after the rail leg's start (%s): tap ignored."),
                     jae_local_time(tap$datetime_utc, "%H:%M:%S"), REVIEW_THRESHOLDS$rail_gate_max_late_s %/% 60,
                     local_time(fx$path$t[1], "%H:%M:%S"))
      return(list(shiny::div(class = "rv-jae", jae_span), shiny::div(class = "rv-warn rv-clamp", title = msg, msg)))
    }
    on <- split_on(id)
    # Split with the start moved to the gate tap: said on the Jaé line (the station is in the title)
    if (on && rail && identical(fx$gate$action, "adjust")) {
      jae_span <- shiny::span(class = "rv-clamp", title = paste0(jae_local_time(tap$datetime_utc, "%H:%M:%S"), " · ", tap$label),
                              paste0("Jaé: ", jae_local_time(tap$datetime_utc), " · ", tr("start adjusted to Jaé tap")))
    }
    leg <- if (rail) act else "bus"
    btn <- shiny::tags$button(
      type = "button", `data-review-split` = "toggle", title = if (on) tr("Cancel split"),
      class = paste("btn btn-sm", if (on) "btn-outline-secondary rv-split-cancel" else "btn-outline-primary"),
      if (on) "\u00d7" else sprintf(tr("Split: walk → wait → %s"), tolower(tr(REVIEW_ACTIVITIES[[leg]]$label)))
    )
    out <- list(shiny::div(class = paste("rv-jae", if (on) "rv-split-on"), jae_span, btn))
    sp <- fx$split
    if (!on || is.null(sp)) return(out)
    chips <- if (length(sp$segments)) {
      shiny::div(class = "rv-segs", lapply(sp$segments, function(g) {
        shiny::span(class = "rv-seg", style = sprintf("border-left-color: %s;", segment_color(g, act)),
                    title = sprintf("%s · %s–%s", g$name, local_time(g$t_start, "%H:%M:%S"), local_time(g$t_end, "%H:%M:%S")),
                    sprintf("%s %.1f min", segment_label(g, act), (g$t_end - g$t_start) / 60))
      }))
    }

    osrm_box <- shiny::div(
      class = "rv-split-osrm", title = tr("Fallback: route the walks with OSRM foot instead of the recorded samples."),
      shiny::checkboxInput("review_split_osrm", "OSRM", isTRUE(sp$osrm_walk))
    )
    if (rail) {
      # The segments and the fallback share a line (the departure is in the start row)
      # The wait takes the departure station (its select in the start row): no stop row
      return(c(out, list(shiny::div(class = "rv-split-row", chips %||% shiny::span(), osrm_box))))
    } else {
      alight <- if (is.null(sp$alight_idx)) {
        shiny::span(class = "rv-hint", sprintf(tr("Click the map where you got off (after %s)."),
                                               jae_local_time(tap$datetime_utc, "%H:%M:%S")))
      } else {
        shiny::span(sprintf(tr("Alighting: %s"), local_time(trip_samples(id)$t[sp$alight_idx], "%H:%M:%S")))
      }
      out <- c(out, list(
        shiny::div(class = "rv-split-row", alight, osrm_box),
        shiny::div(class = "rv-split-after",
                   shiny::radioButtons("review_split_after", NULL, inline = TRUE, selected = sp$after,
                                       choiceNames = list(tr("Walk (recorded)"), tr("Visit at destination"), tr("Leave as is")),
                                       choiceValues = list("walk", "visit", "none")))
      ))
    }
    c(out, list(chips, stop_row(id, sp)))
  }

  # The bus wait's stop name (track name and wait note), editable, with its source
  split_stop <- function(id, sp) {
    edited <- get_ov(id)$split$stop_name
    if (!is.null(edited) && nzchar(edited)) return(list(name = edited, source = "edited"))
    sp$stop
  }
  stop_row <- function(id, sp) {
    if (is.null(sp$stop) || !any(vapply(sp$segments, function(g) identical(g$role, "wait"), logical(1)))) return(NULL)
    shiny::div(
      class = "rv-split-row rv-split-stop",
      shiny::span(tr("Stop")),
      shiny::textInput("review_split_stop", NULL, split_stop(id, sp)$name, updateOn = "blur"),
      shiny::span(class = "text-secondary", title = tr("Name source"), shiny::textOutput("review_split_stop_src", inline = TRUE))
    )
  }
  # Its source follows an edit without re-rendering the detail
  stop_edits <- shiny::reactiveVal(0)
  output$review_split_stop_src <- shiny::renderText({
    stop_edits()
    id <- rv$selected
    fx <- rv$proposal
    if (is.null(id) || is.null(fx) || !identical(fx$id, id) || is.null(fx$split$stop)) return("")
    st <- split_stop(id, fx$split)
    if (identical(st$source, "edited")) tr("edited") else stop_source_label(st$source, current_lang())
  })
  # Ambiguous pins (near two passes of the recording): where each goes, by hand
  pin_order_rows <- function(fx) {
    op <- fx$pins
    if (!identical(fx$method, "pins") || is.null(op) || !any(op$ambiguous)) return(NULL)
    lapply(which(op$ambiguous), function(k) {
      others <- setdiff(seq_len(nrow(op)), k)
      current <- if (k == 1) "start" else op$id[k - 1]
      opts <- c(list(shiny::tags$option(value = "start", selected = if (current == "start") NA, tr("Start"))),
                lapply(others, function(j) shiny::tags$option(value = op$id[j], selected = if (op$id[j] == current) NA,
                                                              sprintf(tr("Pin %d"), j))))
      shiny::div(class = "rv-pin-amb", sprintf(tr("Pin %d is near two passes; after:"), k),
                 shiny::tags$select(class = "form-select form-select-sm", `data-review-pin-after` = op$id[k], opts))
    })
  }

  segment_label <- function(g, act) {
    switch(g$role, walk_to_stop = , walk_from_stop = tr("Walk"), wait = tr("Wait"), bus = tr("Bus"),
           at_destination = tr("At destination"), rail = tr(REVIEW_ACTIVITIES[[act]]$label), g$role)
  }
  segment_color <- function(g, act) {
    if (identical(g$role, "rail")) return(REVIEW_ACTIVITIES[[act]]$color)
    if (identical(g$role, "at_destination")) return(MULTIMODAL_COLORS[["visit"]])
    MULTIMODAL_COLORS[[g$type]]
  }

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
    # Road trips: their OSRM profile, on the title line (not while split: the bus leg uses bus)
    if (family_of(act) == "road" && !split_on(id)) {
      n_pins <- nrow(rv$pins[[id]] %||% data.frame())
      n_excl <- length(rv$exclusions[[id]])
      title <- shiny::div(
        class = "rv-title-row", title,
        shiny::tags$button(
          id = "review_pin_toggle", type = "button", `data-review-pin` = "toggle",
          class = paste("btn btn-sm rv-pin-btn", if (isTRUE(rv$pin_mode)) "btn-primary" else "btn-outline-secondary"),
          title = tr("Edit the route (P): drag the line to add a via pin, drag the start, the end or a pin; right-click or double-click a pin to remove it; Ctrl+Z undoes."),
          paste0("\U0001F4CD", if (n_pins > 0) n_pins else "")
        ),
        shiny::tags$button(
          id = "review_lasso_toggle", type = "button", `data-review-pin` = "lasso",
          class = paste("btn btn-sm rv-pin-btn", if (isTRUE(rv$lasso_mode)) "btn-primary" else "btn-outline-secondary"),
          title = tr("Exclude samples from the matching (L): circle jittery samples (e.g. stopped at a light); they still time the path. Ctrl+Z undoes."),
          paste0("\u27B0", if (n_excl > 0) n_excl else "")
        ),
        shiny::div(class = "review-profile", title = tr("Routing profile for this trip"),
                   shiny::selectInput("review_profile", NULL, choices = tr_choices(profile_choices(REVIEW_PROFILE_CHOICES), lang),
                                      selected = road_profile(id), width = "84px"))
      )
    }
    fragment <- isTRUE(row$fragment)
    parts <- list(
      title,
      # While split, the split rows take the reasons' line (also in the list row's tooltip)
      if (nzchar(reasons) && !split_on(id)) shiny::div(class = "text-secondary rv-clamp", title = reasons, reasons),
      if (fragment) {
        shiny::div(class = "rv-clamp", title = fragment_text(row$path_km),
                   shiny::span(class = "rv-badge rv-fragment", tr("fragment")),
                   tr("Delete it or mark it as bogus in Arc: a GPX import can't remove data."))
      }
    )

    if (is.null(fx) || !identical(fx$id, id)) {
      return(shiny::div(class = "review-detail", parts, shiny::div(class = "rv-warn", tr("No proposal for this trip."))))
    }
    parts <- c(parts, split_parts(id, act, fx, lang))
    if (family_of(act) == "road" && !is.null(fx$split)) return(shiny::div(class = "review-detail", parts))
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
        pins = sprintf(tr("Routed through the pins (%d)"), fx$n_pins),
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
        shiny::div(class = "rv-clamp", title = method, method,
                   if (identical(fx$method, "pins")) {
                     shiny::tagList(" · ", shiny::tags$a(href = "#", `data-review-pin` = "clear", tr("Clear pins")))
                   },
                   if (length(rv$exclusions[[id]])) {
                     shiny::tagList(" · ", shiny::tags$a(href = "#", `data-review-pin` = "clear_excl", tr("Clear exclusions")))
                   }),
        pin_order_rows(fx),
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
    sp <- fx$split
    if (!is.null(sp) && !isTRUE(sp$ignored) && is.null(sp$segments)) {
      notify(tr("Click the map where you got off first."), type = "warning")
      return()
    }
    acc <- rv$accepted
    acc[[id]] <- if (!is.null(sp$segments)) {
      segs <- multimodal_label_segments(sp$segments, split_stop(id, sp), jae_leg_text(sp$tap))
      multimodal_accepted(segs, fx$activity, tap = sp$tap)
    } else {
      list(activity = fx$activity, path = fx$path[, c("lon", "lat", "t")], method = fx$method)
    }
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
    if (family_of(trip_row(id)$activity) == "road") {
      set_ov(id, list(reroute = TRUE))
      if (split_on(id)) set_ov(id, list(split = utils::modifyList(get_ov(id)$split, list(on = FALSE))))
    }
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

  # ---- Split at the Jaé tap ----

  shiny::observeEvent(input$review_split, {
    id <- rv$selected
    if (is.null(id) || nrow(trip_taps(id)) == 0) return()
    sp <- get_ov(id)$split %||% list(after = "walk", osrm_walk = FALSE)
    set_ov(id, list(split = utils::modifyList(sp, list(on = !isTRUE(sp$on)))))
    recompute_selected()
  })

  split_option <- function(key, value, default) {
    id <- rv$selected
    if (is.null(id) || !split_on(id) || is.null(value)) return()
    sp <- get_ov(id)$split
    if (identical(sp[[key]] %||% default, value)) return()
    set_ov(id, list(split = utils::modifyList(sp, stats::setNames(list(value), key))))
    recompute_selected()
  }
  shiny::observeEvent(input$review_split_after, split_option("after", input$review_split_after, "walk"), ignoreInit = TRUE)
  # Stop name edited (sent on blur / Enter): kept unless it is the proposed name; no recompute
  shiny::observeEvent(input$review_split_stop, {
    id <- rv$selected
    fx <- rv$proposal
    if (is.null(id) || !split_on(id) || is.null(fx) || !identical(fx$id, id) || is.null(fx$split$stop)) return()
    name <- trimws(input$review_split_stop)
    if (!nzchar(name) || identical(name, fx$split$stop$name)) name <- NULL
    set_ov(id, list(split = utils::modifyList(get_ov(id)$split, list(stop_name = name), keep.null = TRUE)))
    stop_edits(stop_edits() + 1)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$review_split_osrm, split_option("osrm_walk", isTRUE(input$review_split_osrm), FALSE),
                      ignoreInit = TRUE)

  # Map click while splitting a road trip: the alighting, snapped to the
  # recorded sample nearest to it along the recording after the boarding
  set_alighting <- function(lon, lat) {
    id <- rv$selected
    if (is.null(id) || !split_on(id) || family_of(trip_row(id)$activity) != "road") return()
    taps <- trip_taps(id)
    if (nrow(taps) == 0 || is.null(lon) || is.null(lat)) return()
    s <- trip_samples(id)
    t_tap <- as.numeric(taps$datetime_utc[1])
    ib <- which(s$t >= t_tap)[1]
    if (is.na(ib)) return()
    ia <- multimodal_alighting_index(s, ib, c(lon, lat))
    # Snapped to a sample after the boarding: refused when that sample is far
    # from the click (e.g. a click on the walk before the boarding)
    far <- geosphere::distHaversine(c(lon, lat), c(s$lon[ia], s$lat[ia])) > 100
    if (s$t[ia] <= t_tap || far) {
      notify(sprintf(tr("The alighting must come after the boarding (%s)."), jae_local_time(taps$datetime_utc[1], "%H:%M:%S")),
             type = "warning")
      return()
    }
    sp <- get_ov(id)$split
    if (identical(sp$alight_idx, ia)) return()
    set_ov(id, list(split = utils::modifyList(sp, list(alight_idx = ia))))
    recompute_selected()
  }
  # While editing the route or drawing a lasso a click does nothing (pins come from drags)
  shiny::observeEvent(input$map_click, {
    if (!identical(input$mode, "BatchReview") || isTRUE(rv$pin_mode) || isTRUE(rv$lasso_mode)) return()
    set_alighting(input$map_click$lng, input$map_click$lat)
  })
  shiny::observeEvent(input$map_marker_click, {
    ev <- input$map_marker_click
    if (identical(input$mode, "BatchReview") && !isTRUE(rv$pin_mode) && !isTRUE(rv$lasso_mode) &&
        startsWith(as.character(ev$id %||% ""), "rvs_")) {
      set_alighting(ev$lng, ev$lat)
    }
  })

  # ---- Control pins (road trips) ----

  pin_trip <- function() {
    id <- rv$selected
    if (is.null(id) || nrow(trip_row(id)) == 0 || family_of(trip_row(id)$activity) != "road" || split_on(id)) return(NULL)
    id
  }
  set_pin_mode <- function(on) {
    rv$pin_mode <- isTRUE(on)
    if (isTRUE(on) && isTRUE(rv$lasso_mode)) set_lasso_mode(FALSE)
    session$sendCustomMessage("pin_mode", list(on = isTRUE(on), scope = "review"))
    send_route_edit()
  }
  set_lasso_mode <- function(on) {
    rv$lasso_mode <- isTRUE(on)
    if (isTRUE(on) && isTRUE(rv$pin_mode)) set_pin_mode(FALSE)
    session$sendCustomMessage("lasso_mode", list(on = isTRUE(on), scope = "review"))
  }
  shiny::observeEvent(input$mode, {
    if (!identical(input$mode, "BatchReview") && isTRUE(rv$pin_mode)) set_pin_mode(FALSE)
    if (!identical(input$mode, "BatchReview") && isTRUE(rv$lasso_mode)) set_lasso_mode(FALSE)
  })

  # Route edits of a trip (pins, moved ends, excluded samples), with an undo
  # history per trip (Ctrl+Z)
  edit_hist <- new.env(parent = emptyenv())
  set_named <- function(all, id, value) {
    all[id] <- list(value)
    all[!vapply(all, is.null, logical(1))]
  }
  set_route_edits <- function(id, pins = rv$pins[[id]], ends = rv$route_ends[[id]], excl = rv$exclusions[[id]],
                              record = TRUE) {
    if (record) {
      h <- edit_hist[[id]] %||% list()
      h[[length(h) + 1L]] <- list(pins = rv$pins[[id]], ends = rv$route_ends[[id]], excl = rv$exclusions[[id]])
      assign(id, utils::tail(h, 30), envir = edit_hist)
    }
    rv$pins <- set_named(rv$pins, id, if (is.null(pins) || nrow(pins) == 0) NULL else pins)
    rv$route_ends <- set_named(rv$route_ends, id, if (length(ends)) ends else NULL)
    rv$exclusions <- set_named(rv$exclusions, id, if (length(excl)) sort(unique(excl)) else NULL)
    save_state()
    recompute_selected()
  }
  set_pins <- function(id, pins) set_route_edits(id, pins = pins)
  add_pin <- function(lon, lat) {
    id <- pin_trip()
    if (is.null(id) || is.null(lon) || is.null(lat)) return()
    pins <- rv$pins[[id]] %||% data.frame(id = character(0), lon = numeric(0), lat = numeric(0), after = character(0))
    new_id <- as.character(max(c(0L, suppressWarnings(as.integer(pins$id))), na.rm = TRUE) + 1L)
    set_pins(id, rbind(pins, data.frame(id = new_id, lon = lon, lat = lat, after = NA_character_)))
  }
  # Drag ends (route editing): the line -> a new via pin; the start / end markers -> moved route ends
  shiny::observeEvent(input$review_route_drag, {
    ev <- input$review_route_drag
    id <- pin_trip()
    if (is.null(id) || !isTRUE(rv$pin_mode)) return()
    lon <- as.numeric(ev$lng)
    lat <- as.numeric(ev$lat)
    if (identical(ev$kind, "line")) {
      add_pin(lon, lat)
    } else if (ev$kind %in% c("start", "end")) {
      ends <- rv$route_ends[[id]] %||% list()
      ends[[ev$kind]] <- c(lon, lat)
      set_route_edits(id, ends = ends)
    }
  })
  shiny::observeEvent(input$route_undo, {
    if (!identical(input$route_undo$scope, "review") || !identical(input$mode, "BatchReview")) return()
    id <- pin_trip()
    h <- if (is.null(id)) NULL else edit_hist[[id]]
    if (!length(h)) {
      notify(tr("Nothing to undo."), duration = 3)
      return()
    }
    last <- h[[length(h)]]
    assign(id, h[-length(h)], envir = edit_hist)
    set_route_edits(id, pins = last$pins, ends = last$ends, excl = last$excl, record = FALSE)
  })
  # Lasso: the samples inside are kept out of /match (they still time the path)
  shiny::observeEvent(input$review_lasso_samples, {
    id <- pin_trip()
    if (is.null(id) || !isTRUE(rv$lasso_mode)) return()
    s <- trip_samples(id)
    k <- suppressWarnings(as.integer(sub("^rvs_", "", unlist(input$review_lasso_samples$ids))))
    k <- k[!is.na(k) & k >= 1 & k <= nrow(s)]
    if (length(k) == 0) return()
    set_route_edits(id, excl = c(rv$exclusions[[id]], s$t[k]))
    notify(sprintf(tr("%d samples kept out of the matching (Ctrl+Z undoes)."), length(k)), duration = 4)
  })

  # Route editing handles for the client: the proposed path, its ends and the
  # control points in travel order (start, pins, end)
  send_route_edit <- function() {
    id <- shiny::isolate(rv$selected)
    fx <- shiny::isolate(rv$proposal)
    on <- isTRUE(shiny::isolate(rv$pin_mode)) && !is.null(id) && !is.null(fx) && identical(fx$id, id) &&
      !is.null(fx$path) && nrow(fx$path) >= 2 && is.null(fx$split)
    msg <- list(scope = "review", on = on)
    if (on) {
      p <- fx$path
      n <- nrow(p)
      pins <- fx$pins %||% data.frame(id = character(0), lon = numeric(0), lat = numeric(0))
      msg$ctrl <- c(list(list(id = "start", lat = p$lat[1], lng = p$lon[1])),
                    lapply(seq_len(nrow(pins)), function(k) list(id = as.character(pins$id[k]), lat = pins$lat[k], lng = pins$lon[k])),
                    list(list(id = "end", lat = p$lat[n], lng = p$lon[n])))
      msg$path <- lapply(seq_len(n), function(k) c(p$lat[k], p$lon[k]))
    }
    session$sendCustomMessage("route_edit", msg)
  }
  edit_pin <- function(ev, change) {
    id <- pin_trip()
    pins <- if (is.null(id)) NULL else rv$pins[[id]]
    k <- if (is.null(pins)) NA else match(as.character(ev$id), pins$id)
    if (is.na(k)) return()
    set_pins(id, change(pins, k))
  }

  shiny::observeEvent(input$review_pin, {
    action <- input$review_pin$action
    id <- pin_trip()
    if (identical(action, "toggle")) {
      if (is.null(id)) {
        if (isTRUE(rv$pin_mode)) set_pin_mode(FALSE)
        return()
      }
      set_pin_mode(!isTRUE(rv$pin_mode))
      if (isTRUE(rv$pin_mode)) {
        notify(tr("Route editing: drag the line to add a via pin, drag the start or the end; right-click or double-click a pin removes it (P to leave)."),
               duration = 5)
      }
    } else if (identical(action, "clear") && !is.null(id)) {
      set_pin_mode(FALSE)
      set_route_edits(id, pins = NULL, ends = NULL)
    } else if (identical(action, "lasso")) {
      if (is.null(id)) {
        if (isTRUE(rv$lasso_mode)) set_lasso_mode(FALSE)
        return()
      }
      set_lasso_mode(!isTRUE(rv$lasso_mode))
      if (isTRUE(rv$lasso_mode)) {
        notify(tr("Lasso: circle the jittery samples to keep them out of the matching (L to leave)."), duration = 5)
      }
    } else if (identical(action, "clear_excl") && !is.null(id)) {
      set_route_edits(id, excl = NULL)
    }
  })
  shiny::observeEvent(input$review_pin_moved, edit_pin(input$review_pin_moved, function(p, k) {
    p$lon[k] <- as.numeric(input$review_pin_moved$lng)
    p$lat[k] <- as.numeric(input$review_pin_moved$lat)
    p
  }))
  shiny::observeEvent(input$review_pin_remove, edit_pin(input$review_pin_remove, function(p, k) p[-k, , drop = FALSE]))
  shiny::observeEvent(input$review_pin_after, edit_pin(list(id = input$review_pin_after$pin), function(p, k) {
    p$after[k] <- as.character(input$review_pin_after$after)
    p
  }))

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
      m <- if (!is.null(fx) && identical(fx$id, id) && !is.null(fx$ref_match)) fx$ref_match else rv$matches[[id]]
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

    # Recorded samples (right-click: set as start / end); kept out of /match: grey with an outline
    excl <- round(s$t) %in% round(rv$exclusions[[id]] %||% numeric(0))
    proxy <- leaflet::addCircleMarkers(
      proxy, lng = s$lon, lat = s$lat, radius = ifelse(excl, 4, 3), stroke = excl, color = "#37474F", weight = 1.5,
      fillColor = ifelse(excl, "#CFD8DC", "#616161"), fillOpacity = ifelse(excl, 0.9, 0.7),
      layerId = paste0("rvs_", seq_len(nrow(s))), group = "review_samples",
      label = paste0(local_time(s$t, "%H:%M:%S"), " · ±", round(s$acc), " m",
                     ifelse(excl, paste0(" · ", tr("kept out of the matching")), ""))
    )

    # Jaé boarding of a trip with a tap of its mode
    taps <- trip_taps(id)
    taps <- taps[!is.na(taps$longitude) & !is.na(taps$latitude), ]
    if (nrow(taps)) {
      tap <- taps[1, ]
      jae_color <- unname(JAE_MODE_COLORS[tap$modo])
      proxy <- leaflet::addCircleMarkers(
        proxy, lng = tap$longitude, lat = tap$latitude, radius = 8, color = "white", weight = 2,
        fillColor = if (is.na(jae_color)) "#1565C0" else jae_color, fillOpacity = 1, group = "review_split",
        label = paste0("Jaé ", jae_local_time(tap$datetime_utc, "%H:%M:%S"), " · ", tap$label),
        options = leaflet::pathOptions(bubblingMouseEvents = FALSE)
      )
    }

    # Control pins, numbered in travel order
    pins <- rv$pins[[id]]
    if (!is.null(pins) && nrow(pins) > 0 && family_of(act) == "road") {
      ord <- if (!is.null(fx) && identical(fx$id, id) && !is.null(fx$pins)) fx$pins$id else pins$id
      proxy <- leaflet::addMarkers(
        proxy, lng = pins$lon, lat = pins$lat, layerId = paste0("pin_", pins$id), group = "review_pins",
        label = as.character(match(pins$id, ord)),
        labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "top", offset = c(-15, -18), className = "pin-label"),
        options = leaflet::markerOptions(draggable = TRUE)
      )
    }

    split <- if (!is.null(fx) && identical(fx$id, id) && !is.null(fx$split) && !isTRUE(fx$split$ignored)) fx$split
    if (!is.null(split)) {
      # Split preview: each segment in its colour (walks dashed, waits and visits as dots), the alighting sample
      for (g in split$segments) {
        col <- segment_color(g, act)
        tip <- sprintf("%s · %s–%s", segment_label(g, act), local_time(g$t_start, "%H:%M:%S"), local_time(g$t_end, "%H:%M:%S"))
        proxy <- if (g$type == "stationary") {
          leaflet::addCircleMarkers(proxy, lng = g$path$lon[1], lat = g$path$lat[1], radius = 9, color = "white", weight = 2,
                                    fillColor = col, fillOpacity = 0.95, label = tip, group = "review_split",
                                    options = leaflet::pathOptions(bubblingMouseEvents = FALSE))
        } else {
          leaflet::addPolylines(proxy, lng = g$path$lon, lat = g$path$lat, color = col, weight = if (g$type == "walking") 4 else 5,
                                opacity = 0.9, dashArray = if (g$type == "walking") "2 7" else NULL, label = tip,
                                group = "review_split")
        }
      }
      if (!is.null(split$alight_idx)) {
        ai <- split$alight_idx
        proxy <- leaflet::addCircleMarkers(
          proxy, lng = s$lon[ai], lat = s$lat[ai], radius = 7, color = "#C62828", weight = 3, fill = FALSE,
          label = sprintf(tr("Alighting: %s"), local_time(s$t[ai], "%H:%M:%S")), group = "review_split",
          options = leaflet::pathOptions(bubblingMouseEvents = FALSE)
        )
      }
    } else if (!is.null(fx) && identical(fx$id, id)) {
      # Proposed clean path and its ends
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

    send_route_edit()
    # Fit the view to a newly selected trip only (not after each edit)
    if (!identical(fitted$id, id)) {
      fitted$id <- id
      lons <- c(s$lon, if (!is.null(fx)) fx$path$lon, if (nrow(taps)) taps$longitude[1])
      lats <- c(s$lat, if (!is.null(fx)) fx$path$lat, if (nrow(taps)) taps$latitude[1])
      leaflet::fitBounds(proxy, min(lons), min(lats), max(lons), max(lats))
    }
  }
  fitted <- new.env(parent = emptyenv())

  shiny::observe({
    rv$selected
    rv$proposal
    rv$pins
    rv$exclusions
    input$mode
    shiny::isolate(draw_selected())
  })
  shiny::observeEvent(input$mode, if (!identical(input$mode, "BatchReview")) fitted$id <- NULL)

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
      notify(sprintf(tr("Batch GPX: %d tracks."), review_gpx_track_count(acc)))
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
    if (isTRUE(input$review_jae_notes)) send_jae_notes(acc)
  })

  # Notes of the split trips (ride legs, and waits with "Wait note") as a
  # notes-only import package synced to <Arc dir>/Import. Bus legs carry the
  # SMTR note (BigQuery, cached), their leg text when SMTR has nothing.
  send_jae_notes <- function(acc) {
    taps <- do.call(rbind, lapply(Filter(function(a) !is.null(a$tap), acc), `[[`, "tap"))
    cache <- smtr_cache()
    if (!is.null(taps) && nrow(smtr_taps(taps))) {
      pf <- shiny::withProgress(message = tr("Fetching SMTR bus data..."), value = 0, {
        smtr_prefetch(taps, progress = function(f, d) shiny::setProgress(f, detail = d))
      })
      cache <- pf$cache
      if (!is.null(pf$error)) {
        notify(paste(tr("SMTR data unavailable, bus notes keep the Jaé line text:"), pf$error), type = "warning",
               duration = 12)
      }
    }
    notes <- review_notes(acc, wait_note = isTRUE(input$review_wait_note),
                          ride_body = function(tap) smtr_note_body(tap, cache))
    if (length(notes) == 0) {
      notify(tr("No Jaé notes to send (no accepted split trips)."), type = "warning")
      return()
    }
    pkg <- tempfile("arc_notes_")
    write_notes_package_dir(notes, pkg)
    res <- shiny::withProgress(message = tr("Sending notes to iCloud..."), value = 0.5, {
      icloud_push_import(pkg, lang = current_lang())
    })
    unlink(pkg, recursive = TRUE)
    if (!isTRUE(res$ok)) {
      notify(res$message, type = "error", duration = NULL)
      return()
    }
    notify(sprintf(tr("%d notes sent to %s/Import. In Arc, import the GPX first, then the Import folder."),
                   length(notes), ICLOUD_ARC_DIR), duration = 15)
  }

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
