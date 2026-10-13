# ---- Batch review (Edit → Batch review) ----
# Called from server(). Finds trips whose recorded path doesn't follow its
# network (R/utils_review.R), shows each one with a proposed clean path, and
# exports the accepted fixes as one GPX with one <trk> per trip.
review_server <- function(input, output, session, tr, current_lang) {
  REVIEW_GROUPS <- c("review_ref", "review_recorded", "review_proposed", "review_markers", "review_samples", "review_split",
                     "review_pins")
  SAMPLE_POINT <- "__sample__"  # station select: keep the recorded point
  XFER_NONE <- "__none__"       # transfer select: no station chosen yet
  XFER_MAX <- 6                 # transfer rows

  rv <- shiny::reactiveValues(
    trips = NULL,           # review_trips() result, without samples
    matches = list(),       # OSRM matches by trip id (road activities)
    status = character(0),  # by trip id: flagged, ok, fixed, accepted, skipped
    accepted = list(),      # by trip id: list(activity, path = data.frame(lon, lat, t), method)
    checked = character(0),
    pins = list(),          # control pins by trip id: data.frame(id, lon, lat, after, grab_lon, grab_lat, ww, turns)
    legs = list(),          # leg modes by trip id: c("<from>><to>" = "recorded" | "straight") (default: route)
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
  # An OSRM failure: during the data's maintenance window a notice instead of the error
  osrm_fail <- function(msg) {
    m <- osrm_maintenance()
    if (is.null(m)) notify(msg, type = "error") else notify(osrm_maintenance_notice(m, current_lang()), type = "warning", duration = 10)
  }
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
  # The Jaé gate tap of a rail trip (up to rail_tap_lead_s before its start;
  # not one before the end of a previous rail trip in the list), or NULL
  trip_rail_tap <- function(id) {
    row <- trip_row(id)
    if (nrow(row) == 0 || family_of(row$activity) != "rail") return(NULL)
    trips <- shiny::isolate(rv$trips)
    rail <- vapply(trips$activity, family_of, character(1)) == "rail"
    prev_end <- as.numeric(trips$end_utc[rail & trips$id != id & trips$end_utc <= row$start_utc])
    tap <- rail_trip_tap(jae_all, row$activity, row$start_utc, row$end_utc, after = if (length(prev_end)) max(prev_end) else -Inf)
    if (nrow(tap)) tap
  }
  # The fixed points (ROUTE_ANCHORS) at a car / taxi trip's ends, once per
  # trip and anchors' version
  anchor_cache <- new.env(parent = emptyenv())
  trip_anchor_ends_for <- function(id) {
    row <- trip_row(id)
    an <- route_anchors()
    if (nrow(row) == 0 || !row$activity %in% ANCHOR_ACTIVITIES || is.null(an)) return(list())
    key <- paste(id, .route_anchors$key)
    if (is.null(anchor_cache[[key]])) {
      assign(key, trip_anchor_ends(trip_samples(id), as.Date(row$start_utc, tz = Sys.timezone()), an), envir = anchor_cache)
    }
    anchor_cache[[key]]
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
      exclusions = rv$exclusions, legs = rv$legs
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
    # A rail trip with one sample still gets its station rows (times: the item's)
    if (nrow(s) < 2 && !(family_of(act) == "rail" && nrow(s) == 1)) {
      notify(tr("Not enough samples to build a path for this trip."), type = "warning")
      return(NULL)
    }
    o <- get_ov(id)
    prev <- proposals[[id]]

    if (family_of(act) == "rail") {
      if (!ensure_rail_reference()) return(NULL)
      # Stations (a Jaé tap's station starts it), Google transit between them,
      # else the route along the OSM rails; without a route, the stations
      # alone (fx$path NULL: the rows stay to choose them)
      google_fn <- function(points, mode, ref_time_utc, on_fail) {
        if (!check_google_api()) {
          on_fail("no_key")
          return(NULL)
        }
        calculate_google_transit_route(points, mode, ref_time_utc = ref_time_utc, on_fail = on_fail)
      }
      fx <- shiny::withProgress(message = tr("Calculating transit route..."), value = 0.5, {
        rail_proposal(s, act, c(row$start_utc, row$end_utc), o, tap = trip_rail_tap(id), prev = prev,
                      refresh_route = refresh_route, google_fn = google_fn)
      })
      if (identical(fx$google_fail$code, "no_key")) {
        notify(tr("Google Maps API key not configured or invalid. Set GOOGLE_MAPS_API_KEY in your .Renviron."),
               type = "warning", duration = 8)
      }
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
      # The ends: a dragged one, else a fixed point's (ROUTE_ANCHORS) for car / taxi trips
      user_ends <- shiny::isolate(rv$route_ends[[id]])
      te <- review_trip_ends(user_ends, trip_anchor_ends_for(id))
      ends <- te$ends
      if ((!is.null(pins) && nrow(pins) > 0) || length(ends)) {
        # Control pins / moved ends: local splices of the base path (the match
        # proposal), each leg routed once (leg_cache); timed by the recording
        pf <- shiny::withProgress(message = tr("Routing through the pins..."), value = 0.5, {
          if (!is.null(base$path) && nrow(base$path) >= 2) {
            splice_route_fix(s, base$path, pins, profile, ends = ends, legs = shiny::isolate(rv$legs[[id]]), reach = te$reach,
                             start_offset_s = o$start_offset %||% 0, end_offset_s = o$end_offset %||% 0,
                             bounds = c(as.numeric(row$prev_start_utc %||% NA), as.numeric(row$next_end_utc %||% NA)),
                             route_fn = cached_leg_route)
          } else {
            pinned_route_fix(s, pins, profile, start = ends$start, end = ends$end)
          }
        })
        if (is.null(pf)) {
          osrm_fail(tr("Could not route through the pins (OSRM)."))
        } else {
          fx <- road_proposal_check(pf, s, act, 1L, nrow(s), profile = profile)
          # only the fixed points changed it: as safe as the match it came from, while their splices stay short
          if ((is.null(pins) || nrow(pins) == 0) && !length(user_ends)) fx$bulk_ok <- isTRUE(base$bulk_ok) && anchor_splices_short(pf)
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
          osrm_fail(tr("Could not route through the recorded path (OSRM)."))
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
        osrm_fail(tr("Could not build a path for this trip (OSRM)."))
        return(NULL)
      }
      matched <- startsWith(fx$method, "match")
      fx$plan <- list(origin_idx = if (matched) 1L else from, dest_idx = if (matched) nrow(s) else to)
      fx$confidence <- m$confidence %||% NA_real_
      fx$profile <- profile
      fx$ends_info <- te$info
      fx$ends_eff <- te$ends
      fx$ref_match <- m
    }
    fx$id <- id
    fx$activity <- act
    fx$recorded_km <- fx$recorded_km %||% row$path_km
    fx$split <- split_proposal(id, act, s, fx, o$split)
    assign(id, fx, envir = proposals)
    fx
  }

  # Coverage of the original item (review_cover()): its span, recorded
  # samples, the fill choices, the wait's place (rail: the stations); a
  # departure from a Jaé tap stays where it is
  cover_args <- function(id, fx) {
    row <- trip_row(id)
    rail <- family_of(row$activity) == "rail"
    name_of <- function(st) if (rail && !is.null(st$name)) st$name else ""
    # an anchored end: the wait there is named after the anchor and sits at its node (merging into the visit)
    anchored <- function(w) {
      inf <- fx$ends_info[[w]]
      if (identical(inf$source, "anchor")) inf
    }
    list(span = c(as.numeric(row$start_utc), as.numeric(row$end_utc)), s = trip_samples(id),
         fill = get_ov(id)$fill %||% list(),
         places = list(before = if (rail) name_of(fx$plan$dep) else anchored("start")$anchor %||% "",
                       after = if (rail) name_of(fx$plan$arr) else anchored("end")$anchor %||% ""),
         keep_start = (rail && isTRUE(fx$start_at_tap)) || (!rail && !is.null(fx$t_start_new)),
         keep_end = !rail && !is.null(fx$t_end_new),
         wait_at = list(before = anchored("start")$wait, after = anchored("end")$wait))
  }
  cover_accept <- function(id, fx, entry) {
    a <- cover_args(id, fx)
    review_cover_entry(entry, a$span, a$s, a$fill, a$places, a$keep_start, keep_end = a$keep_end, wait_at = a$wait_at)
  }
  # The coverage of the current proposal (the split's segments while split)
  cover_preview <- function(id, fx) {
    if (is.null(fx$path)) return(NULL)
    main <- if (length(fx$split$segments) && !isTRUE(fx$split$ignored)) fx$split$segments else
      review_main_segments(list(activity = fx$activity, path = fx$path))
    a <- cover_args(id, fx)
    review_cover(main, a$span, a$s, a$fill, a$places, a$keep_start, keep_end = a$keep_end, wait_at = a$wait_at)
  }

  # Split at the first Jaé tap (when switched on): walk -> wait -> bus / rail
  # leg -> after. Bus: needs the alighting sample (map click). Rail: the wait
  # runs until the rail fix's departure; a tap after it is ignored.
  split_proposal <- function(id, act, s, fx, sp) {
    if (!isTRUE(sp$on) || is.null(fx$path)) return(NULL)
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
    if (!identical(shiny::isolate(rv$selected), id)) session$sendCustomMessage("station_pick_off", list())
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

  # The search: trips of acts from..to (dates), measured; the list on step 2
  run_find <- function(acts, from, to) {
    acts <- intersect(acts, names(REVIEW_ACTIVITIES))
    if (length(acts) == 0) {
      notify(tr("Choose at least one activity."), type = "error")
      return()
    }
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
    # The fixed trip-end points: resolved again when ROUTE_ANCHORS or the local OSM extract changed
    if (any(acts %in% ANCHOR_ACTIVITIES) && route_anchors_stale()) {
      res <- shiny::withProgress(message = tr("Resolving the fixed trip-end points..."), value = 0.5, resolve_route_anchors())
      if (!isTRUE(res$ok)) notify(paste(tr("Fixed trip-end points not resolved:"), res$message), type = "warning", duration = 10)
    }
    # Road trips need OSRM: not while its data is rebuilt (the failed matches would be cached)
    m <- osrm_maintenance()
    if (any(vapply(acts, family_of, "") == "road") && !is.null(m)) {
      notify(osrm_maintenance_notice(m, current_lang()), type = "warning", duration = 10)
      return()
    }

    items <- load_items(ITEMS_BACKUP_DIR, from, to)
    if (nrow(items) == 0) {
      notify(tr("No timeline items found for these dates. Fetch everything from iCloud first."), type = "error", duration = 10)
      return()
    }
    codes <- vapply(acts, function(a) REVIEW_ACTIVITIES[[a]]$code, integer(1))
    trips <- trip_inventory(items, codes, with_samples = FALSE)
    # The neighbouring items' times: a dragged start / end stops there
    nb <- items[!items$deleted & !items$disabled, ]
    nb <- nb[order(nb$start_utc), ]
    k <- match(trips$id, nb$id)
    trips$prev_start_utc <- nb$start_utc[ifelse(is.na(k) | k <= 1, NA, k - 1L)]
    trips$next_end_utc <- nb$end_utc[ifelse(is.na(k) | k >= nrow(nb), NA, k + 1L)]
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
  }
  shiny::observeEvent(input$review_find, run_find(input$review_activities, input$review_from, input$review_to))

  # The last trip types and "Only flagged" in this browser (localStorage, sent
  # by the client at page load), applied once; until the client reports those
  # types back, the language relabel keeps them too
  saved <- new.env(parent = emptyenv())
  shiny::observeEvent(input$review_saved_filters, {
    sv <- input$review_saved_filters
    acts <- intersect(as.character(unlist(sv$activities)), names(REVIEW_ACTIVITIES))
    if (length(acts)) {
      saved$acts <- acts
      shiny::updateSelectizeInput(session, "review_activities", selected = acts)
    }
    if (is.logical(sv$only_flagged) && length(sv$only_flagged) == 1) {
      shiny::updateCheckboxInput(session, "review_only_flagged", value = sv$only_flagged)
    }
  }, once = TRUE)
  shiny::observeEvent(input$review_activities, {
    if (!is.null(saved$acts) && setequal(input$review_activities, saved$acts)) saved$acts <- NULL
  })

  # "Everything in range": every trip type, flagged or not, by date; the
  # search runs once those show (after this flush)
  shiny::observeEvent(input$review_find_all, {
    acts <- names(REVIEW_ACTIVITIES)
    shiny::updateSelectizeInput(session, "review_activities", selected = acts)
    shiny::updateCheckboxInput(session, "review_only_flagged", value = FALSE)
    shiny::updateSelectInput(session, "review_sort", selected = "date")
    from <- input$review_from
    to <- input$review_to
    session$onFlushed(function() shiny::withReactiveDomain(session, shiny::isolate(run_find(acts, from, to))), once = TRUE)
  })

  # Day stepping (the step buttons, Alt+arrows): a one-day range moves one day,
  # a longer one by its own length; then the search
  shiny::observeEvent(input$review_step, {
    r <- review_step_range(input$review_from, input$review_to, input$review_step$dir)
    if (is.null(r)) return()
    session$userData$set_date_range("review_from", "review_to", r$from, r$to)
    run_find(input$review_activities, r$from, r$to)
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
    rv$legs <- st$legs %||% list()
    rv$route_ends <- st$route_ends %||% list()
    rv$exclusions <- st$exclusions %||% list()
    rv$params <- st$params
    rv$selected <- NULL
    rv$proposal <- NULL
    if (!is.null(st$params)) {
      shiny::updateSelectizeInput(session, "review_activities", selected = st$params$activities)
      session$userData$set_date_range("review_from", "review_to", st$params$from, st$params$to)
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
    if (on && rail && isTRUE(fx$gate$action %in% c("adjust", "station"))) {
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
          title = tr("Edit the route (P): drag the line to add a pin (only the stretch around it changes), drag the start, the end or a pin; click a pin for its options; right-click a stretch next to a pin to choose how it is drawn; right-click or double-click a pin removes it; Ctrl+Z undoes."),
          paste0("\U0001F4CD", if (n_pins > 0) n_pins else "")
        ),
        shiny::tags$button(
          id = "review_lasso_toggle", type = "button", `data-review-pin` = "lasso",
          class = paste("btn btn-sm rv-pin-btn", if (isTRUE(rv$lasso_mode)) "btn-primary lasso-on" else "btn-outline-secondary"),
          title = tr("Exclude samples from the matching (L): every drag on the map circles jittery samples (e.g. stopped at a light), until you click again or press Esc; they still time the path. Ctrl+Z undoes."),
          paste0("\u27B0", if (n_excl > 0) n_excl else "")
        ),
        shiny::span(class = "lasso-hint", tr("Shift + drag = lasso")),
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
    # A rail trip without a route keeps its station rows (fx$path NULL)
    no_path <- is.null(fx$path)
    if (!no_path) parts <- c(parts, split_parts(id, act, fx, lang))
    if (family_of(act) == "road" && !is.null(fx$split)) return(shiny::div(class = "review-detail", parts, cover_notice(id, fx, lang)))
    length_line <- if (!no_path) {
      sprintf(tr("Proposed %.1f km · recorded %.1f km"),
              fx$proposed_km %||% (path_cumulative_m(cbind(fx$path$lon, fx$path$lat))[nrow(fx$path)] / 1000),
              fx$recorded_km)
    }

    if (family_of(act) == "rail") {
      plan <- fx$plan
      # Station select: the recorded point, the nearest stations of any rail
      # system (with their distance), then every other station by name (all
      # searchable); the pin picks one on the map
      all_st <- rail_stations()
      mode_label <- tolower(tr(REVIEW_ACTIVITIES[[act]]$label))
      end_row <- function(which, label, input_id, picked, options, time) {
        dist <- ifelse(options$dist_m < 1000, sprintf("%.0f m", options$dist_m), sprintf("%.1f km", options$dist_m / 1000))
        # The start from a Jaé tap says so, as a bus stop says its name's source
        jae <- identical(picked$source, "jae")
        src <- ifelse(jae & options$key %in% picked$key, " \u00b7 Ja\u00e9", "")
        near <- stats::setNames(options$key, sprintf("%s (%s)%s", rail_station_label(options, lang), dist, src))
        rest <- all_st[!all_st$key %in% options$key, ]
        rest_lab <- rail_station_label(rest, lang)
        rest <- stats::setNames(rest$key[order(rest_lab)], sort(rest_lab))
        choices <- stats::setNames(list(SAMPLE_POINT, near, rest), c(tr("recorded point"), tr("Nearest stations"), tr("All stations")))
        if (!length(rest)) choices <- choices[1:2]
        pin_title <- if (which == "start") tr("Pick the start station on the map (Esc cancels)") else
          tr("Pick the end station on the map (Esc cancels)")
        # Without a route: no time, the fine adjustments off
        adj <- function(delta, text) {
          shiny::tags$button(type = "button", class = "btn btn-outline-secondary",
                             `data-review-adj` = paste0(which, ":", delta), disabled = if (is.null(time)) NA, text)
        }
        tip <- if (jae) {
          sprintf(tr("Start from the Jaé tap at %s: departure at tap + %d s."), jae_local_time(as.POSIXct(picked$tap_t, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S"),
                  REVIEW_THRESHOLDS$rail_gate_to_train_s)
        }
        shiny::div(
          class = paste("review-end", if (jae) "rv-end-jae"), title = tip,
          shiny::span(class = "dt-label", label),
          shiny::selectInput(input_id, NULL, choices = choices,
                             selected = if (is.null(picked)) SAMPLE_POINT else picked$key, width = "100%"),
          shiny::span(class = "rv-num", if (is.null(time)) "\u2013" else local_time(time, "%H:%M:%S")),
          shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary rv-station-pick",
                             `data-station-pick` = which, title = pin_title, "\U0001F4CD"),
          shiny::div(class = paste("btn-group btn-group-sm", if (jae) "rv-adj-short"),
                     adj(-60, "−1m"), adj(-30, "−30s"), adj(30, "+30s"), adj(60, "+1m")),
          if (jae) shiny::span(class = "rv-end-src", title = tr("Name source"), "Ja\u00e9")
        )
      }
      # Transfer rows (set by hand, else Google's): select, pin, remove; then "+"
      xkeys <- xfer_rows(id, fx)
      s_mid <- trip_samples(id)
      s_mid <- c(s_mid$lon[ceiling(nrow(s_mid) / 2)], s_mid$lat[ceiling(nrow(s_mid) / 2)])
      xfer_row <- function(i, key) {
        st <- all_st[all_st$key == key, , drop = FALSE]
        d <- geosphere::distHaversine(cbind(all_st$lon, all_st$lat), if (nrow(st)) c(st$lon[1], st$lat[1]) else s_mid)
        k5 <- order(d)[seq_len(min(5, nrow(all_st)))]
        dist <- ifelse(d[k5] < 1000, sprintf("%.0f m", d[k5]), sprintf("%.1f km", d[k5] / 1000))
        near <- stats::setNames(all_st$key[k5], sprintf("%s (%s)", rail_station_label(all_st[k5, ], lang), dist))
        rest <- all_st[-k5, ]
        rest_lab <- rail_station_label(rest, lang)
        choices <- stats::setNames(list(XFER_NONE, near, stats::setNames(rest$key[order(rest_lab)], sort(rest_lab))),
                                   c(tr("— choose the station —"), tr("Nearest stations"), tr("All stations")))
        shiny::div(
          class = "review-xfer",
          shiny::span(class = "dt-label", tr("Transfer")),
          shiny::selectInput(paste0("review_xfer_", i), NULL, choices = choices, selected = if (nrow(st)) key else XFER_NONE, width = "100%"),
          shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary rv-station-pick",
                             `data-station-pick` = paste0("xfer:", i), title = tr("Pick the transfer station on the map (Esc cancels)"),
                             "\U0001F4CD"),
          shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary rv-xfer-del",
                             `data-review-xfer` = paste0("del:", i), title = tr("Remove this transfer"), "\u00d7")
        )
      }
      xfer_ui <- c(lapply(seq_along(xkeys), function(i) xfer_row(i, xkeys[i])),
                   if (length(xkeys) < XFER_MAX) list(shiny::div(
                     class = "review-xfer-add",
                     shiny::tags$button(type = "button", class = "btn btn-sm btn-link", `data-review-xfer` = "add",
                                        title = tr("Add a transfer station"), tr("+ Transfer"))
                   )))
      parts <- c(parts, list(end_row("start", tr("Start"), "review_dep", plan$dep, plan$dep_options, fx$t_start)), xfer_ui, list(
        end_row("end", tr("End"), "review_arr", plan$arr, plan$arr_options, fx$t_end),
        # Legs (or the fallback note); the lengths go in its tooltip to save a line
        if (no_path) {
          msg <- paste(c(tr("No proposal for this trip."), rail_no_path_text(fx$no_path, fx$no_path_station, lang),
                         google_fail_text(fx$google_fail, mode_label, lang)), collapse = " ")
          shiny::div(class = "rv-warn rv-clamp rv-no-route", title = msg, msg)
        } else if (identical(fx$source, "osm_rails")) {
          # The legs: each ride's line (its OSM route relation), each walk's length
          rl <- fx$rail_legs %||% list()
          legs <- paste(vapply(rl, function(l) if (identical(l$kind, "walk")) sprintf(tr("walk %.0f m"), l$dist_m) else l$label %||% "",
                               character(1)), collapse = " \u2192 ")
          graph <- any(vapply(rl, function(l) identical(l$source, "graph"), logical(1)))
          tip <- paste(c(length_line, google_fail_text(fx$google_fail, mode_label, lang),
                         if (graph) tr("Part of it along the rails graph (no route relation)."),
                         tr("Times by distance, from the departure to the arrival.")), collapse = "\n")
          shiny::div(class = "rv-clamp rv-osm-source", title = tip,
                     paste0(tr("Source: OSM rails"), if (nzchar(legs)) paste0(" \u00b7 ", legs)))
        } else if (identical(fx$timing, "steps")) {
          tip <- sprintf(tr("Google: %s. Scaled ×%.2f to fit the recorded times."),
                         format_rail_legs(fx$legs, lang, google = TRUE), fx$scale)
          shiny::div(class = "rv-clamp rv-osm-source", title = paste0(length_line, "\n", tip),
                     paste0(tr("Source: Google"), " \u00b7 ", format_rail_legs(fx$legs, lang)))
        } else {
          note <- tr("Timing by distance: Google returned no step durations.")
          shiny::div(class = "text-secondary rv-clamp", title = paste0(length_line, "\n", note), note)
        },
        if (!no_path && !isTRUE(fx$on_network)) {
          warn <- paste(tr("Route off the rails:"), format_reasons(fx$route_reasons, lang))
          shiny::div(class = "rv-warn rv-clamp", title = warn, warn)
        }
      ))
    } else {
      parts <- c(parts, retime_rows(id, fx, row, lang))
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
        if (length(fx$warn)) {
          shiny::div(class = "rv-warn", sprintf(tr("Pin %s: the route through it is much longer than the stretch it replaces, even with wider windows."),
                                                paste(match(fx$warn, fx$pins$id), collapse = ", ")))
        },
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
    shiny::div(class = "review-detail", parts, cover_notice(id, fx, lang))
  })

  # Gaps over cover$notice_s between the proposal and the original item, with
  # the fill of each (select); a proposal reaching outside the item is fitted
  # into it
  cover_notice <- function(id, fx, lang) {
    cv <- tryCatch(cover_preview(id, fx), error = function(e) NULL)
    if (is.null(cv)) return(NULL)
    th <- REVIEW_THRESHOLDS$cover
    row <- trip_row(id)
    hm <- function(t) local_time(t, "%H:%M")
    big <- Filter(function(g) g$mode %in% c("wait", "walk", "extend") && g$gap_s > th$notice_s, cv$gaps)
    over <- cv$raw[1] < cv$span[1] - th$notice_s || cv$raw[2] > cv$span[2] + th$notice_s
    if (!length(big) && !over) return(NULL)
    head <- sprintf(tr("Proposal covers %s–%s of %s–%s"), hm(cv$raw[1]), hm(cv$raw[2]), hm(row$start_utc), hm(row$end_utc))
    labels <- function(g) {
      wait <- if (nzchar(g$place)) sprintf(tr("wait at %s"), g$place) else if (g$side == "before") tr("wait at the start") else
        tr("wait at the end")
      c(wait = wait, walk = tr("recorded walk"), extend = tr("extend the main leg"))
    }
    mins <- function(g) max(1, round(g$gap_s / 60))
    rows <- lapply(big, function(g) {
      lab <- labels(g)
      shiny::div(
        class = "rv-cover-row", `data-cover-side` = g$side,
        shiny::span(class = "rv-cover-lead", sprintf(if (g$side == "before") tr("%d min before:") else tr("%d min after:"), mins(g))),
        shiny::selectInput(paste0("review_fill_", g$side), NULL, choices = stats::setNames(names(lab), lab), selected = g$mode,
                           selectize = FALSE, width = "auto")
      )
    })
    fitted <- if (over) tr("it will be fitted into the trip's time.")
    full <- paste(c(head, vapply(big, function(g) sprintf(tr("%d min will be filled as %s"), mins(g), labels(g)[[g$mode]]), ""), fitted),
                  collapse = "; ")
    shiny::div(class = "rv-cover", title = full,
               shiny::div(class = "rv-clamp rv-cover-head", paste0(head, if (over) paste0(": ", fitted) else if (length(big)) ";")),
               rows)
  }
  for (side in c("before", "after")) local({
    sd <- side
    shiny::observeEvent(input[[paste0("review_fill_", sd)]], {
      id <- rv$selected
      v <- input[[paste0("review_fill_", sd)]]
      if (is.null(id) || is.null(rv$proposal) || !v %in% c("wait", "walk", "extend")) return()
      cv <- tryCatch(cover_preview(id, rv$proposal), error = function(e) NULL)
      if (identical(cv$gaps[[sd]]$mode, v)) return()  # a re-render, not a choice
      f <- get_ov(id)$fill %||% list()
      f[[sd]] <- v
      set_ov(id, list(fill = f))
    }, ignoreInit = TRUE)
  })

  # A dragged start / end of a road trip: its recalculated time (the original
  # minus / plus the drive time of the stretch added, within the neighbouring
  # items) with the fine adjustments; ⚠ when clamped or not routed
  end_clamp_text <- function(id, which) {
    row <- trip_row(id)
    if (which == "start") sprintf(tr("Clamped to the previous item's start (%s)."), local_time(row$prev_start_utc, "%H:%M:%S")) else
      sprintf(tr("Clamped to the next item's end (%s)."), local_time(row$next_end_utc, "%H:%M:%S"))
  }
  retime_rows <- function(id, fx, row, lang) {
    info <- fx$ends_info %||% list()
    lapply(intersect(c("start", "end"), names(info)), function(which) {
      inf <- info[[which]]
      anchored <- identical(inf$source, "anchor")
      why <- fx$fail[[which]]
      t_new <- fx[[paste0("t_", which, "_new")]]
      clamped <- isTRUE(fx[[paste0(which, "_clamped")]])
      was <- local_time(if (which == "start") row$start_utc else row$end_utc, "%H:%M:%S")
      warn <- if (!is.null(why)) splice_fail_text(why, lang) else if (clamped) end_clamp_text(id, which)
      adj <- function(delta, text) {
        shiny::tags$button(type = "button", class = "btn btn-outline-secondary", `data-review-adj` = paste0(which, ":", delta),
                           disabled = if (is.null(t_new)) NA, text)
      }
      label <- if (anchored) {
        sprintf(if (which == "start") tr("Start: %s (default)") else tr("End: %s (default)"), inf$anchor)
      } else if (which == "start") {
        tr("Recalculated start")
      } else {
        tr("Recalculated end")
      }
      shiny::div(
        class = "review-retime", `data-retime` = which, title = paste(c(sprintf(tr("Was %s."), was), warn), collapse = " "),
        shiny::span(class = "rv-retime-label", label),
        shiny::span(class = "rv-num", if (is.null(t_new)) "\u2013" else local_time(t_new, "%H:%M:%S")),
        if (!is.null(warn)) shiny::span(class = "rv-warn rv-retime-warn", title = warn, "\u26A0"),
        # Dragged off a fixed point: back to it
        if (!anchored && !is.null(inf$anchor)) {
          shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary rv-anchor-restore", `data-review-anchor` = which,
                             title = sprintf(tr("Back to the default point: %s"), inf$anchor), paste0("\u21BA ", inf$anchor))
        },
        shiny::div(class = "btn-group btn-group-sm", adj(-60, "\u22121m"), adj(-30, "\u221230s"), adj(30, "+30s"), adj(60, "+1m"))
      )
    })
  }

  # Back to a fixed point: the dragged end goes (the anchor applies again)
  shiny::observeEvent(input$review_anchor_restore, {
    id <- rv$selected
    which <- input$review_anchor_restore$which
    if (is.null(id) || !isTRUE(which %in% c("start", "end"))) return()
    ends <- rv$route_ends[[id]] %||% list()
    ends[[which]] <- NULL
    set_route_edits(id, ends = ends)
  })

  # Station selects, time adjustments, start/end from the map
  station_changed <- function(which, value) {
    id <- rv$selected
    fx <- rv$proposal
    if (is.null(id) || is.null(fx) || is.null(value) || family_of(trip_row(id)$activity) != "rail") return()
    current <- if (which == "dep") fx$plan$dep else fx$plan$arr
    current_key <- if (is.null(current)) SAMPLE_POINT else current$key %||% current$name
    if (identical(value, current_key)) return()
    set_ov(id, stats::setNames(list(if (identical(value, SAMPLE_POINT)) "" else value), which))
    recompute_selected()
  }
  shiny::observeEvent(input$review_dep, station_changed("dep", input$review_dep), ignoreInit = TRUE)
  shiny::observeEvent(input$review_arr, station_changed("arr", input$review_arr), ignoreInit = TRUE)
  # Station pick on the map (the pins by the selects): the stations for the
  # client, then the one picked, as if chosen in the select
  shiny::observeEvent(input$review_station_list, {
    st <- rail_stations()
    session$sendCustomMessage("rail_stations", list(
      lang = current_lang(), key = st$key, label = rail_station_label(st, current_lang()), lat = st$lat, lng = st$lon,
      none = tr("No station within 300 m: click on or next to one (Esc cancels).")
    ))
  })
  shiny::observeEvent(input$review_station_pick, {
    ev <- input$review_station_pick
    if (!is.character(ev$key)) return()
    if (startsWith(ev$which %||% "", "xfer:")) return(xfer_changed(as.integer(sub("^xfer:", "", ev$which)), ev$key))
    if (!ev$which %in% c("start", "end")) return()
    station_changed(if (ev$which == "start") "dep" else "arr", ev$key)
  })

  # Transfer stations of a rail trip: the plan's (Google's, or those set by
  # hand plus any added automatically for a walk), then the rows set by hand
  # but not filled yet (""); editing a row makes them all set by hand
  xfer_rows <- function(id, fx) {
    keys <- get_ov(id)$xfers
    plan_keys <- vapply(fx$plan$xfers %||% list(), function(x) as.character(x$key), character(1))
    if (is.null(keys)) plan_keys else c(plan_keys, keys[!nzchar(keys)])
  }
  xfer_set <- function(id, keys) {
    set_ov(id, list(xfers = keys))
    recompute_selected()
  }
  xfer_changed <- function(i, value) {
    id <- rv$selected
    fx <- rv$proposal
    if (is.null(id) || is.null(fx) || is.na(i) || family_of(trip_row(id)$activity) != "rail") return()
    keys <- xfer_rows(id, fx)
    value <- if (identical(value, XFER_NONE)) "" else value
    if (i < 1 || i > length(keys) || identical(keys[i], value)) return()
    keys[i] <- value
    xfer_set(id, keys)
  }
  for (i in seq_len(XFER_MAX)) local({
    k <- i
    shiny::observeEvent(input[[paste0("review_xfer_", k)]], xfer_changed(k, input[[paste0("review_xfer_", k)]]), ignoreInit = TRUE)
  })
  shiny::observeEvent(input$review_xfer_edit, {
    ev <- input$review_xfer_edit
    id <- rv$selected
    fx <- rv$proposal
    if (is.null(id) || is.null(fx) || family_of(trip_row(id)$activity) != "rail") return()
    keys <- xfer_rows(id, fx)
    if (identical(ev$op, "add") && length(keys) < XFER_MAX) {
      keys <- c(keys, "")
    } else if (identical(ev$op, "del") && isTRUE(as.integer(ev$i) %in% seq_along(keys))) {
      keys <- keys[-as.integer(ev$i)]
    } else {
      return()
    }
    xfer_set(id, keys)
  })

  shiny::observeEvent(input$review_time_adj, {
    id <- rv$selected
    ev <- input$review_time_adj
    if (is.null(id) || is.null(rv$proposal) || is.null(rv$proposal$path)) return()
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
    if (is.null(id) || is.null(fx) || !identical(fx$id, id) || is.null(fx$path)) {
      notify(tr("No proposal to accept for this trip."), type = "warning")
      return()
    }
    sp <- fx$split
    if (!is.null(sp) && !isTRUE(sp$ignored) && is.null(sp$segments)) {
      notify(tr("Click the map where you got off first."), type = "warning")
      return()
    }
    acc <- rv$accepted
    # Its tracks cover the original item exactly (the gaps filled)
    acc[[id]] <- cover_accept(id, fx, if (!is.null(sp$segments)) {
      segs <- multimodal_label_segments(sp$segments, split_stop(id, sp), jae_leg_text(sp$tap))
      multimodal_accepted(segs, fx$activity, tap = sp$tap)
    } else if (family_of(fx$activity) == "rail") {
      rail_accept_entry(fx, fx$activity)   # a walking transfer is its own track
    } else {
      list(activity = fx$activity, path = fx$path[, c("lon", "lat", "t")], method = fx$method)
    })
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
                              legs = rv$legs[[id]], record = TRUE) {
    if (record) {
      h <- edit_hist[[id]] %||% list()
      h[[length(h) + 1L]] <- list(pins = rv$pins[[id]], ends = rv$route_ends[[id]], excl = rv$exclusions[[id]],
                                  legs = rv$legs[[id]])
      assign(id, utils::tail(h, 30), envir = edit_hist)
    }
    rv$pins <- set_named(rv$pins, id, if (is.null(pins) || nrow(pins) == 0) NULL else pins)
    rv$legs <- set_named(rv$legs, id, if (length(legs)) legs else NULL)
    rv$route_ends <- set_named(rv$route_ends, id, if (length(ends)) ends else NULL)
    rv$exclusions <- set_named(rv$exclusions, id, if (length(excl)) sort(unique(excl)) else NULL)
    save_state()
    recompute_selected()
  }
  set_pins <- function(id, pins) set_route_edits(id, pins = pins)
  # A new pin; grab: where its drag started on the path (its splice's centre)
  add_pin <- function(lon, lat, grab = c(NA_real_, NA_real_)) {
    id <- pin_trip()
    if (is.null(id) || is.null(lon) || is.null(lat)) return()
    pins <- pins_normalize(rv$pins[[id]])
    new_id <- as.character(max(c(0L, suppressWarnings(as.integer(pins$id))), na.rm = TRUE) + 1L)
    set_pins(id, rbind(pins, data.frame(id = new_id, lon = lon, lat = lat, after = NA_character_, grab_lon = grab[1],
                                        grab_lat = grab[2], ww = "minor", turns = "respect")))
  }
  # Routed legs of the splices, by their request (a moved pin re-routes only its own legs)
  leg_cache <- new.env(parent = emptyenv())
  cached_leg_route <- function(xy, opts, bearings = NULL, radiuses = NULL) {
    key <- paste(c(sprintf("%.6f", xy), round(bearings), round(radiuses), opts$profile, opts$exclude %||% "-"), collapse = "|")
    if (!is.null(leg_cache[[key]])) return(leg_cache[[key]])
    r <- splice_leg_route(xy, opts, bearings, radiuses)
    if (!is.null(r)) assign(key, r, envir = leg_cache)
    r
  }
  # Drag ends (route editing): the line -> a new via pin; the start / end markers -> moved route ends
  shiny::observeEvent(input$review_route_drag, {
    ev <- input$review_route_drag
    id <- pin_trip()
    if (is.null(id) || !isTRUE(rv$pin_mode)) return()
    lon <- as.numeric(ev$lng)
    lat <- as.numeric(ev$lat)
    if (identical(ev$kind, "line")) {
      add_pin(lon, lat, grab = c(as.numeric(ev$glng %||% NA), as.numeric(ev$glat %||% NA)))
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
    set_route_edits(id, pins = last$pins, ends = last$ends, excl = last$excl, legs = last$legs, record = FALSE)
  })
  # Lasso (the mode on, or Shift + drag): the samples inside are kept out of
  # /match (they still time the path)
  shiny::observeEvent(input$review_lasso_samples, {
    id <- pin_trip()
    if (is.null(id) || !(isTRUE(rv$lasso_mode) || isTRUE(input$review_lasso_samples$shift))) return()
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
      pins <- pins_normalize(fx$pins)
      legs <- unlist(lapply(fx$splices, `[[`, "legs"), recursive = FALSE)
      leg_end <- function(pattern, last) {   # the other end of the leg into / out of a pin (drag preview)
        l <- Filter(function(l) grepl(pattern, l$key), legs)
        if (!length(l)) return(NULL)
        xy <- l[[1]]$coords[if (last) nrow(l[[1]]$coords) else 1L, ]
        list(lat = xy[["lat"]], lng = xy[["lon"]])
      }
      # A dragged end sits where it was dropped; ⚠ with the reason when it got no
      # route, or its time was clamped to the neighbouring item
      end_ctrl <- function(which, k) {
        at <- fx$ends_eff[[which]] %||% c(p$lon[k], p$lat[k])
        why <- fx$fail[[which]]
        clamped <- isTRUE(fx[[paste0(which, "_clamped")]])
        inf <- fx$ends_info[[which]]
        list(id = which, lat = at[2], lng = at[1], warn = !is.null(why) || clamped,
             warn_text = if (!is.null(why)) splice_fail_text(why, current_lang()) else if (clamped) end_clamp_text(id, which),
             title = if (identical(inf$source, "anchor")) sprintf(tr("%s (default)"), inf$anchor))
      }
      pin_fail <- function(k) if (nzchar(pins$warn_text[k] %||% "")) splice_fail_text(pins$warn_text[k], current_lang())
      msg$ctrl <- c(list(end_ctrl("start", 1L)),
                    lapply(seq_len(nrow(pins)), function(k) {
                      pid <- as.character(pins$id[k])
                      list(id = pid, lat = pins$lat[k], lng = pins$lon[k], ww = pins$ww[k], turns = pins$turns[k],
                           warn = isTRUE(pins$warn[k]), warn_text = pin_fail(k),
                           prev = leg_end(paste0(">", pid, "$"), FALSE), `next` = leg_end(paste0("^", pid, ">"), TRUE))
                    }),
                    list(end_ctrl("end", n)))
      msg$path <- lapply(seq_len(n), function(k) c(p$lat[k], p$lon[k]))
      msg$window_m <- REVIEW_THRESHOLDS$pins$splice$window_m   # the drag preview's reach (the splice itself ends where the route rejoins)
      msg$rules <- identical(fx$profile, "official")
      msg$legs <- lapply(legs, function(l) {
        list(key = l$key, mode = l$mode, path = lapply(seq_len(nrow(l$coords)), function(k) unname(l$coords[k, 2:1])))
      })
      msg$labels <- list(
        ww = tr("Wrong-way"), ww_no = tr("No"), ww_minor = tr("Minor streets"), ww_all = tr("All"),
        turns = tr("Restricted turns"), turns_respect = tr("Respect"), turns_ignore = tr("Ignore"),
        warn = tr("The route through this pin is much longer than the stretch it replaces, even with wider windows."),
        route = tr("Route (OSRM)"), recorded = tr("Recorded path"), straight = tr("Straight line"),
        leg_hint = tr("Right-click a stretch next to a pin to change how it is drawn.")
      )
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
        notify(tr("Route editing: drag the line to add a pin (only the stretch around it changes), drag the start or the end; click a pin for its options; right-click or double-click a pin removes it (P to leave)."),
               duration = 5)
      }
    } else if (identical(action, "clear") && !is.null(id)) {
      set_pin_mode(FALSE)
      set_route_edits(id, pins = NULL, ends = NULL, legs = NULL)
    } else if (identical(action, "lasso")) {
      if (is.null(id)) {
        if (isTRUE(rv$lasso_mode)) set_lasso_mode(FALSE)
        return()
      }
      set_lasso_mode(!isTRUE(rv$lasso_mode))
      if (isTRUE(rv$lasso_mode)) {
        notify(tr("Lasso: every drag circles jittery samples to keep them out of the matching; it stays on until you press L or Esc or click the button."), duration = 5)
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
  # Pin popup (official trips): wrong-way and restricted-turn rules of its splice
  shiny::observeEvent(input$review_pin_rule, edit_pin(input$review_pin_rule, function(p, k) {
    ev <- input$review_pin_rule
    p <- pins_normalize(p)
    if (identical(ev$rule, "ww") && isTRUE(ev$value %in% PIN_RULES_WW)) p$ww[k] <- ev$value
    if (identical(ev$rule, "turns") && isTRUE(ev$value %in% PIN_RULES_TURNS)) p$turns[k] <- ev$value
    p
  }))
  # Leg menu (right-click on a spliced stretch): OSRM route, recorded path or straight line
  shiny::observeEvent(input$review_leg_mode, {
    ev <- input$review_leg_mode
    id <- pin_trip()
    if (is.null(id) || !isTRUE(ev$mode %in% LEG_MODES) || !is.character(ev$key)) return()
    legs <- rv$legs[[id]] %||% character(0)
    legs[ev$key] <- ev$mode
    set_route_edits(id, legs = legs[legs != "route"])
  })

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
      acc[[id]] <- cover_accept(id, fixes[[id]], list(activity = fixes[[id]]$activity,
                                                     path = fixes[[id]]$path[, c("lon", "lat", "t")], method = fixes[[id]]$method))
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
      warn <- if (!is.null(fx) && identical(fx$id, id)) fx$warn else character(0)
      proxy <- leaflet::addMarkers(
        proxy, lng = pins$lon, lat = pins$lat, layerId = paste0("pin_", pins$id), group = "review_pins",
        label = paste0(match(pins$id, ord), ifelse(pins$id %in% warn, " \u26A0", "")),
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
      if (!is.null(p)) {
        proxy <- leaflet::addPolylines(proxy, lng = p$lon, lat = p$lat, color = color, weight = 5, opacity = 0.85,
                                       group = "review_proposed")
      }
      # A dragged end dropped off the road: its connector, dashed
      for (sg in fx$splices) if (!is.null(sg$connector)) {
        proxy <- leaflet::addPolylines(proxy, lng = sg$connector[, 1], lat = sg$connector[, 2], color = "white", weight = 6, opacity = 1,
                                       group = "review_proposed", options = leaflet::pathOptions(interactive = FALSE))
        proxy <- leaflet::addPolylines(proxy, lng = sg$connector[, 1], lat = sg$connector[, 2], color = color, weight = 4, opacity = 0.95,
                                       dashArray = "3 6", group = "review_proposed",
                                       options = leaflet::pathOptions(interactive = FALSE, className = "end-connector"))
      }
      # Free legs (recorded path, straight line): dashed over a white gap
      for (sg in fx$splices) for (l in sg$legs) {
        if (l$mode == "route") next
        proxy <- leaflet::addPolylines(proxy, lng = l$coords[, 1], lat = l$coords[, 2], color = "white", weight = 6, opacity = 1,
                                       group = "review_proposed", options = leaflet::pathOptions(interactive = FALSE))
        proxy <- leaflet::addPolylines(proxy, lng = l$coords[, 1], lat = l$coords[, 2], color = color, weight = 5, opacity = 0.95,
                                       dashArray = if (l$mode == "recorded") "2 7" else "12 8", group = "review_proposed",
                                       label = tr(if (l$mode == "recorded") "Recorded path" else "Straight line"),
                                       options = leaflet::pathOptions(interactive = FALSE, className = paste0("free-leg free-leg-", l$mode)))
      }
      plan <- fx$plan
      label_end <- function(st, t) paste0(if (!is.null(st)) paste0(st$name, " · ") else "", local_time(t, "%H:%M:%S"))
      if (!is.null(p)) {
        proxy <- leaflet::addCircleMarkers(
          proxy, lng = c(p$lon[1], p$lon[nrow(p)]), lat = c(p$lat[1], p$lat[nrow(p)]), radius = 7,
          color = "white", weight = 2, fillColor = c("#2E7D32", "#C62828"), fillOpacity = 1,
          label = c(label_end(plan$dep, p$t[1]), label_end(plan$arr, p$t[nrow(p)])), group = "review_markers"
        )
      }
      # Rail: the chosen stations, ringed and named (transfers in orange)
      for (x in plan$xfers %||% list()) {
        proxy <- leaflet::addCircleMarkers(
          proxy, lng = x$lon, lat = x$lat, radius = 10, color = "#EF6C00", weight = 3, opacity = 1, fill = FALSE,
          label = paste0(tr("Transfer"), ": ", rail_station_label(x, current_lang())), group = "review_markers",
          labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "right", offset = c(10, 0), className = "station-chosen"),
          options = leaflet::pathOptions(interactive = FALSE, className = "station-chosen-xfer")
        )
      }
      for (end in list(list(st = plan$dep, col = "#2E7D32", lab = tr("Start")), list(st = plan$arr, col = "#C62828", lab = tr("End")))) {
        if (is.null(end$st) || is.null(end$st$key)) next
        proxy <- leaflet::addCircleMarkers(
          proxy, lng = end$st$lon, lat = end$st$lat, radius = 12, color = end$col, weight = 3, opacity = 1, fill = FALSE,
          label = paste0(end$lab, ": ", rail_station_label(end$st, current_lang())), group = "review_markers",
          labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "right", offset = c(12, 0), className = "station-chosen"),
          options = leaflet::pathOptions(interactive = FALSE, className = paste0("station-chosen-", if (end$lab == tr("Start")) "start" else "end"))
        )
      }
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
    rv$legs
    rv$exclusions
    input$mode
    shiny::isolate(draw_selected())
  })
  shiny::observeEvent(input$mode, if (!identical(input$mode, "BatchReview")) fitted$id <- NULL)

  # ---- Export ----

  export_set <- function() {
    acc <- rv$accepted
    if (isTRUE(input$review_only_checked)) acc <- acc[intersect(names(acc), rv$checked)]
    # Fixes accepted before the coverage rule: covered now (gaps filled from the recording)
    for (id in names(acc)) {
      if (isTRUE(acc[[id]]$covered) || nrow(trip_row(id)) == 0) next
      acc[[id]] <- tryCatch(cover_accept(id, list(activity = acc[[id]]$activity), acc[[id]]), error = function(e) acc[[id]])
    }
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
                                selected = saved$acts %||% shiny::isolate(input$review_activities))
    shiny::updateSelectInput(session, "review_sort", choices = tr_choices(REVIEW_SORT_CHOICES, lang),
                             selected = shiny::isolate(input$review_sort) %||% "date")
  }, ignoreInit = TRUE)

  invisible(NULL)
}
