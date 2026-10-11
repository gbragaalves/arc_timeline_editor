# ---- Helpers: iCloud Drive round-trip via rclone (iclouddrive remote) ----
# Fetches weekly sample backups and sends import packages / GPX files.
# Layout (Arc Timeline 4 / LocoKit2):
#   <remote>:<Arc dir>/Backup/<BACKUP-UUID>/samples/YYYY-Www.json.gz
#   <remote>:<Arc dir>/Import/   (items/, places/, samples/, metadata.json)
#   <remote>:<Arc dir>/GPX/
# Never prints or reads the rclone config itself.

# Config (config_local.R can override any of these)
ICLOUD_REMOTE    <- if (exists("ICLOUD_REMOTE"))    ICLOUD_REMOTE    else "icloud"
ICLOUD_ARC_DIR   <- if (exists("ICLOUD_ARC_DIR"))   ICLOUD_ARC_DIR   else "Arc Timeline"
ICLOUD_BACKUP_ID <- if (exists("ICLOUD_BACKUP_ID")) ICLOUD_BACKUP_ID else NULL  # NULL = auto-detect
# Monthly timeline items (items/YYYY-MM.json), next to the weekly samples
ITEMS_BACKUP_DIR <- if (exists("ITEMS_BACKUP_DIR")) ITEMS_BACKUP_DIR else file.path(dirname(WEEKLY_BACKUP_DIR), "items")

# Session cache (availability, active backup ID, last error output)
.icloud_state <- new.env(parent = emptyenv())

# User-facing message in lang (translation keys in i18n/translations.json;
# plain English when utils_i18n.R isn't loaded)
icloud_msg <- function(key, lang = "en") {
  if (exists("tr_lang", mode = "function")) tr_lang(key, lang) else key
}

# Run rclone with each argument shell-quoted (remote paths contain spaces).
# stderr goes to its own file so JSON on stdout stays parseable.
rclone_run <- function(args, timeout = 300) {
  err_file <- tempfile("rclone_err_")
  on.exit(unlink(err_file), add = TRUE)

  out <- suppressWarnings(tryCatch(
    system2("rclone", shQuote(args), stdout = TRUE, stderr = err_file, timeout = timeout),
    error = function(e) structure(conditionMessage(e), status = 127L)
  ))
  status <- attr(out, "status") %||% 0L
  err <- if (file.exists(err_file)) readLines(err_file, warn = FALSE) else character(0)
  if (identical(as.integer(status), 124L)) err <- c(err, sprintf("rclone timed out after %d s", timeout))

  list(
    ok     = identical(as.integer(status), 0L),
    stdout = as.character(out),
    output = c(as.character(out), err)
  )
}

# TRUE when rclone is installed and the iCloud remote is configured (cached)
icloud_available <- function() {
  if (!is.null(.icloud_state$available)) return(.icloud_state$available)

  ok <- FALSE
  if (nzchar(Sys.which("rclone"))) {
    res <- rclone_run("listremotes", timeout = 30)
    ok <- res$ok && paste0(ICLOUD_REMOTE, ":") %in% trimws(res$stdout)
  }
  .icloud_state$available <- ok
  ok
}

# "icloud:Arc Timeline/<parts...>"
icloud_path <- function(...) {
  parts <- c(ICLOUD_ARC_DIR, ...)
  paste0(ICLOUD_REMOTE, ":", paste(parts[nzchar(parts)], collapse = "/"))
}

# Backup UUID whose samples/ has the most recently modified file (cached).
# Returns NULL on failure; the rclone output is kept in .icloud_state$last_error.
icloud_active_backup <- function() {
  if (!is.null(ICLOUD_BACKUP_ID) && nzchar(ICLOUD_BACKUP_ID)) return(ICLOUD_BACKUP_ID)
  if (!is.null(.icloud_state$backup_id)) return(.icloud_state$backup_id)

  res <- rclone_run(c("lsf", "--dirs-only", icloud_path("Backup")), timeout = 120)
  if (!res$ok) {
    .icloud_state$last_error <- res$output
    message("[iCloud] Could not list backups")
    return(NULL)
  }

  dirs <- sub("/$", "", res$stdout[nzchar(res$stdout)])
  latest <- vapply(dirs, function(d) {
    r <- rclone_run(c("lsjson", icloud_path("Backup", d, "samples")), timeout = 120)
    if (!r$ok) {
      .icloud_state$last_error <- r$output
      return(NA_real_)
    }
    files <- tryCatch(jsonlite::fromJSON(paste(r$stdout, collapse = "\n")), error = function(e) NULL)
    if (!is.data.frame(files) || nrow(files) == 0 || is.null(files$ModTime)) return(NA_real_)
    mt <- as.numeric(lubridate::ymd_hms(files$ModTime, quiet = TRUE))
    if (all(is.na(mt))) NA_real_ else max(mt, na.rm = TRUE)
  }, numeric(1))

  if (length(latest) == 0 || all(is.na(latest))) {
    message("[iCloud] No backup with samples found")
    return(NULL)
  }

  backup_id <- dirs[which.max(latest)]
  message("[iCloud] Active backup: ", backup_id)
  .icloud_state$backup_id <- backup_id
  backup_id
}

# Copy the given weeks (e.g. "2026-W41") from the active backup into dest.
# rclone copy only re-downloads changed files.
# Returns list(ok, message, weeks = weeks present in dest afterwards).
icloud_fetch_weeks <- function(weeks, dest = WEEKLY_BACKUP_DIR, lang = "en") {
  weeks <- unique(as.character(weeks))
  present <- function() weeks[file.exists(file.path(dest, paste0(weeks, ".json.gz")))]

  if (length(weeks) == 0) {
    return(list(ok = TRUE, message = icloud_msg("iCloud: no weeks to fetch", lang), weeks = character(0)))
  }

  backup_id <- icloud_active_backup()
  if (is.null(backup_id)) {
    return(list(ok = FALSE, message = icloud_error_hint(.icloud_state$last_error, lang), weeks = present()))
  }

  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  includes <- as.vector(rbind("--include", paste0(weeks, ".json.gz")))
  res <- rclone_run(c("copy", icloud_path("Backup", backup_id, "samples"), dest, includes), timeout = 600)

  got <- present()
  if (!res$ok) {
    return(list(ok = FALSE, message = icloud_error_hint(res$output, lang), weeks = got))
  }

  msg <- if (length(got) > 0) {
    sprintf(icloud_msg("iCloud: %s up to date", lang), paste(got, collapse = ", "))
  } else {
    icloud_msg("iCloud: no matching weeks found", lang)
  }
  missing <- setdiff(weeks, got)
  if (length(missing) > 0) {
    msg <- paste0(msg, " ", sprintf(icloud_msg("(not in iCloud: %s)", lang), paste(missing, collapse = ", ")))
  }

  list(ok = TRUE, message = msg, weeks = got)
}

# Copy the whole active backup's samples/ into samples_dest and items/ into
# items_dest (rclone copy: only changed files are downloaded, nothing is
# deleted locally). on_step(fraction, label) reports progress (e.g. setProgress
# inside withProgress). Returns list(ok, message).
icloud_fetch_all <- function(samples_dest = WEEKLY_BACKUP_DIR, items_dest = ITEMS_BACKUP_DIR,
                             lang = "en", on_step = NULL) {
  step <- function(fraction, label) if (is.function(on_step)) on_step(fraction, label)

  step(0.05, icloud_msg("Finding the active backup...", lang))
  backup_id <- icloud_active_backup()
  if (is.null(backup_id)) {
    return(list(ok = FALSE, message = icloud_error_hint(.icloud_state$last_error, lang)))
  }

  parts <- list(
    list(sub = "samples", dest = samples_dest, label = icloud_msg("Fetching samples...", lang), at = 0.15),
    list(sub = "items",   dest = items_dest,   label = icloud_msg("Fetching items...", lang),   at = 0.85)
  )
  for (p in parts) {
    step(p$at, p$label)
    if (!dir.exists(p$dest)) dir.create(p$dest, recursive = TRUE, showWarnings = FALSE)
    res <- rclone_run(c("copy", icloud_path("Backup", backup_id, p$sub), p$dest), timeout = 1800)
    if (!res$ok) return(list(ok = FALSE, message = icloud_error_hint(res$output, lang)))
  }

  step(1, icloud_msg("Done", lang))
  n_weeks <- length(list.files(samples_dest, "\\.json\\.gz$"))
  n_months <- length(list.files(items_dest, "\\.json$"))
  list(ok = TRUE, message = sprintf(icloud_msg("iCloud: %d weeks of samples and %d months of items up to date", lang),
                                    n_weeks, n_months))
}

# rclone arguments for icloud_push_import(); stops unless local_dir is an
# import package and the destination is exactly <Arc dir>/Import.
icloud_push_import_args <- function(local_dir, lang = "en") {
  required_dirs <- c("items", "samples", "places")
  if (!dir.exists(local_dir) ||
      !file.exists(file.path(local_dir, "metadata.json")) ||
      !all(dir.exists(file.path(local_dir, required_dirs)))) {
    stop(icloud_msg("Refusing to send: not an import package (needs metadata.json, items/, samples/, places/).", lang),
         call. = FALSE)
  }

  dest <- icloud_path("Import")
  if (!endsWith(dest, "/Import")) {
    stop(sprintf(icloud_msg("Refusing to sync: destination must end with /Import (got '%s').", lang), dest), call. = FALSE)
  }

  c("sync", normalizePath(local_dir), dest)
}

# Sync an import package into <Arc dir>/Import. sync is intentional: it removes
# leftovers from previous imports so Arc doesn't reprocess them.
icloud_push_import <- function(local_dir, lang = "en") {
  args <- tryCatch(icloud_push_import_args(local_dir, lang), error = function(e) e)
  if (inherits(args, "error")) return(list(ok = FALSE, message = conditionMessage(args)))

  res <- rclone_run(args, timeout = 600)
  if (!res$ok) return(list(ok = FALSE, message = icloud_error_hint(res$output, lang)))

  list(ok = TRUE, message = sprintf(icloud_msg("Sent to %s", lang), args[3]))
}

# Copy one file to <Arc dir>/<subdir>/<basename>
icloud_push_file <- function(local_file, subdir = "GPX", lang = "en") {
  if (!file.exists(local_file) || dir.exists(local_file)) {
    return(list(ok = FALSE, message = icloud_msg("File to send not found.", lang)))
  }

  dest <- icloud_path(subdir, basename(local_file))
  res <- rclone_run(c("copyto", normalizePath(local_file), dest), timeout = 300)
  if (!res$ok) return(list(ok = FALSE, message = icloud_error_hint(res$output, lang)))

  list(ok = TRUE, message = sprintf(icloud_msg("Sent to %s", lang), dest))
}

# User-facing hint for a failed rclone call
icloud_error_hint <- function(output, lang = "en") {
  output <- as.character(output %||% character(0))
  if (any(grepl("auth|trust ?token|\\b40[13]\\b|reconnect", output, ignore.case = TRUE))) {
    return(sprintf(icloud_msg("iCloud authorization expired. In a terminal, run: rclone reconnect %s:", lang), ICLOUD_REMOTE))
  }

  lines <- output[nzchar(trimws(output))]
  if (length(lines) == 0) return(icloud_msg("rclone failed with no output.", lang))
  paste(utils::tail(lines, 3), collapse = "; ")
}
