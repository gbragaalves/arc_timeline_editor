# ---- iCloud cleanup: what the app pushes to <Arc dir>/GPX and <Arc dir>/Import ----
# Run daily by tools/icloud_cleanup.R (cron). Only those two folders are ever
# touched: every deletion goes through icloud_cleanup_delete(), which refuses
# any other path (icloud_cleanup_allowed). Backups are only read.
#   Import/: a notes-only package (notes/ plus empty items/places/samples, see
#     write_notes_package_dir) is deleted once Arc has absorbed it: every note id
#     in it is in the latest Arc backup's notes/. Not absorbed after
#     NOTES_PENDING_DAYS: kept, and the iCloud status dot turns orange. Other
#     packages (edits with items / samples) are left alone.
#   GPX/: a file is deleted only when the app sent it (it is in the local send
#     log with the same size) more than GPX_RETENTION_DAYS ago, and a local copy
#     is in GPX_ARCHIVE_DIR (copied from iCloud first when missing). Off unless
#     GPX_CLEANUP_ENABLED (config_local.R).
# The outcome goes to ICLOUD_CLEANUP_STATUS_FILE, read by the app's status dot.

GPX_RETENTION_DAYS  <- if (exists("GPX_RETENTION_DAYS")) GPX_RETENTION_DAYS else 14
GPX_CLEANUP_ENABLED <- if (exists("GPX_CLEANUP_ENABLED")) GPX_CLEANUP_ENABLED else FALSE
GPX_SEND_LOG_FILE   <- if (exists("GPX_SEND_LOG_FILE")) GPX_SEND_LOG_FILE else file.path("exports", "gpx_send_log.csv")
GPX_ARCHIVE_DIR     <- if (exists("GPX_ARCHIVE_DIR")) GPX_ARCHIVE_DIR else file.path("exports", "sent_gpx")
NOTES_PENDING_DAYS  <- 2
ICLOUD_CLEANUP_STATUS_FILE <- if (exists("ICLOUD_CLEANUP_STATUS_FILE")) ICLOUD_CLEANUP_STATUS_FILE else
  file.path("logs", "icloud_cleanup_status.json")

# ---- Guard ----

# TRUE only for a file directly in <Arc dir>/GPX/, a path under <Arc dir>/Import/,
# or (kind "import_root") <Arc dir>/Import itself. Anything else is refused:
# Backup/, the Arc folder, other folders, "." / ".." segments, empty names.
icloud_cleanup_allowed <- function(path, kind = c("file", "import_root")) {
  kind <- match.arg(kind)
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(ICLOUD_ARC_DIR %||% "")) return(FALSE)
  gpx <- icloud_path("GPX")
  imp <- icloud_path("Import")
  rel <- function(root) substring(path, nchar(root) + 2)
  segs <- strsplit(sub("^[^:]*:", "", path), "/", fixed = TRUE)[[1]]
  if (any(segs %in% c(".", "..", "")) || grepl("(^|/)Backup(/|$)", sub("^[^:]*:", "", path))) return(FALSE)
  if (kind == "import_root") return(identical(path, imp))
  if (startsWith(path, paste0(gpx, "/"))) return(nzchar(rel(gpx)) && !grepl("/", rel(gpx), fixed = TRUE))
  startsWith(path, paste0(imp, "/")) && nzchar(rel(imp))
}

# Delete one remote file; stops (deletes nothing) when the guard refuses the path
icloud_cleanup_delete <- function(path, run = rclone_run) {
  if (!icloud_cleanup_allowed(path, "file")) stop("icloud cleanup: refused to delete '", path, "'", call. = FALSE)
  run(c("deletefile", path), timeout = 120)
}

# Empty folders left in <Arc dir>/Import (Import itself is kept)
icloud_cleanup_rmdirs_import <- function(run = rclone_run) {
  imp <- icloud_path("Import")
  if (!icloud_cleanup_allowed(imp, "import_root")) stop("icloud cleanup: refused rmdirs on '", imp, "'", call. = FALSE)
  run(c("rmdirs", imp, "--leave-root"), timeout = 120)
}

# ---- GPX send log ----

# data.frame(file, sent_utc ("YYYY-mm-ddTHH:MM:SSZ"), size); empty when missing
gpx_send_log_read <- function(file = GPX_SEND_LOG_FILE) {
  empty <- data.frame(file = character(0), sent_utc = character(0), size = numeric(0), stringsAsFactors = FALSE)
  if (!file.exists(file)) return(empty)
  d <- tryCatch(utils::read.csv(file, colClasses = c("character", "character", "numeric"), fileEncoding = "UTF-8"),
                error = function(e) NULL)
  if (is.null(d) || !all(names(empty) %in% names(d))) return(empty)
  d[names(empty)]
}

# A GPX the app just sent: one line in the send log, and a copy in the archive
gpx_send_log_add <- function(local_file, file = GPX_SEND_LOG_FILE, archive_dir = GPX_ARCHIVE_DIR,
                             now = Sys.time()) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(local_file, file.path(archive_dir, basename(local_file)), overwrite = TRUE)
  row <- data.frame(file = basename(local_file), sent_utc = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                    size = file.size(local_file), stringsAsFactors = FALSE)
  utils::write.table(row, file, sep = ",", row.names = FALSE, col.names = !file.exists(file), append = file.exists(file),
                     qmethod = "double", fileEncoding = "UTF-8")
  invisible(row)
}

# ---- Plans (pure) ----

# GPX/ files (data.frame Name, Size, ModTime) against the send log: one row per
# file with action "delete" / "keep" and the reason
gpx_cleanup_plan <- function(remote, log, now = Sys.time(), retention_days = GPX_RETENTION_DAYS) {
  if (is.null(remote) || nrow(remote) == 0) {
    return(data.frame(file = character(0), size = numeric(0), action = character(0), reason = character(0)))
  }
  rows <- lapply(seq_len(nrow(remote)), function(i) {
    f <- remote$Name[i]
    sent <- log[log$file == f, , drop = FALSE]
    if (nrow(sent) == 0) return(list("keep", "not in the send log (not sent by the app)"))
    last <- sent[which.max(as.numeric(as.POSIXct(sent$sent_utc, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))), ]
    if (!isTRUE(all.equal(as.numeric(last$size), as.numeric(remote$Size[i])))) {
      return(list("keep", sprintf("size %.0f B differs from the send log (%.0f B)", remote$Size[i], last$size)))
    }
    age <- as.numeric(difftime(now, as.POSIXct(last$sent_utc, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), units = "days"))
    if (is.na(age)) return(list("keep", "unreadable send time in the send log"))
    if (age <= retention_days) return(list("keep", sprintf("sent %.1f days ago (retention %g days)", age, retention_days)))
    list("delete", sprintf("sent by the app %s, %.1f days ago (> %g)", substr(last$sent_utc, 1, 10), age, retention_days))
  })
  data.frame(file = remote$Name, size = as.numeric(remote$Size), action = vapply(rows, `[[`, "", 1),
             reason = vapply(rows, `[[`, "", 2), stringsAsFactors = FALSE)
}

# Import/ files (lsjson -R --files-only: Path, Size): "empty", "notes" (notes/
# plus at most tiny empty items / places / samples files) or "other"
import_package_kind <- function(files) {
  if (is.null(files) || nrow(files) == 0) return("empty")
  top <- sub("/.*$", "", files$Path)
  if (!any(top == "notes")) return("other")
  data_files <- files[top %in% c("items", "places", "samples"), , drop = FALSE]
  if (any(data_files$Size > 64) || any(!top %in% c("notes", "items", "places", "samples", "metadata.json"))) return("other")
  "notes"
}

# Note ids of a notes/ directory (Arc's or a package's)
notes_ids_in_dir <- function(dir) {
  files <- list.files(dir, "\\.json$", full.names = TRUE)
  unlist(lapply(files, function(f) {
    notes <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
    vapply(notes, function(n) as.character(n$id %||% NA_character_), character(1))
  }), use.names = FALSE)
}

# Decision for a notes package: list(action = "delete" / "keep", reason, pending_since)
import_cleanup_decision <- function(pkg_ids, backup_ids, pushed_at, now = Sys.time(), pending_days = NOTES_PENDING_DAYS) {
  pkg_ids <- unique(pkg_ids[!is.na(pkg_ids)])
  if (length(pkg_ids) == 0) return(list(action = "keep", reason = "no note ids in the package", pending_since = NULL))
  missing <- setdiff(pkg_ids, backup_ids)
  if (length(missing) == 0) {
    return(list(action = "delete", reason = sprintf("all %d notes are in Arc's backup (absorbed)", length(pkg_ids)),
                pending_since = NULL))
  }
  age <- as.numeric(difftime(now, pushed_at, units = "days"))
  list(action = "keep",
       reason = sprintf("%d of %d notes not in Arc's backup yet; sent %.1f days ago", length(missing), length(pkg_ids), age),
       pending_since = if (!is.na(age) && age > pending_days) format(pushed_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}

# ---- Runner ----

# One cleanup pass. dry_run: list what would be deleted and why. log(msg):
# progress lines (no coordinates, no config). Returns the status written to
# status_file (not on a dry run): list(date, ok, reconnect, message, import,
# gpx).
icloud_cleanup <- function(dry_run = TRUE, now = Sys.time(), run = rclone_run, log = function(m) cat(m, "\n", sep = ""),
                           gpx_enabled = GPX_CLEANUP_ENABLED, retention_days = GPX_RETENTION_DAYS,
                           send_log = GPX_SEND_LOG_FILE, archive_dir = GPX_ARCHIVE_DIR,
                           status_file = ICLOUD_CLEANUP_STATUS_FILE) {
  iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  status <- list(date = iso(now), ok = TRUE, reconnect = FALSE, message = "", dry_run = dry_run,
                 import = list(state = "empty"), gpx = list(files = 0L, delete = 0L, enabled = isTRUE(gpx_enabled)))
  finish <- function(st) {
    if (!dry_run && !is.null(status_file)) {
      dir.create(dirname(status_file), recursive = TRUE, showWarnings = FALSE)
      jsonlite::write_json(st, status_file, auto_unbox = TRUE, pretty = TRUE, null = "null")
    }
    st
  }
  failed <- function(what, res) {
    status$ok <- FALSE
    if (icloud_auth_expired(res$output)) {
      status$reconnect <- TRUE
      status$message <- sprintf("rclone login expired (%s): run rclone reconnect %s:", what, ICLOUD_REMOTE)
    } else {
      status$message <- sprintf("rclone failed (%s): %s", what, paste(utils::tail(res$output[nzchar(res$output)], 2), collapse = "; "))
    }
    log(paste("SKIP:", status$message))
    finish(status)
  }
  lsjson <- function(path, recursive = FALSE) {
    res <- run(c("lsjson", "--files-only", if (recursive) "-R", path), timeout = 120)
    if (!res$ok) return(res)
    d <- tryCatch(jsonlite::fromJSON(paste(res$stdout, collapse = "\n")), error = function(e) NULL)
    if (!is.data.frame(d)) d <- data.frame(Path = character(0), Name = character(0), Size = numeric(0), ModTime = character(0))
    list(ok = TRUE, files = d)
  }
  mode <- if (dry_run) "would delete" else "deleting"

  # ---- Import/ ----
  imp <- lsjson(icloud_path("Import"), recursive = TRUE)
  if (!isTRUE(imp$ok)) return(failed("listing Import/", imp))
  kind <- import_package_kind(imp$files)
  log(sprintf("Import/: %d files, %s", nrow(imp$files), switch(kind, empty = "empty", notes = "a notes package",
                                                               other = "not a notes-only package: left alone")))
  status$import <- list(state = kind)
  if (kind == "notes") {
    tmp <- tempfile("icloud_cleanup_")
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    got <- run(c("copy", icloud_path("Import", "notes"), file.path(tmp, "package")), timeout = 120)
    if (!got$ok) return(failed("reading the package's notes", got))
    pkg_ids <- notes_ids_in_dir(file.path(tmp, "package"))
    pushed <- suppressWarnings(max(lubridate::ymd_hms(imp$files$ModTime, quiet = TRUE), na.rm = TRUE))
    backup_id <- icloud_active_backup(run = run)
    if (is.null(backup_id)) return(failed("finding the active backup", list(output = .icloud_state$last_error)))
    got <- run(c("copy", icloud_path("Backup", backup_id, "notes"), file.path(tmp, "backup")), timeout = 300)  # read-only
    if (!got$ok) return(failed("reading the backup's notes", got))
    backup_ids <- notes_ids_in_dir(file.path(tmp, "backup"))
    dec <- import_cleanup_decision(pkg_ids, backup_ids, pushed, now)
    log(sprintf("  package sent %s, %d notes; backup has %d notes: %s -> %s", substr(iso(pushed), 1, 16),
                length(pkg_ids), length(backup_ids), dec$reason, if (dec$action == "delete") mode else "kept"))
    status$import <- list(state = if (dec$action == "delete") "absorbed" else "waiting", sent = iso(pushed),
                          notes = length(pkg_ids), pending_since = dec$pending_since)
    if (dec$action == "delete") {
      for (p in imp$files$Path) {
        log(sprintf("  %s Import/%s", mode, p))
        if (!dry_run) {
          res <- icloud_cleanup_delete(icloud_path("Import", p), run)
          if (!res$ok) return(failed(paste("deleting Import/", p), res))
        }
      }
      if (!dry_run) icloud_cleanup_rmdirs_import(run)
      if (!dry_run) status$import$state <- "deleted"
    }
  }

  # ---- GPX/ ----
  gpx <- lsjson(icloud_path("GPX"))
  if (!isTRUE(gpx$ok)) return(failed("listing GPX/", gpx))
  plan <- gpx_cleanup_plan(gpx$files, gpx_send_log_read(send_log), now, retention_days)
  status$gpx$files <- nrow(plan)
  status$gpx$delete <- sum(plan$action == "delete")
  log(sprintf("GPX/: %d files, %d in the send log, %d older than %g days%s", nrow(plan),
              sum(!grepl("^not in the send log", plan$reason)), sum(plan$action == "delete"), retention_days,
              if (isTRUE(gpx_enabled)) "" else " (GPX cleanup disabled: GPX_CLEANUP_ENABLED = FALSE)"))
  for (i in seq_len(nrow(plan))) {
    f <- plan$file[i]
    if (plan$action[i] == "keep") {
      log(sprintf("  keep %s: %s", f, plan$reason[i]))
      next
    }
    if (!isTRUE(gpx_enabled)) {
      log(sprintf("  would delete when enabled %s: %s", f, plan$reason[i]))
      next
    }
    local <- file.path(archive_dir, f)
    archived <- file.exists(local) && isTRUE(all.equal(file.size(local), plan$size[i]))
    log(sprintf("  %s %s: %s; local copy %s", mode, f, plan$reason[i],
                if (archived) "in the archive" else if (dry_run) "missing: would copy it to the archive first" else
                  "missing: copying it to the archive first"))
    if (dry_run) next
    if (!archived) {
      dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
      got <- run(c("copyto", icloud_path("GPX", f), normalizePath(local, mustWork = FALSE)), timeout = 300)
      if (!got$ok || !isTRUE(all.equal(file.size(local), plan$size[i]))) {
        log(sprintf("  kept %s: could not archive it locally", f))
        next
      }
    }
    res <- icloud_cleanup_delete(icloud_path("GPX", f), run)
    if (!res$ok) return(failed(paste("deleting GPX/", f), res))
  }
  finish(status)
}

# ---- Status for the app's iCloud dot ----

icloud_cleanup_status <- function(file = ICLOUD_CLEANUP_STATUS_FILE) {
  if (!file.exists(file)) return(NULL)
  tryCatch(jsonlite::fromJSON(file, simplifyVector = FALSE), error = function(e) NULL)
}

# Tooltip lines from the last cleanup: reconnect, notes package waiting for
# Restore. "" when nothing to say. Orange: icloud_cleanup_warn().
icloud_cleanup_text <- function(st, lang = APP_LANGUAGE, tz = "America/Sao_Paulo") {
  if (is.null(st)) return("")
  out <- character(0)
  since <- as.POSIXct(st$import$pending_since %||% NA_character_, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (!is.na(since)) {
    out <- c(out, sprintf(tr_lang("Notes package waiting for Restore since %s", lang),
                          format(since, if (identical(lang, "pt")) "%d/%m" else "%Y-%m-%d", tz = tz)))
  }
  # Last: it ends with the command ("...reconnect icloud:")
  if (isTRUE(st$reconnect)) out <- c(out, sprintf(tr_lang("Reconnect: rclone reconnect %s:", lang), ICLOUD_REMOTE))
  paste(out, collapse = ". ")
}

icloud_cleanup_warn <- function(st) {
  !is.null(st) && (isTRUE(st$reconnect) || !is.null(st$import$pending_since))
}
