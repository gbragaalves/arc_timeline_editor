# Daily cleanup of what the app pushes to iCloud: <Arc dir>/Import (notes
# packages Arc has absorbed) and <Arc dir>/GPX (GPX files the app sent more than
# GPX_RETENTION_DAYS ago; off unless GPX_CLEANUP_ENABLED). Nothing else is ever
# touched; see R/utils_icloud_cleanup.R. Logs to logs/icloud_cleanup_YYYY-MM-DD.log
# (the last 14 kept); the outcome goes to logs/icloud_cleanup_status.json for the
# app's iCloud status dot.
# Usage (any directory): Rscript tools/icloud_cleanup.R [--dry-run]
# cron (crontab -l), with a lock so two runs never overlap:
#   30 3 * * * cd /home/gbragaalves/projetos_r/arc_desktop && /usr/bin/flock -n logs/.icloud_cleanup.lock /usr/bin/Rscript tools/icloud_cleanup.R >/dev/null 2>&1
args <- commandArgs(TRUE)
unknown <- setdiff(args, "--dry-run")
if (length(unknown)) stop("Unknown option: ", paste(unknown, collapse = " "))
dry_run <- "--dry-run" %in% args

# Project root from the script's path; cron has a minimal PATH
file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (length(file_arg)) setwd(dirname(dirname(normalizePath(file_arg))))
Sys.setenv(PATH = paste("/usr/local/bin:/usr/bin:/bin", Sys.getenv("PATH"), sep = ":"))

`%||%` <- function(x, y) if (!is.null(x)) x else y
if (file.exists("config_local.R")) source("config_local.R")
source("R/utils_icloud.R")
source("R/utils_icloud_cleanup.R")

dir.create("logs", showWarnings = FALSE)
log_file <- file.path("logs", sprintf("icloud_cleanup_%s.log", format(Sys.Date())))
con <- file(log_file, open = "a", encoding = "UTF-8")
log <- function(msg) {
  line <- sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg)
  cat(line, "\n", sep = "")
  writeLines(line, con)
  flush(con)
}

log(sprintf("== icloud_cleanup.R%s (user %s, HOME %s, rclone %s)", if (dry_run) " --dry-run" else "",
            Sys.info()[["user"]], Sys.getenv("HOME"), if (nzchar(Sys.which("rclone"))) "found" else "NOT FOUND"))
st <- if (!nzchar(Sys.which("rclone"))) {
  list(ok = FALSE, message = "rclone not found in PATH")
} else {
  tryCatch(icloud_cleanup(dry_run = dry_run, log = log), error = function(e) list(ok = FALSE, message = conditionMessage(e)))
}
log(sprintf("== %s%s", if (isTRUE(st$ok)) "done" else "SKIPPED / FAILED",
            if (nzchar(st$message %||% "")) paste0(": ", st$message) else ""))
if (dry_run) log("(dry run: nothing deleted, status file not written)")
close(con)

old <- list.files("logs", "^icloud_cleanup_\\d{4}-\\d{2}-\\d{2}\\.log$", full.names = TRUE)
old <- sort(old, decreasing = TRUE)
if (length(old) > 14) unlink(old[-(1:14)])
