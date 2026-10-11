# Checks i18n/translations.json against the translation keys used in the code:
# every used key exists with non-empty "pt" and "en", no key is unused, and
# sprintf placeholders (%d, %s, %.1f, ...) match between the two languages.
#
# Usage (from the project root): Rscript i18n/check_translations.R
#   --list   print the keys found in the code and exit

files <- c(list.files("R", "\\.R$", full.names = TRUE), "app.R")

# Functions whose string-literal arguments (by position) are translation keys
KEY_ARGS <- list(
  t_ = 1, tr = 1, tr_lang = 1, i18n_key = 1, icloud_msg = 1,
  help_icon = 1, mode_hint = 1:2, date_time_row = 1
)
# Named vectors whose names are labels (keys)
LABEL_VECTORS <- c("PROFILE_CHOICES", "ACTIVITY_CHOICES", "REVIEW_ACTIVITY_CHOICES", "REVIEW_SORT_CHOICES",
                   "REVIEW_PROFILE_CHOICES")

fn_name <- function(fn) {
  if (is.name(fn)) return(as.character(fn))
  if (is.call(fn) && identical(fn[[1]], as.name("::"))) return(as.character(fn[[3]]))
  ""
}

positional <- function(args) {
  nm <- names(args)
  if (is.null(nm)) args else args[nm == ""]
}

used <- character(0)
walk <- function(e) {
  if (!is.call(e)) return(invisible())
  name <- fn_name(e[[1]])
  args <- as.list(e)[-1]
  pos <- positional(args)

  if (name %in% names(KEY_ARGS)) {
    for (i in KEY_ARGS[[name]]) {
      if (i <= length(pos) && is.character(pos[[i]])) used <<- c(used, pos[[i]])
    }
  }
  # mode_segments(name, c("Label" = "VALUE", ...), selected)
  if (name == "mode_segments" && length(pos) >= 2 && is.call(pos[[2]])) {
    used <<- c(used, names(as.list(pos[[2]])[-1]))
  }
  # PROFILE_CHOICES <- c("Label" = "value", ...)
  if (name %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% LABEL_VECTORS && is.call(e[[3]])) {
    used <<- c(used, names(as.list(e[[3]])[-1]))
  }

  for (a in args) if (!missing(a)) walk(a)
  invisible()
}

for (f in files) for (e in parse(f, keep.source = FALSE)) walk(e)

# Keys used only by the client-side script: i18nText('...')
js_text <- paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")
js_keys <- regmatches(js_text, gregexpr("i18nText\\('([^']+)'", js_text))[[1]]
used <- unique(c(used, sub("^i18nText\\('", "", sub("'$", "", js_keys))))
used <- used[nzchar(used)]

if ("--list" %in% commandArgs(TRUE)) {
  cat(sort(used), sep = "\n")
  quit(status = 0)
}

dict <- jsonlite::fromJSON("i18n/translations.json", simplifyVector = FALSE)
placeholders <- function(x) regmatches(x, gregexpr("%[-+ #0]*[0-9]*(\\.[0-9]+)?[sdifgx%]", x))[[1]]

missing_keys <- setdiff(used, names(dict))
unused_keys <- setdiff(names(dict), used)
incomplete <- names(dict)[vapply(dict, function(d) {
  !all(c("pt", "en") %in% names(d)) || !nzchar(d$pt %||% "") || !nzchar(d$en %||% "")
}, logical(1))]
bad_placeholders <- names(dict)[vapply(names(dict), function(k) {
  d <- dict[[k]]
  !identical(placeholders(k), placeholders(d$en %||% "")) || !identical(placeholders(k), placeholders(d$pt %||% ""))
}, logical(1))]

report <- function(title, x) {
  cat(sprintf("%-40s %d\n", title, length(x)))
  if (length(x)) cat(paste0("  - ", x), sep = "\n")
}
cat(sprintf("Keys used in code: %d | keys in translations.json: %d\n", length(used), length(dict)))
report("Missing (used but not translated):", missing_keys)
report("Unused (translated but not used):", unused_keys)
report("Incomplete (empty pt/en):", incomplete)
report("Placeholder mismatch (key/en/pt):", bad_placeholders)

ok <- !length(missing_keys) && !length(unused_keys) && !length(incomplete) && !length(bad_placeholders)
cat(if (ok) "OK\n" else "FAILED\n")
quit(status = if (ok) 0 else 1)
