# ---- Helpers: UI language (PT / EN) ----
# Translations: i18n/translations.json, {"<English key>": {"pt": "...", "en": "..."}}.
# Static UI text is rendered with t_() in APP_LANGUAGE and re-translated in the
# browser when the PT | EN toggle changes (see the i18n script in ui.R).
# Server messages use tr() in server.R, which follows input$app_lang.

I18N_LANGS <- c("pt", "en")

# Default language (config_local.R can set APP_LANGUAGE <- "en")
APP_LANGUAGE <- if (exists("APP_LANGUAGE") && isTRUE(APP_LANGUAGE %in% I18N_LANGS)) APP_LANGUAGE else "pt"

I18N <- jsonlite::fromJSON("i18n/translations.json", simplifyVector = FALSE)

# Translate a key; falls back to the key itself (English)
tr_lang <- function(key, lang = APP_LANGUAGE) {
  entry <- I18N[[key]]
  if (is.null(entry)) {
    message("[i18n] Missing key: ", key)
    return(key)
  }
  entry[[lang]] %||% key
}

# Marks a string that is translated elsewhere (e.g. client-side), so the key
# checker sees it as used. Returns the string unchanged.
i18n_key <- function(key) key

# Static UI text: rendered in APP_LANGUAGE, swapped in the browser on toggle
t_ <- function(key) shiny::span(`data-i18n` = key, tr_lang(key))

# Named choices with translated labels (values unchanged)
tr_choices <- function(choices, lang = APP_LANGUAGE) {
  stats::setNames(unname(choices), vapply(names(choices), tr_lang, character(1), lang = lang))
}

# ---- Dates: dd/mm/yyyy in PT, yyyy-mm-dd in EN ----
date_format_r <- function(lang) if (identical(lang, "pt")) "%d/%m/%Y" else "%Y-%m-%d"

fmt_datetime <- function(x, lang, seconds = FALSE) {
  format(x, paste(date_format_r(lang), if (seconds) "%H:%M:%S" else "%H:%M"))
}

datepicker_language <- function(lang) if (identical(lang, "pt")) "pt-BR" else "en"
datepicker_format <- function(lang) if (identical(lang, "pt")) "dd/mm/yyyy" else "yyyy-mm-dd"

# dateInput with the display language/format for lang (value stays yyyy-mm-dd)
date_input <- function(inputId, label, value, lang = APP_LANGUAGE, ...) {
  shiny::dateInput(inputId, label, value,
                   language = datepicker_language(lang), format = datepicker_format(lang), ...)
}
