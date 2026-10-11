# Disables Shiny's automatic sourcing of R/*.R (alphabetical order).
# app.R sources config_local.R and the modules itself, in dependency order:
# ui.R needs utils_icloud.R, and config_local.R must load before the modules
# so its overrides (e.g. ICLOUD_REMOTE) aren't shadowed by module defaults.
