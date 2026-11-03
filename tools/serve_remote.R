# Serves the app to your other devices (e.g. the work computer over Tailscale):
# it listens on every interface, not only on this machine. The OSRM containers
# already do (Docker publishes 0.0.0.0:5001-5006), and the map's router
# network asks the host the page was opened from (osrmUrlForBrowser in
# ui_route_edit_js.R), so nothing else changes.
# Never forward these ports on the router: the app and OSRM have no login.
# Usage (from the project root): Rscript tools/serve_remote.R [port]
#   then open http://<this machine's Tailscale address>:<port> elsewhere
#   (`tailscale ip -4` prints the address).
args <- commandArgs(TRUE)
port <- if (length(args)) as.integer(args[1]) else 7932L
if (is.na(port)) stop("port must be a number")
shiny::runApp(".", host = "0.0.0.0", port = port, launch.browser = FALSE)
