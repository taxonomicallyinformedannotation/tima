# TIMA Shiny Application entry-point.
# Kept intentionally small: resolve sibling files portably, then launch.

.app_dir <- tryCatch(
  {
    frames <- sys.frames()
    ofiles <- Filter(Negate(is.null), lapply(frames, `[[`, "ofile"))
    for (f in rev(ofiles)) {
      d <- dirname(normalizePath(f, mustWork = FALSE))
      if (file.exists(file.path(d, "app_ui.R"))) return(d)
    }
    system.file(package = "tima")
  },
  error = function(e) system.file(package = "tima")
)

source(file.path(.app_dir, "app_ui.R"))
source(file.path(.app_dir, "app_server.R"))

url <- "<http://127.0.0.1:3838>"
if (file.exists("/.dockerenv")) {
  host <- "0.0.0.0"
} else {
  host <- "127.0.0.1"
}
options(shiny.maxRequestSize = 2000 * 1024^2)
tima:::log_info("Please, open: %s on your favorite browser, but not Edge.", url)
shiny::shinyApp(
  ui = ui,
  server = server,
  onStart = function() {
    tima:::copy_backbone()
    tima::go_to_cache()
  },
  options = list(host = host, port = 3838)
)
