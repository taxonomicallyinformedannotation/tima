# TIMA Shiny Application entry-point.
# Kept intentionally small: resolve sibling files portably, then launch.

.app_dir <- tryCatch(
  {
    frames <- sys.frames()
    ofiles <- Filter(Negate(is.null), lapply(frames, `[[`, "ofile"))
    dirs <- vapply(
      rev(ofiles),
      function(f) dirname(normalizePath(f, mustWork = FALSE)),
      character(1L)
    )
    hit <- dirs[file.exists(file.path(dirs, "app_ui.R"))]
    if (length(hit) > 0L) {
      return(hit[[1L]])
    }
    system.file(package = "tima")
  },
  error = function(e) system.file(package = "tima")
)

source(file.path(.app_dir, "app_ui.R"))
source(file.path(.app_dir, "app_server.R"))

.log_info <- getFromNamespace("log_info", "tima")
.copy_backbone <- getFromNamespace("copy_backbone", "tima")

default_host <- if (file.exists("/.dockerenv")) "0.0.0.0" else "127.0.0.1"
host <- getOption("tima.app.host", default = default_host)
port <- as.integer(getOption("tima.app.port", default = 3838L))
url <- sprintf("http://%s:%d", host, port)
options(shiny.maxRequestSize = 2000 * 1024^2)
.log_info("Please, open: %s on your favorite browser, but not Edge.", url)
shiny::shinyApp(
  ui = ui,
  server = server,
  onStart = function() {
    .copy_backbone()
    tima::go_to_cache()
  },
  options = list(host = host, port = port)
)
