# Check if runs in Docker environment or not
if (file.exists("/.dockerenv")) {
  system('echo "I\'m inside matrix ;("')
  b <- FALSE
  h <- "0.0.0.0"
} else {
  # Check if running latest version
  tryCatch(
    expr = {
      install_latest_version()
    },
    error = function(e) {
      source("R/install_latest_version.R")
      install_latest_version()
    }
  )
  system('echo "I\'m living in real world!"')
  b <- TRUE
  h <- "127.0.0.1"
}

shiny::runApp(
  appDir = "inst",
  port = 3838,
  host = h,
  launch.browser = b
)
