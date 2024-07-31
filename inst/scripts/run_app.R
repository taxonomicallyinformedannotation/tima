# Check if runs in Docker environment or not
if (file.exists("/.dockerenv")) {
  system('echo "I\'m inside the matrix ;("')
  b <- FALSE
  h <- "0.0.0.0"
} else {
  # Check if running latest version
  tryCatch(
    expr = {
      install()
    },
    error = function(e) {
      source("R/install.R")
      install()
    }
  )
  system('echo "I\'m living in the real world!"')
  b <- TRUE
  h <- "127.0.0.1"
}

shiny::runApp(
  appDir = "inst",
  port = 3838,
  host = h,
  launch.browser = b
)
