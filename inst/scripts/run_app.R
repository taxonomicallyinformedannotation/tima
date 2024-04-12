# Check if runs in Docker environment or not
if (file.exists("/.dockerenv")) {
  # Print message if inside Docker environment
  system('echo "I\'m inside matrix ;("')
  b <- FALSE
  h <- "0.0.0.0"
} else {
  # Print message if not inside Docker environment
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
