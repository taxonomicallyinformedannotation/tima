# Check if runs in Docker environment or not
if (file.exists("/.dockerenv")) {
  # Print message if inside Docker environment
  system('echo "I\'m inside matrix ;("')
  b <- FALSE
} else {
  # Print message if not inside Docker environment
  system('echo "I\'m living in real world!"')
  b <- TRUE
}

shiny::runApp(
  appDir = "inst/app",
  port = 3838,
  host = "0.0.0.0",
  launch.browser = b
)
