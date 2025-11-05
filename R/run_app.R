#' @title Run app
#'
#' @description This function launches the TIMA Shiny web application for
#'     interactive metabolite annotation. It automatically detects if running
#'     inside a Docker container and adjusts network settings accordingly.
#'
#' @include install.R
#'
#' @param host Character string specifying the host/IP address to listen on.
#'     Default: "127.0.0.1" (localhost). Use "0.0.0.0" to allow external connections.
#' @param port Integer port number to listen on. Default: 3838
#' @param browser Logical whether to automatically launch a web browser when
#'     starting the app. Default: TRUE. Automatically set to FALSE in Docker.
#'
#' @return NULL (invisibly). Launches the Shiny app as a side effect.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch app on localhost
#' run_app()
#'
#' # Launch on custom port
#' run_app(port = 8080)
#'
#' # Allow external connections
#' run_app(host = "0.0.0.0", port = 3838)
#' }
run_app <- function(host = "127.0.0.1", port = 3838, browser = TRUE) {
  # Validate inputs
  if (!is.character(host) || length(host) != 1L || nchar(host) == 0L) {
    stop("host must be a non-empty character string")
  }

  if (!is.numeric(port) || length(port) != 1L || port < 1L || port > 65535L) {
    stop("port must be an integer between 1 and 65535")
  }
  port <- as.integer(port)

  if (!is.logical(browser) || length(browser) != 1L) {
    stop("browser must be a single logical value")
  }

  # Detect Docker environment and adjust settings
  if (file.exists("/.dockerenv")) {
    logger::log_info("Detected Docker environment - adjusting network settings")
    browser <- FALSE
    host <- "0.0.0.0"
  } else {
    logger::log_info("Running in standard environment")
  }

  logger::log_info("Starting TIMA Shiny app on {host}:{port}")

  # Ensure dependencies are installed
  install()

  # Launch the Shiny app
  app_path <- system.file("app.R", package = "tima")
  if (!file.exists(app_path)) {
    stop("TIMA app.R file not found. Package may not be properly installed.")
  }

  shiny::runApp(
    appDir = shiny::shinyAppFile(app_path),
    port = port,
    host = host,
    launch.browser = browser
  )
}
