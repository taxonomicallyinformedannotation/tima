#' @title Run TIMA Shiny app
#'
#' @description Launches the TIMA Shiny web application for interactive
#'     metabolite annotation. Automatically detects Docker containers and
#'     adjusts network settings accordingly.
#'
#' @include install.R
#' @include validators.R
#'
#' @param host Character string specifying the host/IP address to listen on.
#'     Default: "127.0.0.1" (localhost). Use "0.0.0.0" to allow external connections.
#' @param port Integer port number to listen on. Default: 3838. Valid range: 1-65535.
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
#' # Allow external connections (useful in Docker)
#' run_app(host = "0.0.0.0", port = 3838)
#' }
run_app <- function(host = "127.0.0.1", port = 3838, browser = TRUE) {
  # Input Validation ----
  validate_character(host, param_name = "host", allow_empty = FALSE)
  validate_numeric_range(
    port,
    min_value = 1,
    max_value = 65535,
    param_name = "port"
  )
  validate_logical(browser, param_name = "browser")

  # Ensure port is integer
  port <- as.integer(port)

  # Detect Docker Environment ----
  if (file.exists("/.dockerenv")) {
    logger::log_info("Docker environment detected - adjusting settings")
    browser <- FALSE
    host <- "0.0.0.0"
  } else {
    logger::log_info("Running in standard environment")
  }

  logger::log_info("Starting TIMA Shiny app on {host}:{port}")

  # Ensure Dependencies ----
  install()

  # Launch Shiny App ----
  app_path <- system.file("app.R", package = "tima")

  if (!file.exists(app_path)) {
    stop(
      "TIMA app.R file not found. Package may not be properly installed.",
      call. = FALSE
    )
  }

  shiny::runApp(
    appDir = shiny::shinyAppFile(appFile = app_path),
    port = port,
    host = host,
    launch.browser = browser
  )
}
