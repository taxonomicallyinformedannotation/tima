#' @title Run TIMA Shiny app
#'
#' @description Launches the TIMA Shiny web application for interactive
#'     metabolite annotation. Automatically detects Docker containers and
#'     adjusts network settings accordingly.
#'
#' @include install.R
#' @include validations_utils.R
#'
#' @param host [character] Host/IP address to listen on.
#' Default: "127.0.0.1" (localhost). Use "0.0.0.0" to allow external
#'     connections.
#' @param port [integer] Port number to listen on. Default: 3838. Valid range:
#'     1-65535.
#' @param browser [logical] Whether to automatically launch a web browser when
#'     starting the app. Default: TRUE. Automatically set to FALSE in Docker.
#'
#' @return NULL (invisibly). Launches the Shiny app as a side effect.
#'
#' @family workflow
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
  if (is_docker_env()) {
    log_info("Docker environment detected - adjusting settings")
    browser <- FALSE
    host <- "0.0.0.0"
  } else {
    log_info("Running in standard environment")
  }

  log_info("Starting TIMA Shiny app on %s:%d", host, port)

  # Ensure Dependencies ----
  install_tima()

  # Launch Shiny App ----
  app_path <- get_app_path()

  if (!nzchar(app_path) || !app_path_exists(app_path)) {
    cli::cli_abort(
      "app file not found; package may not be properly installed",
      class = c("tima_runtime_error", "tima_error")
    )
  }

  run_shiny_app(
    appDir = build_shiny_app_dir(app_path),
    port = port,
    host = host,
    launch.browser = browser
  )
}

# Helpers kept separate so tests can mock them without invoking Shiny.
is_docker_env <- function() {
  file.exists("/.dockerenv")
}

get_app_path <- function() {
  system.file("app.R", package = "tima")
}

app_path_exists <- function(app_path) {
  file.exists(app_path)
}

build_shiny_app_dir <- function(app_path) {
  shiny::shinyAppFile(appFile = app_path)
}

run_shiny_app <- function(appDir, port, host, launch.browser) {
  # nolint: object_name_linter. Shiny API names.
  shiny::runApp(
    appDir = appDir,
    port = port,
    host = host,
    launch.browser = launch.browser
  )
}
