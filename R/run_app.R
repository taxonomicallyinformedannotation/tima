#' @title Run app
#'
#' @description This function runs the app
#'
#' @include install.R
#'
#' @param host Host. Default to 127.0.0.1
#' @param port Port. Default to 3838
#' @param browser Flag for browser use. Default to TRUE
#'
#' @return Opens the app
#'
#' @export
#'
#' @examples NULL
run_app <- function(host = "127.0.0.1",
                    port = 3838,
                    browser = TRUE) {
  if (file.exists("/.dockerenv")) {
    system('echo "I\'m inside the matrix!"')
    browser <- FALSE
    host <- "0.0.0.0"
  } else {
    system('echo "I\'m living in the real world!"')
  }
  install()
  shiny::runApp(
    appDir = shiny::shinyAppFile(system.file("app.R", package = "tima")),
    port = port,
    host = host,
    launch.browser = browser
  )
}
