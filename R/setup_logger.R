#' Setup logger
#'
#' @param filename Log file name. Default: `"tima.log"`.
#' @return NULL
#' @keywords internal
setup_logger <- function(filename) {
  # logger::log_layout(logger::layout_glue_colors)
  logger::log_threshold(logger::TRACE)
  logger::log_appender(
    appender = logger::appender_tee(
      file = filename
    )
  )
}
