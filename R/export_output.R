#' @title Export output
#'
#' @noRd
#'
#' @param x TODO
#' @param file TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom crayon green
#' @importFrom readr write_delim
#'
#' @examples TODO
export_output <- function(x, file = output, ...) {
  create_dir(export = file)
  log_debug(
    x = "... path to export is",
    crayon::green(file)
  )
  readr::write_delim(
    x = x,
    file = file,
    delim = "\t",
    na = ""
  )
}
