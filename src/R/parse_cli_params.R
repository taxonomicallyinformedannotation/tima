#' Title
#'
#' @return
#' @export
#'
#' @examples
parse_cli_params <- function() {
  log_debug("checking command line arguments")

  if (exists("arguments")) {
    if (!is.null(arguments$filter)) {
      params$filter$mode <- arguments$filter
    }
    if (!is.null(arguments$gnps)) {
      params$job$gnps <- arguments$gnps
    }
    if (!is.null(arguments$input)) {
      params$file$input <- arguments$input
    }
    if (!is.null(arguments$level)) {
      params$filter$level <- arguments$level
    }
    if (!is.null(arguments$nap)) {
      params$job$nap <- arguments$nap
    }
    if (!is.null(arguments$output)) {
      params$file$output <- arguments$output
    }
    if (!is.null(arguments$source)) {
      params$source_name <-
        arguments$source
    }
    if (!is.null(arguments$target)) {
      params$target_name <-
        arguments$target
    }
    if (!is.null(arguments$tool)) {
      params$tool <-
        arguments$tool
    }
    if (!is.null(arguments$value)) {
      params$filter$value <- arguments$value
    }
  }

  return(params)
}