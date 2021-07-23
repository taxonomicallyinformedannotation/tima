#' Title
#'
#' @return
#' @export
#'
#' @examples
parse_cli_params <- function() {
  log_debug("checking command line arguments")

  if (exists("arguments")) {
    if (!is.null(arguments$output)) {
      params$file$output <- arguments$output
    }
    if (!is.null(arguments$gnps)) {
      params$job$gnps <- arguments$gnps
    }
    if (!is.null(arguments$nap)) {
      params$job$nap <- arguments$nap
    }
  }

  return(params)
}