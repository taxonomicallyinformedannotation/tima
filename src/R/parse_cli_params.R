#' Title
#'
#' @return
#' @export
#'
#' @examples
parse_cli_params <- function() {
  log_debug("checking command line arguments")

  if (exists("arguments")) {
    if (!is.null(arguments$column.name)) {
      params$column_name <- arguments$column.name
    }
    if (!is.null(arguments$components)) {
      params$components <- arguments$components
    }
    if (!is.null(arguments$gnps)) {
      params$gnps <- arguments$gnps
    }
    if (!is.null(arguments$input)) {
      params$input <- arguments$input
    }
    if (!is.null(arguments$k.top)) {
      params$top_k <-
        as.numeric(arguments$k.top)
    }
    if (!is.null(arguments$level)) {
      params$level <- arguments$level
    }
    if (!is.null(arguments$filter)) {
      params$mode <- arguments$filter
    }
    if (!is.null(arguments$mode)) {
      params$mode <- arguments$mode
    }
    if (!is.null(arguments$nap)) {
      params$nap <- arguments$nap
    }
    if (!is.null(arguments$output)) {
      params$output <- arguments$output
    }
    if (!is.null(arguments$quickmode)) {
      params$quickmode <- arguments$quickmode
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