#' Title
#'
#' @return
#' @export
#'
#' @examples
parse_cli_params <- function() {
  log_debug("checking command line arguments")

  if (exists("arguments")) {
    if (!is.null(arguments$ms1)) {
      params$ms$annotate <- arguments$ms1
    }
    if (!is.null(arguments$annotations)) {
      params$annotations <- arguments$annotations
    }
    if (!is.null(arguments$column.name)) {
      params$column_name <- arguments$column.name
    }
    if (!is.null(arguments$components)) {
      params$components <- arguments$components
    }
    if (!is.null(arguments$edges)) {
      params$edges <- arguments$edges
    }
    if (!is.null(arguments$extension)) {
      params$extension <- arguments$extension
    }
    if (!is.null(arguments$filter)) {
      params$mode <- arguments$filter
    }
    if (!is.null(arguments$force)) {
      params$force <- arguments$force
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
    if (!is.null(arguments$library)) {
      params$library <- arguments$library
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
    if (!is.null(arguments$ppm)) {
      params$ms$tolerance$ppm <- arguments$ppm
    }
    if (!is.null(arguments$quickmode)) {
      params$quickmode <- arguments$quickmode
    }
    if (!is.null(arguments$rt)) {
      params$ms$tolerance$rt <- arguments$rt
    }
    if (!is.null(arguments$source)) {
      params$source_name <-
        arguments$source
    }
    if (!is.null(arguments$target)) {
      params$target_name <-
        arguments$target
    }
    if (!is.null(arguments$taxa)) {
      params$taxa <-
        arguments$taxa
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
