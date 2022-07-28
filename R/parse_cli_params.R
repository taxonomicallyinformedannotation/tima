#' Title
#'
#' @noRd
#'
#' @return TODO
#' @export
#'
#' @examples
parse_cli_params <- function() {
  log_debug("Loading command line arguments")

  if (exists("arguments")) {
    if (!is.null(arguments$gnps)) {
      params$annotation$gnps <- arguments$gnps
      params$gnps <- arguments$gnps
    }
    if (!is.null(arguments$isdb)) {
      params$annotation$isdb <- arguments$isdb
    }
    if (!is.null(arguments$zirius)) {
      params$annotation$sirius <- arguments$zirius
    }
    if (!is.null(arguments$biological)) {
      params$weights$biological <- as.numeric(arguments$biological)
    }
    if (!is.null(arguments$column.name)) {
      params$column_name <- arguments$column.name
    }
    if (!is.null(arguments$complement)) {
      params$ms$annotate <- arguments$complement
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
    if (!is.null(arguments$features)) {
      params$taxa <- arguments$features
      params$feature_name <- arguments$features
    }
    if (!is.null(arguments$filename)) {
      params$filename <- arguments$filename
    }
    if (!is.null(arguments$filter)) {
      params$filter$mode <- arguments$filter
    }
    if (!is.null(arguments$force)) {
      params$force <- arguments$force
    }
    if (!is.null(arguments$qemical)) {
      params$weights$chemical <- as.numeric(arguments$qemical)
    }
    if (!is.null(arguments$j.top)) {
      params$top_k$initial <- as.numeric(arguments$j.top)
    }
    if (!is.null(arguments$input)) {
      params$input <- arguments$input
    }
    if (!is.null(arguments$k.top)) {
      params$top_k$final <-
        as.numeric(arguments$k.top)
    }
    if (!is.null(arguments$level)) {
      params$filter$level <- as.numeric(arguments$level)
    }
    if (!is.null(arguments$library)) {
      params$library <- arguments$library
    }
    if (!is.null(arguments$metadata)) {
      params$metadata <- arguments$metadata
    }
    if (!is.null(arguments$mode)) {
      params$mode <- arguments$mode
    }
    if (!is.null(arguments$ms)) {
      params$ms$mode <- arguments$ms
    }
    if (!is.null(arguments$name)) {
      params$name <- arguments$name
    }
    if (!is.null(arguments$nap)) {
      params$nap <- arguments$nap
    }
    if (!is.null(arguments$output)) {
      params$output <- arguments$output
    }
    if (!is.null(arguments$ppm)) {
      params$ms$tolerance$ppm <- as.numeric(arguments$ppm)
    }
    if (!is.null(arguments$quickmode)) {
      params$quickmode <- arguments$quickmode
    }
    if (!is.null(arguments$rt)) {
      params$ms$tolerance$rt <- as.numeric(arguments$rt)
      params$rt_name <- arguments$rt
    }
    if (!is.null(arguments$precursor)) {
      params$precursor_name <- arguments$precursor
    }
    if (!is.null(arguments$source)) {
      params$source_name <- arguments$source
    }
    if (!is.null(arguments$spectral)) {
      params$weights$spectral <- as.numeric(arguments$spectral)
    }
    if (!is.null(arguments$target)) {
      params$target_name <- arguments$target
    }
    if (!is.null(arguments$taxon)) {
      params$taxon <- arguments$taxon
    }
    if (!is.null(arguments$tool)) {
      params$tool <- arguments$tool
    }
    if (!is.null(arguments$value)) {
      params$filter$value <- arguments$value
    }
    if (!is.null(arguments$npc)) {
      params$npc <- arguments$npc
    }
    if (!is.null(arguments$workflow)) {
      params$workflow <- arguments$workflow
    }
    if (!is.null(arguments$xbim)) {
      params$ms$ms1only <- arguments$xbim
    }
  }

  if (exists("params")) {
    return(params)
  }
}
