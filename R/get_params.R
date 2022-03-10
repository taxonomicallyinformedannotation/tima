if (!require(docopt)) {
  install.packages("docopt")
  require(package = "docopt", quietly = TRUE)
}

#' Title
#'
#' @noRd
#'
#' @param step TODO
#'
#' @return TODO
#' @export
#'
#' @examples
get_params <- function(step) {
  stopifnot(
    "Your step does not exist. Valid steps are:
    'fake_edges',
    'fake_features_components',
    'prepare_adducts',
    'prepare_closed',
    'prepare_edges',
    'prepare_features_classification',
    'prepare_features_components',
    'prepare_gnps',
    'prepare_isdb',
    'prepare_library',
    'prepare_sirius',
    'prepare_taxa',
    'process_annotations'
    " = step %in% c(
      "fake_edges",
      "fake_features_components",
      "prepare_adducts",
      "prepare_closed",
      "prepare_edges",
      "prepare_features_classification",
      "prepare_features_components",
      "prepare_gnps",
      "prepare_isdb",
      "prepare_library",
      "prepare_sirius",
      "prepare_taxa",
      "process_annotations"
    )
  )

  doc_path <<-
    file.path(paths$inst$scripts$docopt, paste0(step, ".txt"))
  default_path <<-
    file.path(paths$config$default$path, paste0(step, ".yaml"))
  params_path <<-
    file.path(paths$config$params$path, paste0(step, ".yaml"))

  doc <<- readChar(
    con = doc_path,
    nchars = file.info(doc_path)$size
  )

  arguments <<- docopt::docopt(doc)

  params <<- parse_yaml_params()

  params <<- parse_cli_params()

  return(params)
}
