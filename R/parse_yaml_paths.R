if (!require(purrr)) {
  install.packages("purrr")
  require(package = "purrr", quietly = TRUE)
}
if (!require(yaml)) {
  install.packages("yaml")
  require(package = "yaml", quietly = TRUE)
}

#' Title
#'
#' @noRd
#'
#' @param file TODO
#'
#' @return TODO
#' @export
#'
#' @importFrom purrr flatten
#' @importFrom yaml read_yaml
#'
#' @examples
parse_yaml_paths <- function(file = "paths.yaml") {
  log_debug("Loading paths")
  suppressWarnings(paths <- yaml::read_yaml(
    file = file,
    handlers = list(
      seq = function(x) {
        purrr::flatten(x)
      }
    )
  ))
  setwd(paths$base_dir)

  return(paths)
}
