#' Title
#'
#' @noRd
#'
#' @param file TODO
#'
#' @return TODO
#' @export
#'
#' @importFrom yaml read_yaml
#'
#' @examples
parse_yaml_paths <- function(file = "paths.yaml") {
  log_debug("Loading paths")
  suppressWarnings(paths <- yaml::read_yaml(
    file = file,
    handlers = list(
      seq = function(x) {
        flatten(x)
      }
    )
  ))
  setwd(paths$base_dir)

  return(paths)
}
