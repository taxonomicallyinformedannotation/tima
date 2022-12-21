#' @title Parse YAML paths
#'
#' @noRd
#'
#' @param file TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom purrr list_flatten
#' @importFrom yaml read_yaml
#'
#' @examples TODO
parse_yaml_paths <- function(file = "paths.yaml") {
  log_debug("Loading paths")
  suppressWarnings(paths <- yaml::read_yaml(
    file = file,
    handlers = list(
      seq = function(x) {
        purrr::list_flatten(x)
      }
    )
  ))
  setwd(paths$base_dir)

  return(paths)
}
