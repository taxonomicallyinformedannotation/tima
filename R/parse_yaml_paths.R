#' @title Parse YAML paths
#'
#' @param file The file name of the YAML file containing the paths (default is "paths.yaml")
#'
#' @return A list containing the paths specified in the YAML file
#'
#' @export
#'
#' @importFrom purrr list_flatten
#' @importFrom yaml read_yaml
#'
#' @examples paths <- parse_yaml_paths()
parse_yaml_paths <- function(file = "paths.yaml") {
  # Read the YAML file containing the paths
  log_debug("Loading paths")
  suppressWarnings(paths <- yaml::read_yaml(
    file = file,
    handlers = list(
      seq = function(x) {
        purrr::list_flatten(x)
      }
    )
  ))

  # Set the working directory to the base directory specified in the YAML file
  setwd(paths$base_dir)

  # Return the list of paths
  return(paths)
}
