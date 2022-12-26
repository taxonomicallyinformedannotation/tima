#' @title Parse YAML parameters
#'
#' @return A list containing the parameters specified in the YAML files
#'
#' @export
#'
#' @importFrom purrr list_flatten
#' @importFrom yaml read_yaml
#'
#' @examples params <- parse_yaml_params()
parse_yaml_params <- function() {
  # Initialize an empty list to store the parameters
  params <- list()

  # Read the default YAML file
  log_debug("Loading yaml parameters")
  suppressWarnings(params <-
    yaml::read_yaml(file = default_path, handlers = list(
      seq = function(x) {
        purrr::list_flatten(x)
      }
    )))

  # If a user-specified YAML file exists, read it and overwrite the default values with the user-specified ones
  if (file.exists(params_path)) {
    suppressWarnings(params <-
      yaml::read_yaml(file = params_path, handlers = list(
        seq = function(x) {
          purrr::list_flatten(x)
        }
      )))
  }

  # Return the final list of parameters
  return(params)
}
