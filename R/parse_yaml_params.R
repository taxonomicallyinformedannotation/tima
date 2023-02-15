#' @title Parse YAML parameters
#'
#' @description This function parses YAML parameters
#'
#' @param def Default path
#' @param par Param path
#'
#' @return A list containing the parameters specified in the YAML files
#'
#' @export
#'
#' @importFrom purrr list_flatten
#' @importFrom yaml read_yaml
#'
#' @examples NULL
parse_yaml_params <- function(def = default_path, par = params_path) {
  # Initialize an empty list to store the parameters
  params <- list()

  # Read the default YAML file
  log_debug("Loading yaml parameters")
  suppressWarnings(params <-
    yaml::read_yaml(file = def, handlers = list(
      seq = function(x) {
        purrr::list_flatten(x)
      }
    )))

  # If a user-specified YAML file exists, read it and overwrite the default values with the user-specified ones
  if (file.exists(par)) {
    suppressWarnings(params <-
      yaml::read_yaml(file = par, handlers = list(
        seq = function(x) {
          purrr::list_flatten(x)
        }
      )))
  }

  # Return the final list of parameters
  return(params)
}
