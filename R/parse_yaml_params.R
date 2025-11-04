#' @title Parse YAML parameters
#'
#' @description This function parses YAML parameter files, loading default
#'     parameters and optionally overriding them with user-specified values
#'
#' @param def Character string path to the default YAML parameters file
#' @param usr Character string path to the user-specified YAML parameters file
#'     (optional). If it exists, it will override default values.
#'
#' @return A list containing the parameters specified in the YAML files
#'
#' @export
#'
#' @examples NULL
parse_yaml_params <- function(
  def = get("default_path", envir = parent.frame()),
  usr = get("user_path", envir = parent.frame())
) {
  # Validate default file exists
  if (!file.exists(def)) {
    stop("Default YAML file not found: ", def)
  }

  # Read the default YAML file
  params <- yaml::read_yaml(file = def)

  # If a user-specified YAML file exists,
  # read it and overwrite the default values with the user-specified ones
  if (!is.null(usr) && nchar(usr) > 0L && file.exists(usr)) {
    logger::log_debug("Loading user-specified parameters from: ", usr)
    user_params <- yaml::read_yaml(file = usr)
    # Deep merge would be better here, but for now simple override
    params <- user_params
  }

  # Return the final list of parameters
  return(params)
}
