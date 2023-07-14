utils::globalVariables(
  c(
    "default_path",
    "user_path"
  )
)

#' @title Parse YAML parameters
#'
#' @description This function parses YAML parameters
#'
#' @param def Default path
#' @param usr User path
#'
#' @return A list containing the parameters specified in the YAML files
#'
#' @export
#'
#' @examples NULL
parse_yaml_params <- function(def = default_path, usr = user_path) {
  ## Read the default YAML file
  suppressWarnings(
    params <- yaml::read_yaml(file = def)
  )

  ## If a user-specified YAML file exists,
  ## read it and overwrite the default values with the user-specified ones
  if (file.exists(usr)) {
    suppressWarnings(
      params <- yaml::read_yaml(file = usr)
    )
  }

  ## Return the final list of parameters
  return(params)
}
