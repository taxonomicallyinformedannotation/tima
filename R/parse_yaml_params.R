#' @title Parse YAML parameters
#'
#' @description This function parses YAML parameters
#'
#' @importFrom yaml read_yaml
#'
#' @param def Default path
#' @param usr User path
#'
#' @return A list containing the parameters specified in the YAML files
#'
#' @export
#'
#' @examples NULL
parse_yaml_params <- function(def = get("default_path",
                                envir = parent.frame()
                              ),
                              usr = get("user_path",
                                envir = parent.frame()
                              )) {
  ## Read the default YAML file
  params <- read_yaml(file = def)

  ## If a user-specified YAML file exists,
  ## read it and overwrite the default values with the user-specified ones
  if (file.exists(usr)) {
    params <- read_yaml(file = usr)
  }

  ## Return the final list of parameters
  return(params)
}
