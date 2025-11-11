#' @title Parse YAML parameters
#'
#' @description This function parses YAML parameter files, loading default
#'     parameters and optionally overriding them with user-specified values.
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
  params <- tryCatch(
    yaml::read_yaml(file = def),
    error = function(e) {
      stop("Failed to parse default YAML file: ", conditionMessage(e))
    }
  )

  # Validate that parameters were successfully loaded
  if (!is.list(params) || length(params) == 0L) {
    stop("Default YAML file is empty or invalid: ", def)
  }

  # If a user-specified YAML file exists, merge it with defaults
  if (!is.null(usr) && nchar(usr) > 0L && file.exists(usr) && usr != def) {
    if (usr != "params/prepare_params.yaml") {
      logger::log_debug("Loading user-specified parameters from: {usr}")
    }
    user_params <- tryCatch(
      yaml::read_yaml(file = usr),
      error = function(e) {
        logger::log_error("Failed to parse user YAML file: {e$message}")
        stop("Failed to parse user YAML file: ", conditionMessage(e))
      }
    )

    if (!is.list(user_params)) {
      logger::log_warn(
        "User YAML file did not contain valid parameters, using defaults"
      )
    } else {
      # Deep merge: user parameters override defaults
      params <- merge_lists_recursive(params, user_params)
    }
  }

  # Return the final list of parameters
  return(params)
}

#' @title Recursively merge two lists
#' @description Deep merge where user values override defaults
#' @param default Default list
#' @param user User list to override defaults
#' @return Merged list
#' @keywords internal
merge_lists_recursive <- function(default, user) {
  if (!is.list(user)) {
    return(user)
  }

  result <- default

  for (name in names(user)) {
    if (
      name %in%
        names(default) &&
        is.list(default[[name]]) &&
        is.list(user[[name]])
    ) {
      result[[name]] <- merge_lists_recursive(default[[name]], user[[name]])
    } else {
      result[[name]] <- user[[name]]
    }
  }

  return(result)
}
