#' @title Parse YAML parameter files
#'
#' @description Parses YAML parameter files, loading default parameters and
#'     optionally overriding them with user-specified values.
#'
#' @include validations_utils.R
#'
#' @param def Path to the default YAML parameters file
#' @param usr Path to the user-specified YAML parameters file (optional).
#'     If it exists, it will override default values.
#'
#' @return List containing the parameters specified in the YAML files
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Load parameters
#' params <- parse_yaml_params(
#'   def = "params/default_params.yaml",
#'   usr = "params/user_params.yaml"
#' )
#' }
parse_yaml_params <- function(def, usr = NULL) {
  # Input Validation ----
  validate_character(def, param_name = "def", allow_empty = FALSE)

  if (!is.null(usr)) {
    validate_character(usr, param_name = "usr", allow_empty = TRUE)
  }

  # Validate default file exists
  if (!file.exists(def)) {
    stop("Default YAML file not found: ", def, call. = FALSE)
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
      log_debug("Loading user-specified parameters from: %s", usr)
    }
    user_params <- tryCatch(
      yaml::read_yaml(file = usr),
      error = function(e) {
        log_error("Failed to parse user YAML file: %s", e$message)
        stop("Failed to parse user YAML file: ", conditionMessage(e))
      }
    )

    if (!is.list(user_params)) {
      log_warn(
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

#' Merge a single user list item into the result
#' @keywords internal
#' @noRd
.merge_list_item <- function(value, name, result_env, default) {
  if (name %in% names(default) && is.list(default[[name]]) && is.list(value)) {
    result_env$result[[name]] <- merge_lists_recursive(default[[name]], value)
  } else {
    result_env$result[[name]] <- value
  }
  invisible(NULL)
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

  # Create environment to hold result for modification
  result_env <- new.env(parent = emptyenv())
  result_env$result <- default

  # Iterate over user list items, merging nested lists recursively
  purrr::imap(
    user,
    .merge_list_item,
    result_env = result_env,
    default = default
  )

  return(result_env$result)
}
