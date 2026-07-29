#' @title Parse YAML parameter files
#'
#' @description Parses YAML parameter files, loading default parameters and
#'     optionally overriding them with user-specified values.
#'
#' @include validations_utils.R
#' @include logs_utils.R
#' @include validations_params.R
#'
#' @param def [character] Path to the default YAML parameters file
#' @param usr [character] Path to the user-specified YAML parameters file
#'     (optional).
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
    cli::cli_abort(
      c(
        "default YAML file not found",
        "x" = def
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Read the default YAML file
  params <- tryCatch(
    yaml::read_yaml(file = def),
    error = function(e) {
      cli::cli_abort(
        c(
          "failed to parse default YAML file",
          "x" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  # Validate that parameters were successfully loaded
  if (!is.list(params) || length(params) == 0L) {
    cli::cli_abort(
      c(
        "default YAML file is empty or invalid",
        "x" = def
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
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
        cli::cli_abort(
          c(
            "failed to parse user YAML file",
            "x" = conditionMessage(e)
          ),
          class = c("tima_runtime_error", "tima_error"),
          call = NULL
        )
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
  params
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
    value <- user[[name]]
    if (
      name %in% names(default) && is.list(default[[name]]) && is.list(value)
    ) {
      result[[name]] <- merge_lists_recursive(default[[name]], value)
    } else {
      result[[name]] <- value
    }
  }

  result
}
