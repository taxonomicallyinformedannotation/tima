#' @title Load yaml files
#'
#' @description This function loads YAML parameter files, preferring user-specified
#'     parameters over defaults when available. It combines default/user params
#'     with prepare_params files.
#'
#' @include get_default_paths.R
#'
#' @return A list containing:
#'   \item{yamls_params}{Named list of parsed YAML content}
#'   \item{yaml_files}{Character vector of file paths}
#'   \item{yaml_names}{Character vector of parameter names}
#'
#' @examples NULL
load_yaml_files <- function() {
  logger::log_trace("Loading YAML parameter files")

  paths <- get_default_paths()

  # Get file lists once for efficiency
  default_files <- list.files(
    path = paths$params$default,
    pattern = "\\.yaml$",
    full.names = TRUE
  )

  user_files <- list.files(
    path = paths$params$user,
    pattern = "\\.yaml$",
    full.names = TRUE
  )

  # Use user params if available and at least as complete as defaults
  use_user <- length(user_files) >= length(default_files)

  if (use_user) {
    logger::log_debug("Using user-specified parameters")
    param_files <- user_files
  } else {
    logger::log_debug("Using default parameters")
    param_files <- default_files
  }

  # Combine with prepare_params files
  yaml_files <- c(
    param_files,
    paths$params$prepare_params,
    paths$params$prepare_params_advanced
  )

  # Extract clean parameter names from file paths
  yaml_names <- yaml_files |>
    basename() |>
    gsub(pattern = "\\.yaml$", replacement = "")

  # Load all YAML files
  yamls_parsed <- purrr::map(yaml_files, yaml::read_yaml)
  names(yamls_parsed) <- yaml_names

  logger::log_trace("Loaded ", length(yamls_parsed), " YAML parameter files")

  return(list(
    yamls_params = yamls_parsed,
    yaml_files = yaml_files,
    yaml_names = yaml_names
  ))
}
