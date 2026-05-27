#' @title Load YAML parameter files
#'
#' @description Loads YAML parameter files, preferring user-specified parameters
#'     over defaults when available. Combines default/user params with
#'     prepare_params files.
#'
#' @include get_default_paths.R
#' @include get_path.R
#'
#' @return List containing:
#'   \item{yamls_params}{Named list of parsed YAML content}
#'   \item{yaml_files}{Character vector of file paths}
#'   \item{yaml_names}{Character vector of parameter names}
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Load all YAML parameter files
#' params <- load_yaml_files()
#'
#' # Access parsed parameters
#' param_values <- params$yamls_params
#' param_names <- params$yaml_names
#' }
load_yaml_files <- function() {
  paths <- get_default_paths()
  load_yaml_files_from_paths(paths = paths)
}

#' @keywords internal
load_yaml_files_from_paths <- function(
  paths,
  path_resolver = get_path,
  list_files_fn = list.files,
  read_yaml_fn = yaml::read_yaml
) {
  # Get Paths and List Files ----

  # Resolve paths using get_path() to handle inst/ prefix properly
  default_path <- path_resolver(paths$params$default$path)
  user_path <- path_resolver(paths$params$user$path)

  # Get file lists once for efficiency
  default_files <- list_files_fn(
    path = default_path,
    pattern = "\\.yaml$",
    full.names = TRUE
  )

  user_files <- list_files_fn(
    path = user_path,
    pattern = "\\.yaml$",
    full.names = TRUE
  )

  # Determine Which Parameter Set to Use ----

  # Use user params if available and at least as complete as defaults
  use_user <- length(user_files) >= length(default_files)

  param_files <- if (use_user) {
    # log_debug("Using %d user-specified parameters", length(user_files))
    user_files
  } else {
    # log_debug("Using %d default parameters", length(default_files))
    default_files
  }

  # Combine with Prepare Params Files ----

  # Combine with prepare_params files (also need to resolve these paths)
  yaml_files <- c(
    param_files,
    path_resolver(paths$params$prepare_params),
    path_resolver(paths$params$prepare_params_advanced)
  )

  # Extract clean parameter names from file paths
  yaml_names <- yaml_files |>
    basename() |>
    stringi::stri_replace_all_regex(
      pattern = "\\.yaml$",
      replacement = "",
      vectorize_all = FALSE
    )

  # Load All YAML Files ----

  # Load all YAML files with error handling
  yamls_parsed <- tryCatch(
    {
      purrr::map(.x = yaml_files, .f = read_yaml_fn)
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "failed to parse YAML files",
          "x" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  names(yamls_parsed) <- yaml_names

  list(
    yamls_params = yamls_parsed,
    yaml_files = yaml_files,
    yaml_names = yaml_names
  )
}
