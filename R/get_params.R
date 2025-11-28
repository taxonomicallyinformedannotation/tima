#' @title Get parameters
#'
#' @description This function retrieves and merges parameters for a workflow step,
#'     combining default parameters, user-specified YAML configurations, and
#'     command-line arguments. It handles both regular and advanced parameter sets.
#'
#' @include get_default_paths.R
#' @include get_path.R
#' @include parse_cli_params.R
#' @include parse_yaml_params.R
#'
#' @param step Character string name of the workflow step (e.g., "prepare_params",
#'     "annotate_masses"). Must match an available step in the package.
#'
#' @return Named list containing the merged parameters for the specified step
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' params <- get_params("prepare_params")
#' }
get_params <- function(step) {
  # Input Validation (early check) ----

  if (missing(step) || is.null(step) || !nzchar(step)) {
    stop("Step name must be provided and non-empty")
  }

  if (!is.character(step) || length(step) != 1L) {
    stop("Step must be a single character string, got: ", class(step))
  }

  # Get Default Paths and Available Steps ----

  # Get default paths (cached for potential reuse)
  paths <- get_default_paths()

  # Get list of available steps (once)
  available_steps <- list.files(
    path = system.file("scripts/docopt", package = "tima")
  ) |>
    stringi::stri_replace_all_fixed(
      pattern = ".txt",
      replacement = "",
      vectorize_all = FALSE
    )

  # Normalize step name (remove _advanced suffix for validation)
  step_normalized <- gsub(
    pattern = "_advanced",
    replacement = "",
    x = step,
    fixed = TRUE
  )

  # Validate step exists early (before file I/O)
  if (!step_normalized %in% available_steps) {
    stop(
      "Step '",
      step,
      "' does not exist. Available steps: ",
      paste(utils::head(x = available_steps, 10), collapse = ", "),
      if (length(available_steps) > 10) "..." else ""
    )
  }

  # Determine Parameter File Paths ----

  # Determine default parameter file path based on step type
  default_param_path <- if (step == "prepare_params") {
    file.path(
      system.file(package = "tima"),
      get_path(file.path(paths$params$prepare_params))
    )
  } else if (step == "prepare_params_advanced") {
    file.path(
      system.file(package = "tima"),
      get_path(file.path(paths$params$prepare_params_advanced))
    )
  } else {
    file.path(
      system.file(package = "tima"),
      get_path(file.path(paths$params$default$path, paste0(step, ".yaml")))
    )
  }

  # Validate default parameter file exists
  if (!file.exists(default_param_path)) {
    stop(
      "Default parameter file not found for step '",
      step,
      "': ",
      default_param_path
    )
  }

  # Load Docopt Documentation ----

  # Get docopt documentation path
  docopt_path <- file.path(
    system.file("scripts/docopt", package = "tima"),
    paste0(step_normalized, ".txt")
  )

  # Validate docopt file exists
  if (!file.exists(docopt_path)) {
    stop(
      "Docopt documentation not found for step '",
      step_normalized,
      "': ",
      docopt_path
    )
  }

  # Read docopt documentation with error handling
  docopt_text <- tryCatch(
    readChar(
      con = docopt_path,
      nchars = file.info(docopt_path)$size
    ),
    error = function(e) {
      stop("Failed to read docopt file: ", conditionMessage(e))
    }
  )

  # Load and Merge YAML Parameters ----

  # Check for user-specified parameters
  user_param_path <- file.path(
    get_path(paths$params$user$path),
    paste0(step_normalized, ".yaml")
  )

  # Adjust path for prepare_params steps
  if (step == "prepare_params" || step == "prepare_params_advanced") {
    user_param_path <- gsub(
      user_param_path,
      pattern = "user/",
      replacement = "",
      fixed = TRUE
    )
  }

  # Load and merge YAML parameters
  params <- if (file.exists(user_param_path)) {
    # log_debug("Using user parameters: %s", user_param_path)
    parse_yaml_params(def = default_param_path, usr = user_param_path)
  } else {
    # log_debug("Using default parameters: %s", default_param_path)
    parse_yaml_params(def = default_param_path, usr = default_param_path)
  }

  # Validate that parameters were successfully loaded
  if (!is.list(params) || length(params) == 0L) {
    stop("Failed to load parameters for step '", step, "'")
  }

  # Parse and Merge CLI Arguments ----

  # Parse CLI arguments with error handling
  cli_args <- tryCatch(
    docopt::docopt(doc = docopt_text, version = paths$version),
    error = function(e) {
      log_debug(
        "No CLI arguments provided or parsing failed, using defaults: %s",
        e$message
      )
      list() # Return empty list to use defaults
    }
  )

  # Merge CLI arguments into parameters
  params <- parse_cli_params(arguments = cli_args, parameters = params)

  return(params)
}
