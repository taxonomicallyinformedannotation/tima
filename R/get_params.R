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
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' params <- get_params("prepare_params")
#' }
get_params <- function(step) {
  # Validate input
  if (missing(step) || is.null(step) || nchar(step) == 0L) {
    stop("Step name must be provided")
  }

  # Get default paths
  paths <- get_default_paths()

  # Get list of available steps
  available_steps <- list.files(
    path = system.file("scripts/docopt", package = "tima")
  ) |>
    stringi::stri_replace_all_fixed(pattern = ".txt", replacement = "")

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
    stop("Default parameter file not found: ", default_param_path)
  }

  # Normalize step name (remove _advanced suffix for validation)
  step_normalized <- gsub(
    pattern = "_advanced",
    replacement = "",
    x = step,
    fixed = TRUE
  )

  # Validate step exists
  if (!step_normalized %in% available_steps) {
    stop(
      "Step '",
      step,
      "' does not exist. Available steps: ",
      paste(utils::head(available_steps, 10), collapse = ", "),
      if (length(available_steps) > 10) "..." else ""
    )
  }

  # Get docopt documentation path
  docopt_path <- file.path(
    system.file("scripts/docopt", package = "tima"),
    paste0(step_normalized, ".txt")
  )

  # Read docopt documentation
  if (!file.exists(docopt_path)) {
    stop("Docopt file not found: ", docopt_path)
  }

  docopt_text <- readChar(
    con = docopt_path,
    nchars = file.info(docopt_path)$size
  )

  # Check for user-specified parameters
  user_param_path <- file.path(
    get_path(paths$params$user$path),
    paste0(step_normalized, ".yaml")
  )

  # Load and merge YAML parameters
  if (file.exists(user_param_path)) {
    logger::log_debug("Loading user parameters from: {user_param_path}")
    params <- parse_yaml_params(def = default_param_path, usr = user_param_path)
  } else {
    params <- parse_yaml_params(
      def = default_param_path,
      usr = default_param_path
    )
  }

  # Validate that parameters were successfully loaded
  if (!is.list(params) || length(params) == 0L) {
    stop("Failed to load parameters for step: ", step)
  }

  # Parse and merge CLI arguments
  cli_args <- tryCatch(
    docopt::docopt(doc = docopt_text, version = paths$version),
    error = function(e) {
      logger::log_error("Failed to parse CLI arguments: {e$message}")
      list() # Return empty list to use defaults
    }
  )

  params <- parse_cli_params(arguments = cli_args, parameters = params)

  return(params)
}
