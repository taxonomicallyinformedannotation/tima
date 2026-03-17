#' @title Get default paths
#'
#' @description This function loads and parses the default paths configuration
#'     from a YAML file. These paths define locations for data files, libraries,
#'     parameters, and other resources used throughout the TIMA workflow.
#'
#' @param yaml Character string path to the YAML file containing path definitions.
#'     Default is the paths.yaml file included in the package installation.
#'
#' @return Named list containing all configured paths and settings. The structure
#'     mirrors the YAML file hierarchy with nested lists for organized access.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Get default paths
#' paths <- get_default_paths()
#'
#' # Access specific paths
#' data_path <- paths$data$source$path
#' }
get_default_paths <- function(
  yaml = system.file("paths.yaml", package = "tima")
) {
  # Validate that YAML file exists
  if (!file.exists(yaml)) {
    cli::cli_abort(
      c(
        "paths configuration file not found",
        "x" = yaml,
        "i" = "ensure the tima package is properly installed"
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Load and parse YAML configuration
  paths_config <- tryCatch(
    {
      yaml::read_yaml(file = yaml)
    },
    error = function(e) {
      log_error("Failed to parse paths YAML: %s", e$message)
      cli::cli_abort(
        c(
          "failed to parse paths YAML file",
          "x" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  # Test override: allow tests to redirect interim params path to a temp directory.
  override_params <- getOption("tima.test.interim_params_dir", default = NULL)
  if (
    !is.null(override_params) &&
      is.character(override_params) &&
      length(override_params) == 1L
  ) {
    if (is.null(paths_config$data)) {
      paths_config$data <- list()
    }
    if (is.null(paths_config$data$interim)) {
      paths_config$data$interim <- list()
    }
    if (is.null(paths_config$data$interim$params)) {
      paths_config$data$interim$params <- list()
    }
    paths_config$data$interim$params$path <- override_params
  }

  return(paths_config)
}
