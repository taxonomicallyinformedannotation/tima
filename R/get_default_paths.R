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
#' @export
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
    stop(
      "Paths configuration file not found: ",
      yaml,
      "\nEnsure the tima package is properly installed."
    )
  }

  # Load and parse YAML configuration
  tryCatch(
    {
      paths_config <- yaml::read_yaml(file = yaml)
      logger::log_trace("Loaded paths configuration from: ", yaml)
      return(paths_config)
    },
    error = function(e) {
      stop("Failed to parse paths YAML file: ", conditionMessage(e))
    }
  )
}
