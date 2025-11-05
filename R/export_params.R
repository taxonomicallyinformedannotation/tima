#' @title Export parameters
#'
#' @description This function writes parameter configurations to a timestamped
#'     YAML file for reproducibility and tracking. Each export includes the
#'     step identifier and exact timestamp.
#'
#' @include get_default_paths.R
#'
#' @param parameters Named list of parameters to be exported
#' @param directory Character string path to the directory where the YAML file
#'     will be saved (default: data/interim/params/)
#' @param step Character string step identifier to be included in the YAML
#'     file name (required)
#'
#' @return NULL (invisibly). Creates YAML file as side effect.
#'
#' @examples NULL
export_params <- function(
  parameters = get("parameters", envir = parent.frame()),
  directory = get_default_paths()$data$interim$params$path,
  step
) {
  # Validate inputs
  if (
    is.null(step) ||
      !is.character(step) ||
      length(step) != 1L ||
      nchar(step) == 0L
  ) {
    stop("Step identifier must be a non-empty character string")
  }

  if (!is.list(parameters)) {
    stop("Parameters must be a list")
  }

  if (
    is.null(directory) ||
      !is.character(directory) ||
      length(directory) != 1L ||
      nchar(directory) == 0L
  ) {
    stop("Output directory must be a non-empty character string")
  }

  # Create directory if it does not exist
  create_dir(export = directory)

  # Generate timestamped filename (format: YYMMDD_HHMMSS_step.yaml)
  timestamp <- format(Sys.time(), "%y%m%d_%H%M%OS")
  filename <- paste0(timestamp, "_", step, ".yaml")
  filepath <- file.path(directory, filename)

  # Log the export location
  logger::log_info("Exporting parameters to: ", filepath)

  # Write parameters to YAML file
  tryCatch(
    {
      yaml::write_yaml(x = parameters, file = filepath)
      logger::log_debug(
        "Successfully exported ",
        length(parameters),
        " parameters"
      )
    },
    error = function(e) {
      stop("Failed to export parameters: ", conditionMessage(e))
    }
  )

  invisible(NULL)
}
