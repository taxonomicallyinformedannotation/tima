#' @title Export parameters to YAML file
#'
#' @description Writes parameter configurations to a timestamped YAML file for
#'     reproducibility and tracking. Each export includes the step identifier
#'     and exact timestamp.
#'
#' @include get_default_paths.R
#' @include validations_utils.R
#'
#' @param parameters Named list of parameters to export
#' @param step Step identifier to include in the YAML filename (required)
#' @param directory Path to the directory where the YAML file will be saved
#'     (default: data/interim/params/)
#'
#' @return NULL (invisibly). Creates YAML file as side effect.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Export parameters for a specific step
#' export_params(
#'   parameters = list(threshold = 0.5, mode = "pos"),
#'   step = "annotation"
#' )
#' }
export_params <- function(
  parameters,
  step,
  directory = get_default_paths()$data$interim$params$path
) {
  # Input Validation ----
  validate_list_or_vector(parameters, param_name = "parameters", min_length = 1)
  validate_character(step, param_name = "step", allow_empty = FALSE)
  validate_character(directory, param_name = "directory", allow_empty = FALSE)

  # Create Directory ----
  create_dir(export = directory)

  # Generate Timestamped Filename ----
  timestamp <- format(Sys.time(), "%y%m%d_%H%M%OS")
  filename <- paste0(timestamp, "_", step, ".yaml")
  filepath <- file.path(directory, filename)

  # Log the export location
  logger::log_info("Exporting parameters to: {filepath}")

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
