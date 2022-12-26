#' @title Export parameters
#'
#' @description This function writes the parameters to a YAML file in the specified directory.
#'
#' @param parameters list of parameters to be exported
#' @param directory directory where the YAML file will be saved
#' @param step step identifier to be included in the YAML file name
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom crayon green
#' @importFrom yaml write_yaml
#'
#' @examples export_params(parameters = list(), directory = ".", step = "step")
export_params <- function(parameters = params, directory = paths$data$interim$config$path, step) {
  # Create directory if it does not exist
  create_dir(export = directory)

  # Log the path to the used parameters
  log_debug(x = "... path to used parameters is", crayon::green(directory))

  # Write parameters to YAML file with current timestamp and step identifier in the file name
  yaml::write_yaml(
    x = parameters,
    file = file.path(
      directory,
      paste0(
        format(Sys.time(), "%y%m%d_%H%M%OS"),
        "_",
        "tima",
        paths$version,
        "_",
        step,
        ".yaml"
      )
    )
  )
}
