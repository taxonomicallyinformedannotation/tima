import::from(crayon, green, .into = environment())
import::from(yaml, write_yaml, .into = environment())

#' @title Export parameters
#'
#' @description This function writes the parameters
#'    to a YAML file in the specified directory.
#'
#' @importFrom crayon green
#' @importFrom yaml write_yaml
#'
#' @include get_default_paths.R
#'
#' @param parameters list of parameters to be exported
#' @param directory directory where the YAML file will be saved
#' @param step step identifier to be included in the YAML file name
#'
#' @return NULL
#'
#' @examples NULL
export_params <-
  function(parameters = get("parameters", envir = parent.frame()),
           directory = get_default_paths()$data$interim$params$path,
           step) {
    ## Create directory if it does not exist
    create_dir(export = directory)

    ## Log the path to the used parameters
    log_debug(x = "... path to used parameters is", green(directory))

    write_yaml(x = parameters, file = file.path(
      directory,
      paste0(
        format(Sys.time(), "%y%m%d_%H%M%OS"),
        "_",
        step,
        ".yaml"
      )
    ))
  }
