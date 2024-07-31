import::from(crayon, green, .into = environment())
import::from(pak, pkg_status, .into = environment())
import::from(yaml, write_yaml, .into = environment())

#' @title Export parameters
#'
#' @description This function writes the parameters
#'    to a YAML file in the specified directory.
#'
#' @importFrom crayon green
#' @importFrom pak pkg_status
#' @importFrom yaml write_yaml
#'
#' @include parse_yaml_paths.R
#'
#' @param parameters list of parameters to be exported
#' @param directory directory where the YAML file will be saved
#' @param step step identifier to be included in the YAML file name
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
export_params <-
  function(parameters = get("parameters", envir = parent.frame()),
           directory = parse_yaml_paths()$data$interim$params$path,
           step) {
    # ## Use default system data directory
    # directory <- file.path(
    #   rappdirs::user_data_dir(
    #     appname = appname,
    #     appauthor = appauthor,
    #     version = version
    #   ),
    #   directory
    # )

    ## Create directory if it does not exist
    create_dir(export = directory)

    ## Log the path to the used parameters
    log_debug(x = "... path to used parameters is", green(directory))
    tima_version <- pkg_status("tima")$version[1]

    write_yaml(x = parameters, file = file.path(
      directory,
      paste0(
        format(Sys.time(), "%y%m%d_%H%M%OS"),
        "_",
        "tima",
        tima_version,
        "_",
        step,
        ".yaml"
      )
    ))
  }
