#' @title Export parameters
#'
#' @noRd
#'
#' @param parameters TODO
#' @param directory TODO
#' @param step TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom crayon green
#' @importFrom yaml write_yaml
#'
#' @examples TODO
export_params <-
  function(parameters = params,
           directory = paths$data$interim$config$path,
           step) {
    create_dir(export = directory)
    log_debug(
      x = "... path to used parameters is",
      crayon::green(directory)
    )

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
