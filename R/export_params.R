#' Title
#'
#' @noRd
#'
#' @param parameters TODO
#' @param directory TODO
#' @param step TODO
#'
#' @return TODO
#' @export
#'
#' @importFrom yaml write_yaml
#'
#' @examples
export_params <- function(parameters = params, directory = paths$data$interim$config$path, step) {
  ifelse(
    test = !dir.exists(dirname(directory)),
    yes = dir.create(dirname(directory)),
    no = paste(dirname(directory), "exists")
  )
  ifelse(
    test = !dir.exists(directory),
    yes = dir.create(directory),
    no = paste(directory, "exists")
  )
  log_debug(
    x = "... path to used parameters is",
    directory
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
