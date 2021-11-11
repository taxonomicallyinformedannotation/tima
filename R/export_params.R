if (!require(yaml)) {
  install.packages("yaml")
  require(package = "yaml", quietly = TRUE)
}

source(file = "R/log_debug.R")

#' Title
#'
#' @param parameters
#' @param directory
#' @param step
#'
#' @return
#' @export
#'
#' @examples
export_params <- function(parameters, directory, step) {
  ifelse(
    test = !dir.exists(paths$data$interim$path),
    yes = dir.create(paths$data$interim$path),
    no = paste(paths$data$interim$path, "exists")
  )
  ifelse(
    test = !dir.exists(paths$data$interim$config$path),
    yes = dir.create(paths$data$interim$config$path),
    no = paste(paths$data$interim$config$path, "exists")
  )
  log_debug(
    x = "... path to used parameters is",
    paths$data$interim$config$path
  )

  yaml::write_yaml(
    x = parameters,
    file = file.path(
      directory,
      paste0(
        format(Sys.time(), "%y%m%d_%H%M%OS"),
        "_",
        step,
        ".yaml"
      )
    )
  )
}
