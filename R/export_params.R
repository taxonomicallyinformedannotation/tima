require(package = yaml, quietly = TRUE)

source(file = here::here("R", "log_debug.R"))

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
    test = !dir.exists(here::here(paths$data$interim$path)),
    yes = dir.create(here::here(paths$data$interim$path)),
    no = paste(here::here(paths$data$interim$path), "exists")
  )
  ifelse(
    test = !dir.exists(here::here(paths$data$interim$config$path)),
    yes = dir.create(here::here(paths$data$interim$config$path)),
    no = paste(here::here(paths$data$interim$config$path), "exists")
  )
  log_debug(
    x = "... path to used parameters is",
    here::here(paths$data$interim$config$path)
  )

  yaml::write_yaml(
    x = parameters,
    file = here::here(file.path(
      directory,
      paste0(
        format(Sys.time(), "%y%m%d_%H%M%OS"),
        "_",
        step,
        ".yaml"
      )
    ))
  )
}
