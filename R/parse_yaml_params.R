if (!require(purrr)) {
  install.packages("purrr")
  require(package = "purrr", quietly = TRUE)
}
if (!require(yaml)) {
  install.packages("yaml")
  require(package = "yaml", quietly = TRUE)
}

#' Title
#'
#' @return TODO
#' @export
#'
#' @examples
parse_yaml_params <- function() {
  log_debug("Loading yaml parameters")
  suppressWarnings(params <-
    yaml::read_yaml(file = default_path, handlers = list(
      seq = function(x) {
        purrr::flatten(x)
      }
    )))
  suppressWarnings(params <-
    yaml::read_yaml(file = params_path, handlers = list(
      seq = function(x) {
        purrr::flatten(x)
      }
    )))
  return(params)
}
