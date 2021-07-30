require(purrr)
require(yaml)

source(file = "src/R/log_debug.R")

#' Title
#'
#' @return
#' @export
#'
#' @examples
parse_yaml_params <- function() {
  log_debug("Loading yaml parameters")
  params <-
    yaml::read_yaml(file = default_path, handlers = list(
      seq = function(x) {
        purrr::flatten(x)
      }
    ))
  params <-
    yaml::read_yaml(file = params_path, handlers = list(
      seq = function(x) {
        purrr::flatten(x)
      }
    ))
  return(params)
}