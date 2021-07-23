require(purrr)
require(yaml)

#' Title
#'
#' @return
#' @export
#'
#' @examples
parse_yaml_params <- function() {
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