#' Title
#'
#' @noRd
#'
#' @return TODO
#' @export
#'
#' @importFrom yaml read_yaml
#'
#' @examples
parse_yaml_params <- function() {
  log_debug("Loading yaml parameters")
  suppressWarnings(params <-
    yaml::read_yaml(file = default_path, handlers = list(
      seq = function(x) {
        flatten(x)
      }
    )))
  suppressWarnings(params <-
    yaml::read_yaml(file = params_path, handlers = list(
      seq = function(x) {
        flatten(x)
      }
    )))
  return(params)
}
