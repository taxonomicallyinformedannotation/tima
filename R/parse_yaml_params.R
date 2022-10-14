#' @title Parse YAML parameters
#'
#' @noRd
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom purrr flatten
#' @importFrom yaml read_yaml
#'
#' @examples TODO
parse_yaml_params <- function() {
  log_debug("Loading yaml parameters")
  suppressWarnings(params <-
    yaml::read_yaml(file = default_path, handlers = list(
      seq = function(x) {
        purrr::flatten(x)
      }
    )))
  if (file.exists(params_path)) {
    suppressWarnings(params <-
      yaml::read_yaml(file = params_path, handlers = list(
        seq = function(x) {
          purrr::flatten(x)
        }
      )))
  }
  return(params)
}
