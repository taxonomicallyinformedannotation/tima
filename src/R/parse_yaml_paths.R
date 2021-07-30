require(purrr)
require(yaml)

source(file = "src/R/log_debug.R")

#' Title
#'
#' @return
#' @export
#'
#' @examples
parse_yaml_paths <- function() {
  log_debug("Loading paths")
  paths <- yaml::read_yaml(
    file = "paths.yaml",
    handlers = list(
      seq = function(x) {
        purrr::flatten(x)
      }
    )
  )

  setwd(paths$base_dir)

  return(paths)
}
