require(purrr)
require(yaml)

#' Title
#'
#' @return
#' @export
#'
#' @examples
parse_yaml_paths <- function() {
  paths <- yaml::read_yaml(file = "paths.yaml",
                           handlers = list(
                             seq = function(x) {
                               purrr::flatten(x)
                             }
                           ))
  
  setwd(paths$base_dir)
  
  return(paths)
}