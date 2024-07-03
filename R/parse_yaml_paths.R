#' @title Parse YAML paths
#'
#' @description This function parses YAML paths
#'
#' @param file The file name of the YAML file containing the paths
#'   (default is "inst/paths.yaml")
#'
#' @return A list containing the paths specified in the YAML file
#'
#' @export
#'
#' @examples NULL
parse_yaml_paths <- function(file = system.file("extdata", "paths.yaml", package = "timaR")) {
  return(suppressWarnings(yaml::read_yaml(file = file)))
}
