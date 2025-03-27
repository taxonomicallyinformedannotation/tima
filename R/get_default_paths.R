#' @title Get default paths
#'
#' @description This function gets default paths
#'
#' @param yaml The YAML file containing the paths
#'   (default is "paths.yaml")
#'
#' @return A list containing the paths specified in the YAML file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_default_paths()
#' }
get_default_paths <- function(
  yaml = system.file("paths.yaml", package = "tima")
) {
  return(yaml::read_yaml(file = yaml))
}
