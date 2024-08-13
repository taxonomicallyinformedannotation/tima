#' @title Get default paths
#'
#' @description This function gets default paths
#'
#' @param yaml The YAML file containing the paths
#'   (default is "extdata/paths.yaml")
#'
#' @return A list containing the paths specified in the YAML file
#'
#' @export
#'
#' @examples get_default_paths()
get_default_paths <- function(yaml = system.file("extdata", "paths.yaml", package = "tima")) {
  ## Read the YAML file containing the paths
  paths <- yaml::read_yaml(file = yaml)

  ## Set the working directory to the base directory specified in the YAML file
  setwd(paths$base_dir)

  ## Return the list of paths
  return(paths)
}
