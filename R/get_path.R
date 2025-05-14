#' @title Get path
#'
#' @description This function gets path
#'
#' @include get_default_paths.R
#' @include parse_cli_params.R
#' @include parse_yaml_params.R
#'
#' @param base_path Base path
#'
#' @return A path
#'
#' @examples NULL
get_path <- function(base_path) {
  if (file.exists(base_path)) {
    return(base_path)
  } else {
    new_path <- gsub(pattern = "inst", replacement = "", base_path)
    if (file.exists(new_path)) {
      return(new_path)
    } else {
      return(gsub(
        pattern = "inst",
        replacement = system.file(package = "tima"),
        base_path
      ))
    }
  }
}