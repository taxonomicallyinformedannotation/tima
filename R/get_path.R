#' @title Get path
#'
#' @description This function resolves file paths by checking multiple locations,
#'     handling the difference between installed packages (where inst/ is removed)
#'     and development environments (where inst/ exists). It tries paths in order
#'     until one is found.
#'
#' @include get_default_paths.R
#' @include parse_cli_params.R
#' @include parse_yaml_params.R
#'
#' @param base_path Character string representing the base path to resolve.
#'     Can include "inst/" which will be handled appropriately.
#'
#' @return Character string of the resolved absolute path that exists
#'
#' @examples NULL
get_path <- function(base_path) {
  # Validate input
  if (
    is.null(base_path) ||
      !is.character(base_path) ||
      length(base_path) != 1L ||
      nchar(base_path) == 0L
  ) {
    stop("Base path must be a non-empty character string")
  }

  # Try 1: Check if base path exists as-is
  if (file.exists(base_path)) {
    logger::log_trace("Path found as-is: ", base_path)
    return(base_path)
  }

  # Try 2: Remove "inst" prefix (for installed packages)
  # In installed packages, files from inst/ are moved to package root
  path_without_inst <- gsub(
    pattern = "inst",
    replacement = "",
    x = base_path,
    fixed = TRUE
  )

  if (file.exists(path_without_inst)) {
    logger::log_trace("Path found without 'inst': ", path_without_inst)
    return(path_without_inst)
  }

  # Try 3: Replace "inst" with package installation directory
  # This handles cases where files are in the installed package location
  path_with_pkg_dir <- gsub(
    pattern = "inst",
    replacement = system.file(package = "tima"),
    x = base_path,
    fixed = TRUE
  )

  if (file.exists(path_with_pkg_dir)) {
    logger::log_trace("Path found in package dir: ", path_with_pkg_dir)
    return(path_with_pkg_dir)
  }

  # If none of the paths exist, return the last attempt and let caller handle
  logger::log_warn(
    "Path not found. Tried: ",
    base_path,
    ", ",
    path_without_inst,
    ", ",
    path_with_pkg_dir
  )

  return(path_with_pkg_dir)
}
