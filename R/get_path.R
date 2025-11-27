#' @title Resolve file path
#'
#' @description Resolves file paths by checking multiple locations, handling
#'     the difference between installed packages (where inst/ is removed) and
#'     development environments (where inst/ exists). Tries paths in order
#'     until one is found.
#'
#' @include get_default_paths.R
#' @include parse_cli_params.R
#' @include parse_yaml_params.R
#' @include validators.R
#'
#' @param base_path Base path to resolve. Can include "inst/" which will be
#'     handled appropriately for installed vs development environments.
#'
#' @return Character string of the resolved absolute path that exists
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Resolve a path that may or may not include inst/
#' resolved <- get_path("inst/params/default.yaml")
#' # Returns correct path whether in dev or installed package
#' }
get_path <- function(base_path) {
  # Input Validation ----
  validate_character(base_path, param_name = "base_path", allow_empty = FALSE)

  # Try 1: Check if base path exists as-is
  if (file.exists(base_path)) {
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
    return(path_with_pkg_dir)
  }

  # If none of the paths exist, return the last attempt and let caller handle
  # logger::log_warn(
  #  "Path not found. Tried: {base_path}, {path_without_inst}, {path_with_pkg_dir}"
  # )

  return(path_with_pkg_dir)
}
