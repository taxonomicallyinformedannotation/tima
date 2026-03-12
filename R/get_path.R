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
#' @include validations_utils.R
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

  # Try 1: path exists as provided
  if (file.exists(base_path)) {
    return(base_path)
  }

  # Try 2: remove a leading inst/ prefix for installed-package layouts
  path_without_inst <- sub(pattern = "^inst/", replacement = "", x = base_path)
  if (file.exists(path_without_inst)) {
    return(path_without_inst)
  }

  # Try 3: resolve from installed package root
  pkg_path <- system.file(path_without_inst, package = "tima")
  if (nzchar(pkg_path) && file.exists(pkg_path)) {
    return(pkg_path)
  }

  # Try 4: fallback to package root + original base_path
  pkg_root <- system.file(package = "tima")
  if (nzchar(pkg_root)) {
    candidate <- file.path(pkg_root, base_path)
    if (file.exists(candidate)) {
      return(candidate)
    }
  }

  # Return best-effort package-relative candidate for caller diagnostics
  if (nzchar(pkg_path)) {
    return(pkg_path)
  }
  path_without_inst
}
