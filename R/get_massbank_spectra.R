#' @title Get MassBank spectra
#'
#' @description This function gets MassBank spectra
#'
#' @include get_default_paths.R
#'
#' @noRd
#'
#' @param output_dir Output where to store the spectra
#' @param mb_file MassBank file
#' @param mb_url MassBank URL
#' @param mb_version MassBank version
#'
#' @return NULL
#'
#' @examples NULL
get_massbank_spectra <-
  function(output_dir = "data/source/libraries/spectra/exp",
           mb_file = get_default_paths()$urls$massbank$file,
           mb_url = get_default_paths()$urls$massbank$url,
           mb_version = get_default_paths()$urls$massbank$version) {
    log_debug("Checking if a previous MassBank version already exists")
    export <- file.path(output_dir, paste(mb_version, mb_file, sep = "_"))
    if (!file.exists(export)) {
      log_debug("Downloading MassBank", mb_version)
      get_file(
        url = paste(mb_url, mb_version, mb_file, sep = "/"),
        export = export
      )
    } else {
      log_debug(
        "It appears you already have",
        "the most recent MassBank version available!"
      )
    }
    return(export)
  }
