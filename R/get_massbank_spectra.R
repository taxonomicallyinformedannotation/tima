#' @title Get MassBank spectra
#'
#' @description This function gets MassBank spectra
#'
#' @include parse_yaml_paths.R
#'
#' @param output_dir Output where to store the spectra
#' @param mb_file MassBank file
#' @param mb_url MassBank URL
#' @param mb_version MassBank version
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
get_massbank_spectra <-
  function(output_dir = "data/source/libraries/spectra/exp",
           mb_file = parse_yaml_paths()$urls$massbank$file,
           mb_url = parse_yaml_paths()$urls$massbank$url,
           mb_version = parse_yaml_paths()$urls$massbank$version) {
    log_debug("Checking if a previous MassBank version already exists")
    export <- file.path(output_dir, paste(mb_version, mb_file, sep = "_"))
    if (!file.exists(export)) {
      log_debug("Downloading MassBank", mb_version)
      get_file(
        url = paste(
          mb_url,
          mb_version,
          mb_file,
          sep = "/"
        ),
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
