#' @title Get MassBank spectra
#'
#' @description This function gets MassBank spectra
#'
#' @include get_default_paths.R
#'
#' @param output_dir Output where to store the spectra
#' @param mb_file MassBank file
#' @param mb_url MassBank URL
#' @param mb_version MassBank version
#'
#' @return The path to MassBank spectra
#'
#' @examples NULL
get_massbank_spectra <-
  function(
    output_dir = "data/source/libraries/spectra/exp",
    mb_file = get_default_paths()$urls$massbank$file,
    mb_url = get_default_paths()$urls$massbank$url,
    mb_version = get_default_paths()$urls$massbank$version
  ) {
    logger::log_info("Checking if a previous MassBank version already exists")
    export <- file.path(output_dir, paste(mb_version, mb_file, sep = "_"))
    if (!file.exists(export)) {
      logger::log_info("Downloading MassBank", mb_version)
      get_file(
        url = paste(mb_url, mb_version, mb_file, sep = "/"),
        export = export
      )
    } else {
      logger::log_info(
        "It appears you already have",
        "the most recent MassBank version available!"
      )
    }
    return(export)
  }
