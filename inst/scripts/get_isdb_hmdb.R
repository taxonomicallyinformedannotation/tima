start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("downloads HMDB structures. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

#' @title Get HMDB In Silico
#'
#' @param url URL to HMDB in silico predicted spectra
#' @param export Output file
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom curl curl_download write_csv
#'
#' @examples NULL
get_isdb_hmdb <-
  function(url = paths$urls$hmdb$spectra$predicted,
           export = paths$data$source$spectra$hmdb_isdb) {
    paths <- parse_yaml_paths()

    ## TODO check md5 if possible (see https://twitter.com/Adafede/status/1592543895094788096)
    create_dir(export = export)

    log_debug("Downloading HMDB (might be long)")
    curl::curl_download(url = url, destfile = export)
  }

get_isdb_hmdb()

log_debug("Script finished in", crayon::green(format(end - start)))
