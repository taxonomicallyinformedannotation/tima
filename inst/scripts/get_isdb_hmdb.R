start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

cat(
  "This script",
  crayon::green("downloads HMDB structures. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

#' @title Get HMDB In Silico
#'
#' @param url TODO
#' @param export TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom curl curl_download write_csv
#'
#' @examples TODO
get_isdb_hmdb <-
  function(url = paths$urls$hmdb$spectra$predicted,
           export = paths$data$source$libraries$hmdb_isdb) {
    paths <- parse_yaml_paths()

    ## TODO check md5 if possible (see https://twitter.com/Adafede/status/1592543895094788096)
    create_dir(export = export)

    log_debug("Downloading HMDB (might be long)")
    curl::curl_download(url = url, destfile = export)
  }

get_isdb_hmdb()

log_debug("Script finished in", crayon::green(format(end - start)))
