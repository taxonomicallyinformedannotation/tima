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

paths <- parse_yaml_paths()

#' @title Get HMDB
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
get_hmdb <-
  function(url = paths$urls$hmdb$metabolites,
           export = paths$data$source$libraries$hmdb) {
    ## TODO check md5 if possible (see https://twitter.com/Adafede/status/1592543895094788096)
    create_dir(export = export)

    log_debug("Downloading HMDB (might be long)")
    curl::curl_download(url = url, destfile = export)
  }

get_hmdb()

log_debug("Script finished in", crayon::green(format(end - start)))
