#' @title Tima Full
#'
#' @description This function runs everything you need.
#'
#' @include go_to_cache.R
#'
#' @return Everything you need.
#'
#' @export
#'
#' @examples NULL
tima_full <- function() {
  start <- Sys.time()
  log_debug(
    "This script",
    crayon::green("does everything you ever dreamt of. \n")
  )
  log_debug("Authors: ", crayon::green("AR"), "\n")
  log_debug("Contributors: ", crayon::blue("PMA"), "\n")
  go_to_cache()
  targets::tar_make(names = tidyselect::matches("^ann_pre$"))
  end <- Sys.time()
  log_debug("Script finished in", crayon::green(format(end - start)))
}
