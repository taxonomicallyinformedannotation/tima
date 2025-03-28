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
  if (file.exists("tima.log")) {
    file.remove("tima.log")
  }
  logger::log_info(
    "This script does everything you ever dreamt of."
  )
  logger::log_info("Authors: AR")
  logger::log_info("Contributors: PMA")
  go_to_cache()
  targets::tar_make(names = tidyselect::matches("^ann_pre$"))
  end <- Sys.time()
  logger::log_info("Script finished in ", format(end - start))
  file.rename(
    from = "tima.log",
    to = paste0(
      "data/processed/",
      format(end, format = "%Y%m%d_%H%M%S"),
      "_tima.log"
    )
  )
}
