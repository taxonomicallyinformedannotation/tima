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
  logger::log_trace(
    "This script does everything you ever dreamt of."
  )
  logger::log_trace("Authors: AR")
  logger::log_trace("Contributors: PMA")
  go_to_cache()
  targets::tar_make(names = tidyselect::matches("^ann_pre$"))
  end <- Sys.time()
  logger::log_success("Script finished in ", format(end - start))
  ## Not using file.rename because of Docker
  logs <- "tima.log"
  success <- file.copy(
    from = "tima.log",
    to = paste0(
      "data/processed/",
      format(end, format = "%Y%m%d_%H%M%S"),
      "_",
      logs
    )
  )
  if (
    success |>
      all()
  ) {
    file.remove(logs)
  }
}
