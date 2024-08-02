import::from(crayon, blue, .into = environment())
import::from(crayon, green, .into = environment())
import::from(targets, matches, .into = environment())
import::from(targets, tar_make, .into = environment())

#' @title Tima Full
#'
#' @description This function runs everything you need.
#'
#' @importFrom crayon blue
#' @importFrom crayon green
#' @importFrom targets matches
#' @importFrom targets tar_make
#'
#' @return Everything you need.
#'
#' @export
#'
#' @examples NULL
tima_full <- function() {
  start <- Sys.time()
  log_debug("This script", green("does everything you ever dreamt of. \n"))
  log_debug("Authors: ", green("AR"), "\n")
  log_debug("Contributors: ", blue("PMA"), "\n")
  setwd(dir = fs::path_home(".tima"))
  tar_make(names = matches("^ann_pre$"))
  end <- Sys.time()
  log_debug("Script finished in", green(format(end - start)))
}
