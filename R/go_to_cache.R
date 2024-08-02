import::from(fs, dir_create, .into = environment())
import::from(fs, path_home, .into = environment())


#' @title Go to cache
#'
#' @description This function goes to cache
#'
#' @importFrom fs dir_create
#' @importFrom fs path_home
#'
#' @return Goes to cache
#'
#' @export
#'
#' @examples NULL
go_to_cache <- function(dir = ".tima") {
  cache <- path_home(dir)
  dir_create(cache)
  message("Working in ", cache)
  setwd(dir = cache)
}
