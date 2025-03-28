#' @title Go to cache
#'
#' @description This function goes to cache
#'
#' @param dir Directory
#'
#' @return Goes to cache
#'
#' @export
#'
#' @examples NULL
go_to_cache <- function(dir = ".tima") {
  cache <- fs::path_home(dir)
  fs::dir_create(cache)
  logger::log_info("Working in ", cache)
  setwd(dir = cache)
}
