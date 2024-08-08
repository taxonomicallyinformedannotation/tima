import::from(fs, dir_copy, .into = environment())
import::from(fs, dir_create, .into = environment())
import::from(fs, path_home, .into = environment())

#' @title Copy backbone
#'
#' @description This function copies backbone
#'
#' @importFrom fs dir_copy
#' @importFrom fs dir_create
#' @importFrom fs path_home
#'
#' @param cache_dir Cache directory
#' @param package Package
#' @param copy_dir Copy directory
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
copy_backbone <- function(cache_dir = ".tima",
                          package = "tima",
                          copy_dir = "inst") {
  cache <- fs::path_home(cache_dir)
  message("Creating cache at ", cache)
  fs::dir_create(path = cache)
  message("Copying default architecture ...")
  fs::dir_copy(
    path = system.file(package = package),
    new_path = file.path(cache, copy_dir),
    overwrite = TRUE
  )
}
