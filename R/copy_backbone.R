import::from(fs, dir_copy, .into = environment())
import::from(fs, dir_create, .into = environment())
import::from(fs, path_home, .into = environment())

#' @title Copy backbone
#'
#' @description This function copies backbone
#'
#' @export
#'
#' @importFrom fs dir_copy
#' @importFrom fs dir_create
#' @importFrom fs path_home
#'
#' @noRd
#'
#' @param cache_dir Cache directory
#' @param package Package
#' @param copy_dir Copy directory
#'
#' @return NULL
#'
#' @examples NULL
copy_backbone <- function(cache_dir = fs::path_home(".tima"),
                          package = "tima",
                          copy_dir = "inst") {
  message("Creating cache at ", cache_dir)
  fs::dir_create(path = cache_dir)
  message("Copying default architecture ...")
  fs::dir_copy(
    path = system.file(package = package),
    new_path = file.path(cache_dir, copy_dir),
    overwrite = TRUE
  )
}
