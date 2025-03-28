#' @title Copy backbone
#'
#' @description This function copies backbone
#'
#' @param cache_dir Cache directory
#' @param package Package
#'
#' @return NULL
#'
#' @examples NULL
copy_backbone <- function(
  cache_dir = fs::path_home(".tima"),
  package = "tima"
) {
  logger::log_info("Creating cache at ", cache_dir)
  fs::dir_create(path = cache_dir)
  logger::log_info("Copying default architecture")
  fs::dir_copy(
    path = system.file(package = package),
    new_path = cache_dir,
    overwrite = TRUE
  )
}
