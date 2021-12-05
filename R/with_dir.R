#' Title
#'
#' @noRd
#'
#' @param dir TODO
#' @param expr TODO
#'
#' @return TODO
#' @export
#'
#' @examples
with_dir <- function(dir, expr) {
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(dir)
  evalq(expr)
}
