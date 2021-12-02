#' Title
#'
#' @noRd
#'
#' @param x TODO
#' @param y TODO
#'
#' @return TODO
#' @export
#'
#' @examples TODO
y_as_na <- function(x, y) {
  if ("factor" %in% class(x)) {
    x <- as.character(x)
  } ## since ifelse wont work with factors
  ifelse(test = as.character(x) != y,
    yes = x,
    no = NA
  )
}
