#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
y_as_na <- function(x, y) {
  if ("factor" %in% class(x)) {
    x <- as.character(x)
  } ## since ifelse wont work with factors
  ifelse(test = as.character(x) != y,
    yes = x,
    no = NA
  )
}
