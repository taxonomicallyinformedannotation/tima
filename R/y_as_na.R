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
#' @examples
y_as_na <- function(x, y) {
  if ("factor" %in% class(x)) {
    x <- as.character(x)
  } #' Since ifelse wont work with factors
  ifelse(test = as.character(x) != y,
    yes = x,
    no = NA
  )
}
