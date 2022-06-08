#' Title
#'
#' @noRd
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
pretreat_names_sirius <- function(x) {
  y <- x |>
    gsub(
      pattern = "/.*",
      replacement = ""
    )
  return(y)
}
