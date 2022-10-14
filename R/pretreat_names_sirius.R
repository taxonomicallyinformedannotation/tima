#' @title Pretreat names sirius
#'
#' @noRd
#'
#' @param x
#'
#' @return TODO
#'
#' @export
#'
#' @examples TODO
pretreat_names_sirius <- function(x) {
  y <- x |>
    gsub(
      pattern = "/.*",
      replacement = ""
    )
  return(y)
}
