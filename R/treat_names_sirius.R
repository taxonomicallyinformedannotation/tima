#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
treat_names_sirius <- function(x) {
  y <- x |>
    gsub(
      pattern = ".*_",
      replacement = ""
    )
  # |>gsub(pattern = "adduct",
  #      replacement = "")
  return(y)
}
