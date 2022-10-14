#' @title Treat names sirius
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
treat_names_sirius <- function(x) {
  y <- x |>
    gsub(
      pattern = ".*_",
      replacement = ""
    )
  # |> gsub(pattern = "adduct",
  #      replacement = "")
  return(y)
}
