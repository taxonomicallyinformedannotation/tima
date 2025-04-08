#' @title Round reals
#'
#' @description This function rounds reals in columns of a dataframe
#'
#' @param df Dataframe to use
#' @param dig Number of digits
#' @param cols Columns
#'
#' @return NULL
#'
#' @examples NULL
round_reals <- function(
  df,
  dig = 5L,
  cols = c("structure_exact_mass", "structure_xlogp")
) {
  df |>
    tidytable::mutate(tidytable::across(
      .cols = cols |>
        tidyselect::contains(),
      .fns = \(x) round(as.numeric(x), digits = dig)
    ))
}
