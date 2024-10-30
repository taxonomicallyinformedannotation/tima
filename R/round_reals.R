#' @title Round reals
#'
#' @description This function rounds some reals in a dataframe
#'
#' @param df Dataframe to use
#' @param dig Number of digits
#'
#' @return NULL
#'
#' @examples NULL
round_reals <- function(df, dig = 5) {
  df |>
    ## Round to 5 digits to avoid small discrepancies
    ## TODO rename it as for now it gives the feeling all reals in df will be
    tidytable::mutate(tidytable::across(
      .cols = c(
        tidyselect::contains("structure_exact_mass"),
        tidyselect::contains("structure_xlogp")
      ),
      .fns = \(x) round(as.numeric(x), digits = dig)
    ))
}
