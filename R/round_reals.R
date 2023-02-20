#' @title Round reals
#'
#' @param df Dataframe to use
#' @param dig Number of digits
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr across any_of mutate
#'
#' @examples NULL
round_reals <- function(df, dig = 5) {
  df |>
    # Round to 5 digits to avoid small discrepancies
    dplyr::mutate(dplyr::across(
      .cols = dplyr::any_of(c(
        "structure_exact_mass",
        "structure_xlogp"
      )),
      .fns = \(x) round(x,
        digits = dig
      )
    ))
}
