import::from(tidytable, across, .into = environment())
import::from(tidytable, contains, .into = environment())
import::from(tidytable, mutate, .into = environment())

#' @title Round reals
#'
#' @importFrom tidytable across
#' @importFrom tidytable contains
#' @importFrom tidytable mutate
#'
#' @noRd
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
    mutate(across(
      .cols = c(
        contains("structure_exact_mass"),
        contains("structure_xlogp")
      ),
      .fns = \(x) round(as.numeric(x), digits = dig)
    ))
}
