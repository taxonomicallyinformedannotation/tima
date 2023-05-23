#' @title Round reals
#'
#' @param df Dataframe to use
#' @param dig Number of digits
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
round_reals <- function(df, dig = 5) {
  df_round <- df |>
    # Round to 5 digits to avoid small discrepancies
    tidytable::mutate(tidytable::across(
      tidytable::any_of(
        c(
          "structure_exact_mass",
          "structure_xlogp"
        )
      ),
      as.numeric
    )) |>
    tidytable::mutate(tidytable::across(
      tidytable::any_of(
        c(
          "structure_exact_mass",
          "structure_xlogp"
        )
      ),
      round,
      digits = dig
    ))

  return(df_round)
}
