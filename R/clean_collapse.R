import::from(tidytable, across, .into = environment())
import::from(tidytable, everything, .into = environment())
import::from(tidytable, mutate, .into = environment())
import::from(tidytable, na_if, .into = environment())
import::from(tidytable, reframe, .into = environment())
import::from(tidytable, ungroup, .into = environment())
import::from(tidytable, where, .into = environment())

#' @title Clean collapse
#'
#' @description This function collapses a grouped dataframe and trims it
#'
#' @importFrom tidytable across
#' @importFrom tidytable everything
#' @importFrom tidytable mutate
#' @importFrom tidytable na_if
#' @importFrom tidytable reframe
#' @importFrom tidytable ungroup
#' @importFrom tidytable where
#'
#' @noRd
#'
#' @param grouped_df Grouped dataframe
#' @param cols Column(s) to apply collapse to
#'
#' @return Cleaned and collapsed dataframe
#'
#' @examples NULL
clean_collapse <- function(grouped_df, cols = NA) {
  clean_collapse_df <- grouped_df |>
    reframe(across(
      .cols = ifelse(
        test = is.na(cols),
        yes = everything(),
        no = cols
      ),
      .fns = function(x) {
        x <- list(paste(unique(x[!is.na(x)]), collapse = " $ "))
      }
    )) |>
    ungroup() |>
    mutate(across(.cols = where(is.list), .fns = as.character)) |>
    mutate(across(.cols = where(is.character), .fns = trimws)) |>
    mutate(across(
      .cols = where(is.character),
      .fns = function(x) {
        na_if(x, "")
      }
    ))

  return(clean_collapse_df)
}
