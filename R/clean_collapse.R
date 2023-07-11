#' @title Clean collapse
#'
#' @description This function collapses a grouped dataframe and trims it
#'
#' @param grouped_df Grouped dataframe
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
clean_collapse <- function(grouped_df) {
  clean_collapse_df <- grouped_df |>
    tidytable::reframe(tidytable::across(
      .cols = tidytable::everything(),
      .fns = function(x) {
        x <- list(paste(unique(x[!is.na(x)]), collapse = " $ "))
      }
    )) |>
    tidytable::ungroup() |>
    tidyft::mutate(tidytable::across(tidytable::everything(), trimws)) |>
    tidyft::mutate(tidytable::across(tidytable::everything(), .fns = function(x) {
      tidytable::na_if(x, "")
    }))

  return(clean_collapse_df)
}
