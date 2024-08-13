#' @title Clean collapse
#'
#' @description This function collapses a grouped dataframe and trims it
#'
#' @param grouped_df Grouped dataframe
#' @param cols Column(s) to apply collapse to
#'
#' @return Cleaned and collapsed dataframe
#'
#' @examples NULL
clean_collapse <- function(grouped_df, cols = NA) {
  clean_collapse_df <- grouped_df |>
    tidytable::reframe(tidytable::across(
      .cols = ifelse(
        test = is.na(cols),
        yes = tidyselect::everything(),
        no = cols
      ),
      .fns = function(x) {
        x <- list(paste(unique(x[!is.na(x)]), collapse = " $ "))
      }
    )) |>
    tidytable::ungroup() |>
    tidytable::mutate(tidytable::across(.cols = tidyselect::where(is.list), .fns = as.character)) |>
    tidytable::mutate(tidytable::across(.cols = tidyselect::where(is.character), .fns = trimws)) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.character),
      .fns = function(x) {
        tidytable::na_if(x, "")
      }
    ))

  return(clean_collapse_df)
}
