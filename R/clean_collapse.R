#' @title Clean collapse
#'
#' @description This function collapses a grouped dataframe and trims it.
#'     It removes NA values, collapses unique values with " $ " separator,
#'     trims whitespace, and converts empty strings to NA.
#'
#' @param grouped_df Grouped dataframe to be collapsed
#' @param cols Character vector of column name(s) to apply collapse to.
#'     If NULL (default), applies to all columns.
#'
#' @return A cleaned and collapsed dataframe with unique values collapsed
#'     per group
#'
#' @examples NULL
clean_collapse <- function(grouped_df, cols = NULL) {
  # Determine columns to process
  cols <- if (is.null(cols)) {
    names(grouped_df)
  } else {
    as.character(cols)
  }

  # Define collapse function once to avoid repeated function creation
  collapse_unique <- function(x) {
    list(paste(unique(x[!is.na(x)]), collapse = " $ "))
  }

  # Pipeline for collapsing and cleaning
  clean_collapse_df <- grouped_df |>
    tidytable::reframe(tidytable::across(
      .cols = tidyselect::all_of(cols),
      .fns = collapse_unique
    )) |>
    tidytable::ungroup() |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.list),
      .fns = as.character
    )) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.character),
      .fns = \(x) tidytable::na_if(trimws(x), "")
    ))

  return(clean_collapse_df)
}
