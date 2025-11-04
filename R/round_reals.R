#' @title Round reals
#'
#' @description This function rounds numeric values in specified columns
#'     of a dataframe. It only affects columns that contain the specified
#'     patterns in their names.
#'
#' @param df Dataframe containing columns to round
#' @param dig Integer number of decimal digits to round to (default: 5)
#' @param cols Character vector of column name patterns to match
#'     (default: c("structure_exact_mass", "structure_xlogp"))
#'
#' @return The dataframe with specified numeric columns rounded to the
#'     specified number of digits
#'
#' @examples NULL
round_reals <- function(
  df,
  dig = 5L,
  cols = c("structure_exact_mass", "structure_xlogp")
) {
  # Validate inputs
  if (!is.data.frame(df) && !inherits(df, "tbl")) {
    stop("Input 'df' must be a data frame or tibble")
  }

  if (!is.numeric(dig) || dig < 0) {
    stop("Number of digits 'dig' must be a non-negative integer")
  }

  if (length(cols) == 0) {
    logger::log_trace("No column patterns specified, returning unchanged")
    return(df)
  }

  # Check if any matching columns exist
  matching_cols <- names(df)[sapply(cols, function(pattern) {
    any(grepl(pattern, names(df), fixed = TRUE))
  })]

  if (length(matching_cols) == 0) {
    logger::log_trace("No matching columns found for rounding")
    return(df)
  }

  df |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::contains(cols),
      .fns = \(x) round(as.numeric(x), digits = dig)
    ))
}
