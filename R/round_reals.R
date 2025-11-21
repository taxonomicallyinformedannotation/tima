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
  # Input Validation ----

  # Validate dataframe first
  if (!is.data.frame(df) && !inherits(df, "tbl")) {
    stop("Input 'df' must be a data frame or tibble")
  }

  # Early exit for empty patterns
  if (length(cols) == 0L) {
    # logger::log_trace("No column patterns specified for rounding")
    return(df)
  }

  # Validate digits parameter
  if (!is.numeric(dig) || dig < 0L || dig != as.integer(dig)) {
    stop("Number of digits 'dig' must be a non-negative integer, got: ", dig)
  }

  # Find Matching Columns ----

  # Get column names once
  df_names <- names(df)

  # Vectorized pattern matching (faster than loop for many columns)
  matching_cols <- character(0L)

  for (pattern in cols) {
    matches <- grep(pattern, df_names, fixed = TRUE, value = TRUE)
    matching_cols <- c(matching_cols, matches)
  }

  # Remove duplicates efficiently
  matching_cols <- unique(matching_cols)

  # Early exit if no columns found
  if (length(matching_cols) == 0L) {
    # logger::log_trace(
    #  "No matching columns found for rounding patterns: {paste(cols, collapse = ', ')}"
    # )
    return(df)
  }

  # logger::log_trace(
  # "Rounding {length(matching_cols)} columns to {dig} decimal places: {paste(matching_cols, collapse = ', ')}"
  # )

  # Apply Rounding ----

  # Only process columns that exist
  df |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::any_of(x = matching_cols),
      .fns = ~ round(as.numeric(.x), digits = dig)
    ))
}
