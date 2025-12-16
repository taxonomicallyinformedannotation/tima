#' @title Round numeric values in dataframe columns
#'
#' @description Rounds numeric values in columns matching specified patterns.
#'     Useful for standardizing precision in mass spectrometry data where
#'     exact mass and logP values need consistent decimal places.
#'
#' @include validations_utils.R
#'
#' @param df Data frame or tibble containing columns to round
#' @param dig Integer number of decimal digits (default: 5). Must be >= 0.
#'     Use 0 to round to whole numbers.
#' @param cols Character vector of column name patterns to match
#'     (default: c("structure_exact_mass", "structure_xlogp"))
#'     Uses fixed string matching for performance.
#'
#' @return Data frame with specified numeric columns rounded to dig decimal places
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   structure_exact_mass = c(123.456789, 234.567890),
#'   structure_xlogp = c(2.345678, 3.456789),
#'   other_col = c(1, 2)
#' )
#'
#' # Round to 3 decimal places
#' round_reals(df, dig = 3)
#'
#' # Round to whole numbers
#' round_reals(df, dig = 0)
#' }
round_reals <- function(
  df,
  dig = 5L,
  cols = c("structure_exact_mass", "structure_xlogp")
) {
  # Input Validation ----
  validate_dataframe(df, param_name = "df")
  validate_list_or_vector(cols, min_length = 0, param_name = "cols")

  # Validate dig is a non-negative integer (0 is valid for whole numbers)
  if (
    !is.numeric(dig) ||
      length(dig) != 1L ||
      is.na(dig) ||
      dig < 0 ||
      (dig %% 1) != 0
  ) {
    stop(
      "dig must be a non-negative integer (>= 0), got: ",
      dig,
      call. = FALSE
    )
  }

  # Early exit for empty patterns
  if (length(cols) == 0L) {
    return(df)
  }

  # Find Matching Columns ----
  matching_cols <- find_matching_columns(names(df), cols)

  # Early exit if no columns found
  if (length(matching_cols) == 0L) {
    return(df)
  }

  # Apply Rounding ----
  df |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::any_of(x = matching_cols),
      .fns = ~ round(as.numeric(.x), digits = dig)
    ))
}

#' Find columns matching any pattern
#' @keywords internal
find_matching_columns <- function(column_names, patterns) {
  # Helper to find matches for a single pattern
  .find_pattern_matches <- function(pattern) {
    grep(pattern, column_names, fixed = TRUE, value = TRUE)
  }

  # Vectorized: create regex pattern from all search patterns
  # This is faster than looping for many columns
  all_matches <- unlist(
    lapply(X = patterns, FUN = .find_pattern_matches),
    use.names = FALSE
  )

  unique(all_matches)
}
