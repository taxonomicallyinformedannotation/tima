#' @title Collapse and clean grouped data
#'
#' @description Collapses grouped dataframe by combining unique values per group.
#'     Removes NA values, trims whitespace, and converts empty strings to NA.
#'     Useful for aggregating annotations or metadata.
#'
#' @include validators.R
#'
#' @param grouped_df Grouped data frame to collapse
#' @param cols Character vector of column names to collapse.
#'     If NULL (default), applies to all columns.
#' @param separator Character string separator for collapsed values
#'     (default: " $ ")
#'
#' @return Data frame with unique values collapsed per group
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(tidytable)
#' df <- data.frame(
#'   group = c("A", "A", "B", "B"),
#'   value = c("x", "y", "x", "x")
#' )
#' grouped <- df |> group_by(group)
#' clean_collapse(grouped, cols = "value")
#' }
clean_collapse <- function(grouped_df, cols = NULL, separator = " $ ") {
  # Input Validation ----
  validate_dataframe(grouped_df, param_name = "grouped_df")
  validate_character(
    separator,
    param_name = "separator",
    allow_empty = FALSE
  )

  # Determine Columns to Process ----
  cols <- determine_columns_to_process(grouped_df, cols)

  # Collapse and Clean ----
  collapse_fn <- create_collapse_function(separator)

  grouped_df |>
    tidytable::reframe(tidytable::across(
      .cols = tidyselect::all_of(x = cols),
      .fns = collapse_fn
    )) |>
    tidytable::ungroup() |>
    convert_lists_to_characters() |>
    clean_character_columns()
}

# Helper Functions ----

#' Determine which columns to process
#' @keywords internal
determine_columns_to_process <- function(df, cols) {
  if (is.null(cols)) {
    return(names(df))
  }

  cols <- as.character(cols)

  # Validate columns exist
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0L) {
    stop(
      "Column(s) not found: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  cols
}

#' Create collapse function with separator
#' @keywords internal
create_collapse_function <- function(separator) {
  function(x) {
    list(paste(unique(x[!is.na(x)]), collapse = separator))
  }
}

#' Convert list columns to character
#' @keywords internal
convert_lists_to_characters <- function(df) {
  df |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.list),
      .fns = as.character
    ))
}

#' Clean character columns (trim and convert empty to NA)
#' @keywords internal
clean_character_columns <- function(df) {
  df |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.character),
      .fns = \(x) tidytable::na_if(x = trimws(x), y = "")
    ))
}
