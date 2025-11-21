#' @title Select annotations columns
#'
#' @description This function selects and standardizes annotation columns by
#'     filtering to relevant fields, cleaning NULL/NA values, rounding numeric
#'     values, and complementing with structure metadata. Used to prepare
#'     annotation results for downstream analysis.
#'
#' @include columns_model.R
#' @include complement_metadata_structures.R
#' @include round_reals.R
#'
#' @param df Data frame containing annotation results with structure and
#'     candidate information
#' @param str_stereo Character string path to file containing structure stereochemistry
#' @param str_met Character string path to file containing structure metadata
#' @param str_nam Character string path to file containing structure names
#' @param str_tax_cla Character string path to file containing Classyfire taxonomy
#' @param str_tax_npc Character string path to file containing NPClassifier taxonomy
#'
#' @return Data frame with standardized annotation columns, cleaned values,
#'     and complemented metadata
#'
#' @examples NULL
select_annotations_columns <- function(
  df,
  str_stereo = get("str_stereo", envir = parent.frame()),
  str_met = get("str_met", envir = parent.frame()),
  str_nam = get("str_nam", envir = parent.frame()),
  str_tax_cla = get("str_tax_cla", envir = parent.frame()),
  str_tax_npc = get("str_tax_npc", envir = parent.frame())
) {
  # Validate inputs
  if (!is.data.frame(df) && !inherits(df, "tbl")) {
    stop("df must be a data frame or tibble")
  }

  if (nrow(df) == 0L) {
    logger::log_warn("Empty data frame provided to select_annotations_columns")
    return(df)
  }

  # Validate file paths (these are passed to complement_metadata_structures)
  file_params <- list(
    str_stereo = str_stereo,
    str_met = str_met,
    str_nam = str_nam,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  )

  for (param_name in names(file_params)) {
    param_value <- file_params[[param_name]]
    if (!is.character(param_value) || length(param_value) != 1L) {
      stop(param_name, " must be a single character string")
    }
  }

  # logger::log_trace("Selecting and standardizing annotation columns")
  logger::log_debug("Input: {nrow(df)} rows, {ncol(df)} columns")

  # Get column model
  model <- columns_model()

  # Select relevant columns
  df <- df |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          "feature_id",
          model$features_calculated_columns,
          model$candidates_calculated_columns,
          model$candidates_sirius_for_columns,
          model$candidates_sirius_str_columns,
          model$candidates_spectra_columns,
          model$candidates_structures_columns
        )
      )
    ) |>
    # Clean various NULL/NA representations
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.character),
      .fns = ~ tidytable::na_if(x = .x, y = "N/A")
    )) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.character),
      .fns = ~ tidytable::na_if(x = .x, y = "null")
    )) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.character),
      .fns = ~ tidytable::na_if(x = .x, y = "")
    )) |>
    # Round numeric values
    round_reals() |>
    # Convert all numeric to character for consistency
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.numeric),
      .fns = as.character
    )) |>
    # Complement with structure metadata
    complement_metadata_structures()

  # logger::log_trace("Output: ", nrow(df), " rows, ", ncol(df), " columns")

  return(df)
}
