#' @title Select and standardize annotation columns
#'
#' @description Selects and standardizes annotation columns by filtering to
#'     relevant fields, cleaning NULL/NA values, rounding numeric values,
#'     and complementing with structure metadata.
#'
#' @include columns_utils.R
#' @include complement_metadata_structures.R
#' @include round_reals.R
#' @include validations_utils.R
#'
#' @param df [data.frame] Data frame containing annotation results with structure and
#'     candidate information
#' @param str_stereo [character] Path to structure stereochemistry file
#' @param str_met [character] Path to structure metadata file
#' @param str_nam [character] Path to structure names file
#' @param str_tax_cla [character] Path to ClassyFire taxonomy file
#' @param str_tax_npc [character] Path to NPClassifier taxonomy file
#'
#' @return Data frame with standardized annotation columns, cleaned values,
#'     and complemented metadata
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' result <- select_annotations_columns(
#'   df = annotations,
#'   str_stereo = "data/str_stereo.tsv",
#'   str_met = "data/str_metadata.tsv",
#'   str_nam = "data/str_names.tsv",
#'   str_tax_cla = "data/str_tax_classyfire.tsv",
#'   str_tax_npc = "data/str_tax_npclassifier.tsv"
#' )
#' }
select_annotations_columns <- function(
  df,
  str_stereo,
  str_met,
  str_nam,
  str_tax_cla,
  str_tax_npc
) {
  # Input Validation ----
  validate_dataframe(df, param_name = "df")

  # Early exit for empty input
  if (nrow(df) == 0L) {
    log_warn("Empty data frame provided")
    return(df)
  }

  # Validate file paths (will be passed to complement_metadata_structures)
  validate_file_existence(list(
    str_stereo = str_stereo,
    str_met = str_met,
    str_nam = str_nam,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  ))

  log_debug("Input: %d rows, %d columns", nrow(df), ncol(df))

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
    )

  character_cols <- names(df)[vapply(df, is.character, logical(1L))]
  if (length(character_cols) > 0L) {
    df <- df |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::all_of(x = character_cols),
        .fns = .normalize_annotation_text
      ))
  }

  df <- df |>
    # Round numeric values
    round_reals() |>
    # Complement with structure metadata
    complement_metadata_structures(
      str_stereo = str_stereo,
      str_met = str_met,
      str_nam = str_nam,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc
    )

  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1L))]
  if (length(numeric_cols) > 0L) {
    df <- df |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::all_of(x = numeric_cols),
        .fns = as.character
      ))
  }

  # log_trace("Output: ", nrow(df), " rows, ", ncol(df), " columns")

  return(df)
}

.normalize_annotation_text <- function(x) {
  vals <- trimws(as.character(x))
  vals[vals %in% c("N/A", "null", "")] <- NA_character_
  vals
}
