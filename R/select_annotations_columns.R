#' @title Select annotations columns
#'
#' @description This function selects annotations columns
#'
#' @include columns_model.R
#' @include complement_metadata_structures.R
#' @include round_reals.R
#'
#' @param df Dataframe
#' @param str_stereo File containing structures stereo
#' @param str_met File containing structures metadata
#' @param str_nam File containing structures names
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
select_annotations_columns <- function(
    df,
    str_stereo = get("str_stereo", envir = parent.frame()),
    str_met = get("str_met", envir = parent.frame()),
    str_nam = get("str_nam", envir = parent.frame()),
    str_tax_cla = get("str_tax_cla", envir = parent.frame()),
    str_tax_npc = get("str_tax_npc", envir = parent.frame())) {
  model <- columns_model()
  df <- df |>
    tidytable::select(tidytable::any_of(c(
      "feature_id",
      model$candidates_sirius_formula_columns,
      model$candidates_sirius_structural_columns,
      model$candidates_spectra_columns,
      model$candidates_structures_columns
    ))) |>
    tidytable::mutate(
      tidytable::across(
        .cols = tidytable::where(is.character),
        .fns = function(x) {
          tidytable::na_if(x, "N/A")
        }
      )
    ) |>
    tidytable::mutate(
      tidytable::across(
        .cols = tidytable::where(is.character),
        .fns = function(x) {
          tidytable::na_if(x, "null")
        }
      )
    ) |>
    tidytable::mutate(
      tidytable::across(
        .cols = tidytable::where(is.character),
        .fns = function(x) {
          tidytable::na_if(x, "")
        }
      )
    ) |>
    round_reals() |>
    tidytable::mutate(
      tidytable::across(
        .cols = tidytable::where(is.numeric),
        .fns = as.character
      )
    ) |>
    complement_metadata_structures()
  return(df)
}
