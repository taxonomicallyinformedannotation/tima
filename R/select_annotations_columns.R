import::from(tidytable, across, .into = environment())
import::from(tidytable, any_of, .into = environment())
import::from(tidytable, mutate, .into = environment())
import::from(tidytable, na_if, .into = environment())
import::from(tidytable, select, .into = environment())
import::from(tidytable, where, .into = environment())

#' @title Select annotations columns
#'
#' @description This function selects annotations columns
#'
#' @export
#'
#' @importFrom tidytable across
#' @importFrom tidytable any_of
#' @importFrom tidytable mutate
#' @importFrom tidytable na_if
#' @importFrom tidytable select
#' @importFrom tidytable where
#'
#' @include columns_model.R
#' @include complement_metadata_structures.R
#' @include round_reals.R
#'
#' @noRd
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
#' @examples NULL
select_annotations_columns <- function(df,
                                       str_stereo = get("str_stereo", envir = parent.frame()),
                                       str_met = get("str_met", envir = parent.frame()),
                                       str_nam = get("str_nam", envir = parent.frame()),
                                       str_tax_cla = get("str_tax_cla", envir = parent.frame()),
                                       str_tax_npc = get("str_tax_npc", envir = parent.frame())) {
  model <- columns_model()
  df <- df |>
    select(any_of(
      c(
        "feature_id",
        model$features_calculated_columns,
        model$candidates_calculated_columns,
        model$candidates_sirius_for_columns,
        model$candidates_sirius_str_columns,
        model$candidates_spectra_columns,
        model$candidates_structures_columns
      )
    )) |>
    mutate(across(
      .cols = where(is.character),
      .fns = function(x) {
        na_if(x, "N/A")
      }
    )) |>
    mutate(across(
      .cols = where(is.character),
      .fns = function(x) {
        na_if(x, "null")
      }
    )) |>
    mutate(across(
      .cols = where(is.character),
      .fns = function(x) {
        na_if(x, "")
      }
    )) |>
    round_reals() |>
    mutate(across(.cols = where(is.numeric), .fns = as.character)) |>
    complement_metadata_structures()
  return(df)
}
