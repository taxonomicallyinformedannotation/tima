#' @title Select annotations columns
#'
#' @description This function selects annotations columns
#'
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
select_annotations_columns <- function(df,
                                       str_stereo = paths$
                                         data$
                                         interim$
                                         libraries$
                                         merged$
                                         structures$
                                         stereo,
                                       str_met = paths$
                                         data$
                                         interim$
                                         libraries$
                                         merged$
                                         structures$
                                         metadata,
                                       str_nam = paths$
                                         data$
                                         interim$
                                         libraries$
                                         merged$
                                         structures$
                                         names,
                                       str_tax_cla = paths$
                                         data$
                                         interim$
                                         libraries$
                                         merged$
                                         structures$
                                         taxonomies$
                                         classyfire,
                                       str_tax_npc = paths$
                                         data$
                                         interim$
                                         libraries$
                                         merged$
                                         structures$
                                         taxonomies$
                                         npc) {
  df |>
    tidytable::select(
      feature_id,
      error_mz,
      structure_name,
      # structure_inchikey,
      structure_inchikey_no_stereo,
      # structure_smiles,
      structure_smiles_no_stereo,
      structure_molecular_formula,
      structure_exact_mass,
      structure_xlogp,
      library,
      ## TODO library_type,
      score_input,
      # score_input_normalized,
      # score_sirius_csi,
      # score_sirius_zodiac,
      # score_sirius_sirius,
      count_peaks_matched,
      count_peaks_explained,
      structure_tax_npc_01pat,
      structure_tax_npc_02sup,
      structure_tax_npc_03cla,
      ## TODO until better
      structure_tax_cla_chemontid,
      structure_tax_cla_01kin,
      structure_tax_cla_02sup,
      structure_tax_cla_03cla,
      structure_tax_cla_04dirpar
    ) |>
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
    complement_metadata_structures(
      str_stereo = str_stereo,
      str_met = str_met,
      str_nam = str_nam,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc
    )
}
