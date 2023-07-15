#' @title Select annotations columns
#'
#' @description This function selects annotations columns
#'
#' @include complement_metadata_structures.R
#' @include round_reals.R
#'
#' @param df Dataframe
#' @param str_2d_3d File containing 2D and 3D structures
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
                                       str_2d_3d = paths$
                                         data$
                                         interim$
                                         libraries$
                                         merged$
                                         structures$
                                         dd_ddd,
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
      error_rt,
      structure_name,
      # structure_inchikey,
      structure_inchikey_2D,
      # structure_smiles,
      structure_smiles_2D,
      structure_molecular_formula,
      structure_exact_mass,
      structure_xlogp,
      library,
      score_input,
      # score_input_normalized,
      # score_sirius_csi,
      # score_sirius_zodiac,
      # score_sirius_sirius,
      count_peaks_matched,
      count_peaks_explained,
      structure_taxonomy_npclassifier_01pathway,
      structure_taxonomy_npclassifier_02superclass,
      structure_taxonomy_npclassifier_03class,
      ## TODO until better
      structure_taxonomy_classyfire_chemontid,
      structure_taxonomy_classyfire_01kingdom,
      structure_taxonomy_classyfire_02superclass,
      structure_taxonomy_classyfire_03class,
      structure_taxonomy_classyfire_04directparent
    ) |>
    tidyft::mutate_vars(
      is.character,
      .func = function(x) {
        tidytable::na_if(x, "N/A")
      }
    ) |>
    tidyft::mutate_vars(
      is.character,
      .func = function(x) {
        tidytable::na_if(x, "null")
      }
    ) |>
    tidyft::mutate_vars(
      is.character,
      .func = function(x) {
        tidytable::na_if(x, "")
      }
    ) |>
    round_reals() |>
    tidyft::mutate_vars(is.numeric, .func = as.character) |>
    complement_metadata_structures(
      str_2d_3d = str_2d_3d,
      str_met = str_met,
      str_nam = str_nam,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc
    )
}
