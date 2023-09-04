#' @title Select sirius columns
#'
#' @description This function selects sirius columns
#'
#' @param df Dataframe
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
select_sirius_columns <- function(df) {
  df <- df |>
    tidytable::select(tidytable::any_of(c(
      "feature_id",
      "candidate_structure_name" = "name",
      "candidate_structure_smiles_no_stereo" = "smiles",
      "candidate_structure_inchikey_no_stereo" = "InChIkey2D",
      "candidate_structure_molecular_formula" = "molecularFormula",
      "candidate_structure_xlogp" = "xlogp",
      "candidate_score_sirius_confidence" = "ConfidenceScore",
      "candidate_score_sirius_csi" = "CSI:FingerIDScore"
    ))) |>
    tidytable::distinct() |>
    tidytable::mutate(
      candidate_library = "SIRIUS",
      candidate_structure_tax_npc_01pat = NA_character_,
      candidate_structure_tax_npc_02sup = NA_character_,
      candidate_structure_tax_npc_03cla = NA_character_,
      candidate_structure_tax_cla_chemontid = NA_character_,
      candidate_structure_tax_cla_01kin = NA_character_,
      candidate_structure_tax_cla_02sup = NA_character_,
      candidate_structure_tax_cla_03cla = NA_character_,
      candidate_structure_tax_cla_04dirpar = NA_character_
    )
  return(df)
}

#' @title Select sirius columns 2
#'
#' @description This function selects sirius columns (2)
#'
#' @include harmonize_names_sirius.R
#'
#' @param df Dataframe
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
select_sirius_columns_2 <- function(df) {
  df <- df |>
    tidytable::mutate(feature_id = harmonize_names_sirius(id)) |>
    tidytable::mutate(
      candidate_structure_exact_mass = as.numeric(ionMass) -
        as.numeric(`massErrorPrecursor(ppm)`) *
          as.numeric(ionMass) *
          1E-6,
      candidate_structure_error_mz = as.numeric(ionMass) *
        as.numeric(`massErrorPrecursor(ppm)`) *
        1E-6
    ) |>
    tidytable::select(tidytable::any_of(c(
      "feature_id",
      "candidate_structure_molecular_formula" = "molecularFormula",
      "candidate_structure_exact_mass",
      "candidate_structure_error_mz",
      "candidate_score_sirius_zodiac" = "ZodiacScore",
      "candidate_score_sirius_sirius" = "SiriusScore",
      "candidate_score_sirius_tree" = "TreeScore",
      "candidate_score_sirius_isotope" = "IsotopeScore",
      "candidate_score_sirius_intensity" = "explainedIntensity",
      "candidate_count_sirius_peaks_explained" = "numExplainedPeaks"
    )))
  return(df)
}
