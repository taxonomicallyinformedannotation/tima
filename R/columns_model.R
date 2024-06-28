#' @title A simplistic column model
#'
#' @description This function models columns
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
columns_model <- function() {
  features_columns <- c(
    "feature_id",
    "feature_mz",
    "feature_rt"
  )

  features_calculated_columns <- c(
    "feature_spectrum_entropy",
    "feature_pred_tax_cla_01kin_val",
    "feature_pred_tax_cla_01kin_score",
    "feature_pred_tax_cla_02sup_val",
    "feature_pred_tax_cla_02sup_score",
    "feature_pred_tax_cla_03cla_val",
    "feature_pred_tax_cla_03cla_score",
    "feature_pred_tax_cla_04dirpar_val",
    "feature_pred_tax_cla_04dirpar_score",
    "feature_pred_tax_npc_01pat_val",
    "feature_pred_tax_npc_01pat_score",
    "feature_pred_tax_npc_02sup_val",
    "feature_pred_tax_npc_02sup_score",
    "feature_pred_tax_npc_03cla_val",
    "feature_pred_tax_npc_03cla_score"
  )

  candidates_calculated_columns <- c("candidate_spectrum_entropy")

  candidates_sirius_for_columns <- c(
    "candidate_structure_molecular_formula",
    "candidate_count_sirius_peaks_explained",
    "candidate_score_sirius_intensity",
    "candidate_score_sirius_isotope",
    "candidate_score_sirius_sirius",
    "candidate_score_sirius_tree",
    "candidate_score_sirius_zodiac"
  )

  candidates_sirius_str_columns <-
    c(
      "candidate_score_sirius_confidence",
      "candidate_score_sirius_csi",
      "candidate_score_sirius_msnovelist"
    )

  candidates_spectra_columns <- c(
    "candidate_library",
    "candidate_adduct",
    "candidate_count_similarity_peaks_matched",
    "candidate_score_similarity"
  )

  candidates_structures_columns <- c(
    "candidate_structure_name",
    "candidate_structure_exact_mass",
    "candidate_structure_molecular_formula",
    "candidate_structure_xlogp",
    "candidate_structure_inchikey_no_stereo",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_tax_cla_chemontid",
    "candidate_structure_tax_cla_01kin",
    "candidate_structure_tax_cla_02sup",
    "candidate_structure_tax_cla_03cla",
    "candidate_structure_tax_cla_04dirpar",
    "candidate_structure_tax_npc_01pat",
    "candidate_structure_tax_npc_02sup",
    "candidate_structure_tax_npc_03cla",
    # "candidate_structure_tax_cla",
    # "candidate_structure_tax_npc",
    "candidate_structure_organism_occurrence_closest",
    "candidate_structure_organism_occurrence_reference",
    "candidate_structure_error_mz",
    "candidate_structure_error_rt"
  )

  components_columns <- c("component_id")

  rank_columns <- c(
    "rank_initial",
    "rank_final"
  )

  score_columns <- c(
    "score_initial",
    "score_biological",
    # "score_interim",
    "score_chemical",
    "score_final"
  )

  return(
    list(
      "features_columns" = features_columns,
      "features_calculated_columns" = features_calculated_columns,
      "candidates_calculated_columns" = candidates_calculated_columns,
      "candidates_sirius_for_columns" = candidates_sirius_for_columns,
      "candidates_sirius_str_columns" = candidates_sirius_str_columns,
      "candidates_spectra_columns" = candidates_spectra_columns,
      "candidates_structures_columns" = candidates_structures_columns,
      "components_columns" = components_columns,
      "rank_columns" = rank_columns,
      "score_columns" = score_columns
    )
  )
}
