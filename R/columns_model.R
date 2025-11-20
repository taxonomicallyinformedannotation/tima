#' @title Columns model
#'
#' @description This function defines the standardized column naming schema
#'     used throughout the TIMA package. It organizes columns into logical
#'     groups for features, candidates, components, and scoring.
#'
#' @return A named list containing character vectors of column names organized by category:
#'   \item{features_columns}{Basic feature identifiers (ID, m/z, RT)}
#'   \item{features_calculated_columns}{Calculated feature properties (entropy, taxonomy predictions)}
#'   \item{candidates_calculated_columns}{Calculated candidate properties}
#'   \item{candidates_sirius_for_columns}{SIRIUS formula-level annotations}
#'   \item{candidates_sirius_str_columns}{SIRIUS structure-level scores}
#'   \item{candidates_spectra_columns}{Spectral library matching results}
#'   \item{candidates_structures_columns}{Chemical structure metadata and taxonomy}
#'   \item{components_columns}{Molecular network component IDs}
#'   \item{rank_columns}{Candidate ranking columns}
#'   \item{score_columns}{Candidate scoring columns}
#'
#' @examples NULL
columns_model <- function() {
  # Basic feature identifiers ----
  features_columns <- c("feature_id", "feature_mz", "feature_rt")

  # Calculated feature-level properties ----
  features_calculated_columns <- c(
    "feature_spectrum_entropy",
    "feature_spectrum_peaks",
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

  # Calculated candidate-level properties ----
  candidates_calculated_columns <- c("candidate_spectrum_entropy")

  # SIRIUS formula-level columns ----
  candidates_sirius_for_columns <- c(
    "candidate_structure_molecular_formula",
    "candidate_count_sirius_peaks_explained",
    "candidate_score_sirius_intensity",
    "candidate_score_sirius_isotope",
    "candidate_score_sirius_sirius",
    "candidate_score_sirius_tree",
    "candidate_score_sirius_zodiac"
  )

  # SIRIUS structure-level scores ----
  candidates_sirius_str_columns <- c(
    "candidate_score_sirius_confidence",
    "candidate_score_sirius_csi",
    "candidate_score_sirius_msnovelist"
  )

  # Spectral library matching columns ----
  candidates_spectra_columns <- c(
    "candidate_library",
    "candidate_spectrum_id",
    "candidate_adduct",
    "candidate_count_similarity_peaks_matched",
    "candidate_score_similarity"
  )

  # Chemical structure metadata and taxonomy ----
  candidates_structures_columns <- c(
    "candidate_structure_name",
    "candidate_structure_exact_mass",
    "candidate_structure_molecular_formula",
    "candidate_structure_xlogp",
    "candidate_structure_inchikey_connectivity_layer",
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

  # Component identification ----
  components_columns <- c("component_id")

  # Ranking columns ----
  rank_columns <- c("rank_initial", "rank_final")

  # Scoring columns ----
  score_columns <- c(
    "score_initial",
    "score_biological",
    # "score_interim",
    "score_chemical",
    "score_final"
  )

  return(list(
    features_columns = features_columns,
    features_calculated_columns = features_calculated_columns,
    candidates_calculated_columns = candidates_calculated_columns,
    candidates_sirius_for_columns = candidates_sirius_for_columns,
    candidates_sirius_str_columns = candidates_sirius_str_columns,
    candidates_spectra_columns = candidates_spectra_columns,
    candidates_structures_columns = candidates_structures_columns,
    components_columns = components_columns,
    rank_columns = rank_columns,
    score_columns = score_columns
  ))
}
