#' @title Fake annotations columns
#'
#' @description This function creates a template data frame with all expected
#'     annotation columns initialized to NA. Used as a fallback when no
#'     annotations are available or to ensure consistent column structure.
#'
#' @return Data frame with one row and columns for all annotation fields,
#'     with all values set to NA. Columns include feature IDs, structure
#'     information, scores, and taxonomic classifications.
#'
#' @examples NULL
fake_annotations_columns <- function() {
  data.frame(
    feature_id = NA_character_,
    candidate_structure_error_mz = NA_real_,
    candidate_structure_error_rt = NA_real_,
    candidate_structure_name = NA_character_,
    candidate_structure_inchikey_connectivity_layer = NA_character_,
    candidate_structure_smiles_no_stereo = NA_character_,
    candidate_structure_molecular_formula = NA_character_,
    candidate_structure_exact_mass = NA_real_,
    candidate_structure_xlogp = NA_real_,
    candidate_adduct = NA_character_,
    candidate_library = NA_character_,
    candidate_score_similarity = NA_real_,
    candidate_count_similarity_peaks_matched = NA_integer_,
    candidate_structure_tax_npc_01pat = NA_character_,
    candidate_structure_tax_npc_02sup = NA_character_,
    candidate_structure_tax_npc_03cla = NA_character_,
    candidate_structure_tax_cla_chemontid = NA_character_,
    candidate_structure_tax_cla_01kin = NA_character_,
    candidate_structure_tax_cla_02sup = NA_character_,
    candidate_structure_tax_cla_03cla = NA_character_,
    candidate_structure_tax_cla_04dirpar = NA_character_,
    stringsAsFactors = FALSE
  )
}
