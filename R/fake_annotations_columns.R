#' @title Fake annotations columns
#'
#' @description This function fakes annotations columns
#'
#' @return NULL
#'
#' @examples NULL
fake_annotations_columns <- function() {
  data.frame(
    feature_id = NA,
    candidate_structure_error_mz = NA,
    candidate_structure_error_rt = NA,
    candidate_structure_name = NA,
    candidate_structure_inchikey_connectivity_layer = NA,
    candidate_structure_smiles_no_stereo = NA,
    candidate_structure_molecular_formula = NA,
    candidate_structure_exact_mass = NA,
    candidate_structure_xlogp = NA,
    candidate_adduct = NA,
    candidate_library = NA,
    candidate_score_similarity = NA,
    candidate_count_similarity_peaks_matched = NA,
    candidate_structure_tax_npc_01pat = NA,
    candidate_structure_tax_npc_02sup = NA,
    candidate_structure_tax_npc_03cla = NA,
    candidate_structure_tax_cla_chemontid = NA,
    candidate_structure_tax_cla_01kin = NA,
    candidate_structure_tax_cla_02sup = NA,
    candidate_structure_tax_cla_03cla = NA,
    candidate_structure_tax_cla_04dirpar = NA
  )
}
