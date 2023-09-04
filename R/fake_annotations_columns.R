#' @title Fake annotations columns
#'
#' @description This function fakes annotations columns
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
fake_annotations_columns <- function() {
  data.frame(
    feature_id = NA,
    error_mz = NA,
    error_rt = NA,
    structure_name = NA,
    structure_inchikey_no_stereo = NA,
    structure_smiles_no_stereo = NA,
    structure_molecular_formula = NA,
    structure_exact_mass = NA,
    structure_xlogp = NA,
    library = NA,
    ## TODO library_type = NA,
    score_input = NA,
    # score_sirius_csi = NA,
    # score_sirius_zodiac = NA,
    # score_sirius_sirius = NA,
    count_peaks_matched = NA,
    count_peaks_explained = NA,
    structure_tax_npc_01pat = NA,
    structure_tax_npc_02sup = NA,
    structure_tax_npc_03cla = NA,
    structure_tax_cla_chemontid = NA,
    structure_tax_cla_01kin = NA,
    structure_tax_cla_02sup = NA,
    structure_tax_cla_03cla = NA,
    structure_tax_cla_04dirpar = NA
  )
}
