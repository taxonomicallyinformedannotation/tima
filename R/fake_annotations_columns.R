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
    structure_inchikey_2D = NA,
    structure_smiles_2D = NA,
    structure_molecular_formula = NA,
    structure_exact_mass = NA,
    structure_xlogp = NA,
    library = NA,
    score_input = NA,
    # score_sirius_csi = NA,
    # score_sirius_zodiac = NA,
    # score_sirius_sirius = NA,
    count_peaks_matched = NA,
    count_peaks_explained = NA,
    structure_taxonomy_npclassifier_01pathway = NA,
    structure_taxonomy_npclassifier_02superclass = NA,
    structure_taxonomy_npclassifier_03class = NA,
    structure_taxonomy_classyfire_chemontid = NA,
    structure_taxonomy_classyfire_01kingdom = NA,
    structure_taxonomy_classyfire_02superclass = NA,
    structure_taxonomy_classyfire_03class = NA,
    structure_taxonomy_classyfire_04directparent = NA
  )
}
