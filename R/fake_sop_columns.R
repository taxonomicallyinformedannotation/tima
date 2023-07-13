#' @title Fake SOP columns
#'
#' @description This function fakes sop columns
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
fake_sop_columns <- function() {
  data.frame(
    structure_name = NA,
    structure_inchikey = NA,
    structure_smiles = NA,
    structure_inchikey_2D = NA,
    structure_smiles_2D = NA,
    structure_molecular_formula = NA,
    structure_exact_mass = NA,
    structure_xlogp = NA,
    structure_taxonomy_npclassifier_01pathway = NA,
    structure_taxonomy_npclassifier_02superclass = NA,
    structure_taxonomy_npclassifier_03class = NA,
    structure_taxonomy_classyfire_chemontid = NA,
    structure_taxonomy_classyfire_01kingdom = NA,
    structure_taxonomy_classyfire_02superclass = NA,
    structure_taxonomy_classyfire_03class = NA,
    structure_taxonomy_classyfire_04directparent = NA,
    organism_name = NA,
    organism_taxonomy_ottid = NA,
    organism_taxonomy_01domain = NA,
    organism_taxonomy_02kingdom = NA,
    organism_taxonomy_03phylum = NA,
    organism_taxonomy_04class = NA,
    organism_taxonomy_05order = NA,
    organism_taxonomy_06family = NA,
    organism_taxonomy_07tribe = NA,
    organism_taxonomy_08genus = NA,
    organism_taxonomy_09species = NA,
    organism_taxonomy_10varietas = NA,
    reference_doi = NA
  )
}
