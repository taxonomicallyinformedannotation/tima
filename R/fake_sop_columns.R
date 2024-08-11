#' @title Fake SOP columns
#'
#' @description This function fakes sop columns
#'
#' @export
#'
#' @noRd
#'
#' @return NULL
#'
#' @examples NULL
fake_sop_columns <- function() {
  data.frame(
    structure_name = NA,
    structure_inchikey = NA,
    structure_smiles = NA,
    structure_inchikey_no_stereo = NA,
    structure_smiles_no_stereo = NA,
    structure_molecular_formula = NA,
    structure_exact_mass = NA,
    structure_xlogp = NA,
    structure_tax_npc_01pat = NA,
    structure_tax_npc_02sup = NA,
    structure_tax_npc_03cla = NA,
    structure_tax_cla_chemontid = NA,
    structure_tax_cla_01kin = NA,
    structure_tax_cla_02sup = NA,
    structure_tax_cla_03cla = NA,
    structure_tax_cla_04dirpar = NA,
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
