#' @title Fake SOP columns
#'
#' @description This function creates an empty structure-organism pair (SOP)
#'     dataframe template with all standard column names and NA values.
#'     Used as a placeholder when actual SOP data is unavailable.
#'
#' @return A single-row data frame with standard SOP columns filled with
#'     NA_character_ values
#'
#' @examples NULL
fake_sop_columns <- function() {
  # Create template with all standard SOP columns as character type
  tidytable::tidytable(
    structure_name = NA_character_,
    structure_inchikey = NA_character_,
    structure_smiles = NA_character_,
    structure_inchikey_connectivity_layer = NA_character_,
    structure_smiles_no_stereo = NA_character_,
    structure_molecular_formula = NA_character_,
    structure_exact_mass = NA_real_,
    structure_xlogp = NA_real_,
    structure_tax_npc_01pat = NA_character_,
    structure_tax_npc_02sup = NA_character_,
    structure_tax_npc_03cla = NA_character_,
    structure_tax_cla_chemontid = NA_character_,
    structure_tax_cla_01kin = NA_character_,
    structure_tax_cla_02sup = NA_character_,
    structure_tax_cla_03cla = NA_character_,
    structure_tax_cla_04dirpar = NA_character_,
    organism_name = NA_character_,
    organism_taxonomy_ottid = NA_character_,
    organism_taxonomy_01domain = NA_character_,
    organism_taxonomy_02kingdom = NA_character_,
    organism_taxonomy_03phylum = NA_character_,
    organism_taxonomy_04class = NA_character_,
    organism_taxonomy_05order = NA_character_,
    organism_taxonomy_06family = NA_character_,
    organism_taxonomy_07tribe = NA_character_,
    organism_taxonomy_08genus = NA_character_,
    organism_taxonomy_09species = NA_character_,
    organism_taxonomy_10varietas = NA_character_,
    reference_doi = NA_character_
  )
}
