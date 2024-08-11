import::from(tidytable, select, .into = environment())

#' @title Select SOP columns
#'
#' @description This function selects sop columns
#'
#' @importFrom tidytable select
#'
#' @noRd
#'
#' @param df Dataframe
#'
#' @return NULL
#'
#' @examples NULL
select_sop_columns <- function(df) {
  df <- df |>
    select(
      structure_name,
      structure_inchikey,
      structure_smiles,
      structure_inchikey_no_stereo = structure_inchikey_2D,
      structure_smiles_no_stereo = structure_smiles_2D,
      structure_molecular_formula,
      structure_exact_mass,
      structure_xlogp,
      structure_tax_npc_01pat = structure_taxonomy_npclassifier_01pathway,
      structure_tax_npc_02sup = structure_taxonomy_npclassifier_02superclass,
      structure_tax_npc_03cla = structure_taxonomy_npclassifier_03class,
      structure_tax_cla_chemontid = structure_taxonomy_classyfire_chemontid,
      structure_tax_cla_01kin = structure_taxonomy_classyfire_01kingdom,
      structure_tax_cla_02sup = structure_taxonomy_classyfire_02superclass,
      structure_tax_cla_03cla = structure_taxonomy_classyfire_03class,
      structure_tax_cla_04dirpar = structure_taxonomy_classyfire_04directparent,
      organism_name,
      organism_taxonomy_ottid,
      organism_taxonomy_01domain,
      organism_taxonomy_02kingdom,
      organism_taxonomy_03phylum,
      organism_taxonomy_04class,
      organism_taxonomy_05order,
      organism_taxonomy_06family,
      organism_taxonomy_07tribe,
      organism_taxonomy_08genus,
      organism_taxonomy_09species,
      organism_taxonomy_10varietas,
      reference_doi
    )

  return(df)
}
