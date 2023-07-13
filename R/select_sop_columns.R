#' @title Select SOP columns
#'
#' @description This function selects sop columns
#'
#' @param df Dataframe
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
select_sop_columns <- function(df) {
  df |>
    tidytable::select(
      structure_name,
      structure_inchikey,
      structure_smiles,
      structure_inchikey_2D,
      structure_smiles_2D,
      structure_molecular_formula,
      structure_exact_mass,
      structure_xlogp,
      structure_taxonomy_npclassifier_01pathway,
      structure_taxonomy_npclassifier_02superclass,
      structure_taxonomy_npclassifier_03class,
      structure_taxonomy_classyfire_chemontid,
      structure_taxonomy_classyfire_01kingdom,
      structure_taxonomy_classyfire_02superclass,
      structure_taxonomy_classyfire_03class,
      structure_taxonomy_classyfire_04directparent,
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
