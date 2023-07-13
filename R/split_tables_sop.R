utils::globalVariables(
  c(
    "n",
    "organism_name",
    "organism_taxonomy_01domain",
    "organism_taxonomy_02kingdom",
    "organism_taxonomy_03phylum",
    "organism_taxonomy_04class",
    "organism_taxonomy_05order",
    "organism_taxonomy_06family",
    "organism_taxonomy_07tribe",
    "organism_taxonomy_08genus",
    "organism_taxonomy_09species",
    "organism_taxonomy_10varietas",
    "organism_taxonomy_ottid",
    "reference_doi",
    "structure_exact_mass",
    "structure_inchikey",
    "structure_inchikey_2D",
    "structure_molecular_formula",
    "structure_nameTraditional",
    "structure_smiles",
    "structure_smiles_2D",
    "structure_taxonomy_classyfire_01kingdom",
    "structure_taxonomy_classyfire_02superclass",
    "structure_taxonomy_classyfire_03class",
    "structure_taxonomy_classyfire_04directparent",
    "structure_taxonomy_classyfire_chemontid",
    "structure_taxonomy_npclassifier_01pathway",
    "structure_taxonomy_npclassifier_02superclass",
    "structure_taxonomy_npclassifier_03class",
    "structure_xlogp"
  )
)

#' @title Split Structure Organism Pairs table
#'
#' @description This function splits the structure organism table for efficiency.
#'
#' @include clean_collapse.R
#'
#' @param table Table to split
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
split_tables_sop <- function(table) {
  log_debug(x = "Splitting the concatenated library into smaller pieces")

  table_keys <- table |>
    tidyft::filter(!is.na(structure_inchikey)) |>
    tidyft::filter(!is.na(structure_smiles)) |>
    tidyft::filter(!is.na(organism_name)) |>
    tidytable::select(
      structure_inchikey,
      structure_smiles,
      organism_name,
      reference_doi
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey, structure_smiles, organism_name) |>
    tidytable::add_count() |>
    tidytable::ungroup() |>
    tidyft::filter(!is.na(reference_doi) | n == 1) |>
    tidytable::select(-n)
  log_debug(x = "Led to", nrow(table_keys), "referenced structure-organism pairs")

  table_structures_2D_3D <- table |>
    tidyft::filter(!is.na(structure_inchikey)) |>
    tidyft::filter(!is.na(structure_smiles)) |>
    tidyft::filter(!is.na(structure_inchikey_2D)) |>
    tidyft::filter(!is.na(structure_smiles_2D)) |>
    tidytable::select(
      structure_inchikey,
      structure_smiles,
      structure_inchikey_2D,
      structure_smiles_2D
    ) |>
    tidytable::distinct()
  log_debug(x = "Corresponding to", nrow(table_structures_2D_3D), "unique 3D structures...")
  log_debug(
    x = "and",
    nrow(
      table_structures_2D_3D |>
        tidytable::distinct(structure_inchikey_2D)
    ),
    "unique 2D structures"
  )

  table_structures_metadata <- table |>
    tidyft::filter(!is.na(structure_inchikey)) |>
    tidyft::filter(!is.na(structure_smiles)) |>
    tidyft::filter(!is.na(structure_molecular_formula)) |>
    tidyft::filter(!is.na(structure_exact_mass)) |>
    tidyft::filter(!is.na(structure_xlogp)) |>
    tidytable::select(
      structure_inchikey,
      structure_smiles,
      structure_molecular_formula,
      structure_exact_mass,
      structure_xlogp
    ) |>
    tidytable::distinct()

  table_structures_names <- table |>
    tidyft::filter(!is.na(structure_inchikey)) |>
    tidyft::filter(!is.na(structure_smiles)) |>
    tidyft::filter(!is.na(structure_name)) |>
    tidytable::select(
      structure_inchikey,
      structure_smiles,
      structure_name
    ) |>
    tidytable::distinct() |>
    dplyr::group_by(
      structure_inchikey,
      structure_smiles
    ) |>
    clean_collapse()

  table_structures_taxonomy_npc <- table |>
    tidyft::filter(!is.na(structure_smiles_2D)) |>
    dplyr::filter(
      !is.na(structure_taxonomy_npclassifier_01pathway) |
        !is.na(structure_taxonomy_npclassifier_02superclass) |
        !is.na(structure_taxonomy_npclassifier_03class)
    ) |>
    tidytable::select(
      structure_smiles_2D,
      structure_taxonomy_npclassifier_01pathway,
      structure_taxonomy_npclassifier_02superclass,
      structure_taxonomy_npclassifier_03class
    ) |>
    tidytable::distinct() |>
    dplyr::group_by(structure_smiles_2D) |>
    clean_collapse(cols = c(
      "structure_taxonomy_npclassifier_01pathway",
      "structure_taxonomy_npclassifier_02superclass",
      "structure_taxonomy_npclassifier_03class"
    )) |>
    tidyft::mutate_vars(is.character, .func = function(x) {
      tidytable::replace_na(x, "notClassified")
    })

  table_structures_taxonomy_classyfire <- table |>
    tidyft::filter(!is.na(structure_inchikey_2D)) |>
    tidyft::filter(!is.na(structure_taxonomy_classyfire_chemontid)) |>
    tidytable::select(
      structure_inchikey_2D,
      structure_taxonomy_classyfire_chemontid,
      structure_taxonomy_classyfire_01kingdom,
      structure_taxonomy_classyfire_02superclass,
      structure_taxonomy_classyfire_03class,
      structure_taxonomy_classyfire_04directparent
    ) |>
    tidytable::distinct() |>
    tidyft::mutate_vars(is.character, .func = function(x) {
      tidytable::replace_na(x, "notClassified")
    })

  table_organisms_names <- table |>
    tidyft::filter(!is.na(organism_name)) |>
    tidytable::select(organism_name) |>
    tidytable::distinct()

  log_debug(x = "among", nrow(table_organisms_names), "unique organisms")

  table_organisms_taxonomy_ott <- table |>
    tidyft::filter(!is.na(organism_name)) |>
    tidyft::filter(!is.na(organism_taxonomy_ottid)) |>
    tidytable::select(
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
      organism_taxonomy_10varietas
    ) |>
    tidytable::distinct() |>
    tidyft::mutate_vars(is.character, .func = function(x) {
      tidytable::replace_na(x, "notClassified")
    })

  tables <-
    list(
      "key" = table_keys,
      # "org_nam" = table_organisms_names,
      "org_tax_ott" = table_organisms_taxonomy_ott,
      "str_2D_3D" = table_structures_2D_3D,
      "str_met" = table_structures_metadata,
      "str_nam" = table_structures_names,
      "str_tax_cla" = table_structures_taxonomy_classyfire,
      "str_tax_npc" = table_structures_taxonomy_npc
    )

  return(tables)
}
