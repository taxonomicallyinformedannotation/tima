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
    dplyr::filter(
      !is.na(structure_inchikey),
      !is.na(structure_smiles),
      !is.na(organism_name)
    ) |>
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
    dplyr::filter(!is.na(reference_doi) | n == 1) |>
    tidytable::select(-n)
  log_debug(x = "Led to", nrow(table_keys), "referenced structure-organism pairs")

  table_structures_2D_3D <- table |>
    dplyr::filter(
      !is.na(structure_inchikey),
      !is.na(structure_smiles),
      !is.na(structure_inchikey_2D),
      !is.na(structure_smiles_2D)
    ) |>
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
    dplyr::filter(
      !is.na(structure_inchikey),
      !is.na(structure_smiles),
      !is.na(structure_molecular_formula),
      !is.na(structure_exact_mass),
      !is.na(structure_xlogp)
    ) |>
    tidytable::select(
      structure_inchikey,
      structure_smiles,
      structure_molecular_formula,
      structure_exact_mass,
      structure_xlogp
    ) |>
    tidytable::distinct()

  table_structures_names <- table |>
    dplyr::filter(
      !is.na(structure_inchikey),
      !is.na(structure_smiles),
      !is.na(structure_name)
    ) |>
    tidytable::select(
      structure_inchikey,
      structure_smiles,
      structure_name
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(
      structure_inchikey,
      structure_smiles
    ) |>
    dplyr::summarize(dplyr::across(dplyr::everything(), function(x) {
      x <- list(paste(unique(x[!is.na(x)]), collapse = " $ "))
    })) |>
    tidytable::ungroup() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), trimws))

  table_structures_taxonomy_npc <- table |>
    dplyr::filter(!is.na(structure_smiles_2D)) |>
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
    dplyr::summarize(dplyr::across(dplyr::everything(), function(x) {
      x <- list(paste(unique(x[!is.na(x)]), collapse = " $ "))
    })) |>
    tidytable::ungroup() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), trimws)) |>
    dplyr::mutate(dplyr::across(
      dplyr::matches("taxonomy.*_0"),
      ~ tidytable::replace_na(.x, "notClassified")
    ))

  table_structures_taxonomy_classyfire <- table |>
    dplyr::filter(
      !is.na(structure_inchikey_2D) &
        !is.na(structure_taxonomy_classyfire_chemontid)
    ) |>
    tidytable::select(
      structure_inchikey_2D,
      structure_taxonomy_classyfire_chemontid,
      structure_taxonomy_classyfire_01kingdom,
      structure_taxonomy_classyfire_02superclass,
      structure_taxonomy_classyfire_03class,
      structure_taxonomy_classyfire_04directparent
    ) |>
    tidytable::distinct() |>
    dplyr::mutate(dplyr::across(
      dplyr::matches("taxonomy.*_0"),
      ~ tidytable::replace_na(.x, "notClassified")
    ))

  table_organisms_names <- table |>
    dplyr::filter(!is.na(organism_name)) |>
    tidytable::select(organism_name) |>
    tidytable::distinct()

  log_debug(x = "among", nrow(table_organisms_names), "unique organisms")

  table_organisms_taxonomy_ott <- table |>
    dplyr::filter(
      !is.na(organism_name),
      !is.na(organism_taxonomy_ottid)
    ) |>
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
    dplyr::mutate(dplyr::across(
      dplyr::matches("taxonomy.*_0"),
      ~ tidytable::replace_na(.x, "notClassified")
    ))

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
