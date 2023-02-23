#' @title Complement structures metadata
#'
#' @description This function complement structural metadata
#'
#' @param df Data frame with structural metadata to be complemented
#' @param str_2D_3D File containing 2D and 3D structures
#' @param str_met File containing structures metadata
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#'
#' @return Data frame with complemented structural metadata
#'
#' @export
#'
#' @importFrom readr read_delim
#' @importFrom dplyr coalesce distinct left_join mutate select
#'
#' @examples NULL
complement_structures_metadata <- function(df,
                                           str_2D_3D = paths$data$interim$libraries$merged$structures$dd_ddd,
                                           str_met = paths$data$interim$libraries$merged$structures$metadata,
                                           str_nam = paths$data$interim$libraries$merged$structures$names,
                                           str_tax_cla = paths$data$interim$libraries$merged$structures$taxonomies$classyfire,
                                           str_tax_npc = paths$data$interim$libraries$merged$structures$taxonomies$npc) {
  log_debug("Trying to look for already computed metadata")
  dd_ddd <- readr::read_delim(str_2D_3D)

  dd_ddd_s <- dd_ddd |>
    dplyr::select(
      structure_inchikey_2D_s = structure_inchikey_2D,
      structure_smiles_2D
    ) |>
    dplyr::distinct(structure_smiles_2D, .keep_all = TRUE)

  dd_ddd_i <- dd_ddd |>
    dplyr::select(structure_inchikey_2D,
      structure_smiles_2D_i = structure_smiles_2D
    ) |>
    dplyr::distinct(structure_inchikey_2D, .keep_all = TRUE)

  met_2D <- readr::read_delim(str_met) |>
    dplyr::left_join(dd_ddd) |>
    dplyr::distinct(
      structure_inchikey_2D,
      structure_smiles_2D,
      structure_exact_mass,
      structure_xlogp,
      structure_molecular_formula
    ) |>
    ## Avoid small discrepancies
    dplyr::distinct(structure_inchikey_2D,
      .keep_all = TRUE
    ) |>
    dplyr::distinct(structure_smiles_2D,
      .keep_all = TRUE
    )

  nam_2D <- readr::read_delim(str_nam) |>
    dplyr::left_join(dd_ddd) |>
    dplyr::distinct(
      structure_inchikey_2D,
      structure_smiles_2D,
      structure_name
    ) |>
    dplyr::group_by(
      structure_inchikey_2D,
      structure_smiles_2D
    ) |>
    dplyr::summarise_all(function(x) {
      x <- list(paste(unique(x[!is.na(x)]), collapse = " $ "))
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate_all(trimws) |>
    ## Avoid small discrepancies
    dplyr::distinct(structure_inchikey_2D,
      .keep_all = TRUE
    ) |>
    dplyr::distinct(structure_smiles_2D,
      .keep_all = TRUE
    )

  tax_cla <- readr::read_delim(str_tax_cla) |>
    dplyr::select(
      structure_inchikey_2D,
      structure_taxonomy_classyfire_chemontid_i = structure_taxonomy_classyfire_chemontid,
      structure_taxonomy_classyfire_01kingdom_i = structure_taxonomy_classyfire_01kingdom,
      structure_taxonomy_classyfire_02superclass_i = structure_taxonomy_classyfire_02superclass,
      structure_taxonomy_classyfire_03class_i = structure_taxonomy_classyfire_03class,
      structure_taxonomy_classyfire_04directparent_i = structure_taxonomy_classyfire_04directparent
    ) |>
    dplyr::distinct(structure_inchikey_2D, .keep_all = TRUE)

  tax_npc <- readr::read_delim(str_tax_npc) |>
    dplyr::select(
      structure_smiles_2D,
      structure_taxonomy_npclassifier_01pathway_s = structure_taxonomy_npclassifier_01pathway,
      structure_taxonomy_npclassifier_02superclass_s = structure_taxonomy_npclassifier_02superclass,
      structure_taxonomy_npclassifier_03class_s = structure_taxonomy_npclassifier_03class,
    ) |>
    dplyr::distinct(structure_smiles_2D, .keep_all = TRUE)

  met_i <- met_2D |>
    dplyr::select(
      structure_inchikey_2D,
      structure_molecular_formula_i = structure_molecular_formula,
      structure_exact_mass_i = structure_exact_mass,
      structure_xlogp_i = structure_xlogp
    ) |>
    dplyr::distinct(structure_inchikey_2D, .keep_all = TRUE)

  met_s <- met_2D |>
    dplyr::select(
      structure_smiles_2D,
      structure_molecular_formula_s = structure_molecular_formula,
      structure_exact_mass_s = structure_exact_mass,
      structure_xlogp_s = structure_xlogp
    ) |>
    dplyr::distinct(structure_smiles_2D, .keep_all = TRUE)

  nam_i <- nam_2D |>
    dplyr::select(structure_inchikey_2D,
      structure_name_i = structure_name
    ) |>
    dplyr::distinct(structure_inchikey_2D, .keep_all = TRUE)

  nam_s <- nam_2D |>
    dplyr::select(structure_smiles_2D,
      structure_name_s = structure_name
    ) |>
    dplyr::distinct(structure_smiles_2D, .keep_all = TRUE)

  ## Always returning preferentially internal values (smiles > inchikey > external)
  table_final <- df |>
    dplyr::left_join(dd_ddd_i) |>
    dplyr::left_join(dd_ddd_s) |>
    dplyr::mutate(
      structure_smiles_2D = dplyr::coalesce(structure_smiles_2D_i, structure_smiles_2D),
      structure_inchikey_2D = dplyr::coalesce(structure_inchikey_2D_s, structure_inchikey_2D)
    ) |>
    dplyr::select(-structure_smiles_2D_i, -structure_inchikey_2D_s) |>
    dplyr::left_join(met_i) |>
    dplyr::left_join(met_s) |>
    dplyr::mutate(
      structure_molecular_formula = dplyr::coalesce(
        structure_molecular_formula_s,
        structure_molecular_formula_i,
        structure_molecular_formula
      ),
      structure_exact_mass = dplyr::coalesce(
        structure_exact_mass_s,
        structure_exact_mass_i,
        structure_exact_mass
      ),
      structure_xlogp = dplyr::coalesce(structure_xlogp_s, structure_xlogp_i, structure_xlogp)
    ) |>
    dplyr::select(
      -structure_molecular_formula_s,
      -structure_molecular_formula_i,
      -structure_exact_mass_s,
      -structure_exact_mass_i,
      -structure_xlogp_s,
      -structure_xlogp_i
    ) |>
    dplyr::left_join(nam_i) |>
    dplyr::left_join(nam_s) |>
    dplyr::mutate(structure_name = dplyr::coalesce(structure_name_s, structure_name_i, structure_name)) |>
    dplyr::select(-structure_name_s, -structure_name_i) |>
    dplyr::left_join(tax_npc) |>
    dplyr::mutate(
      structure_taxonomy_npclassifier_01pathway = dplyr::coalesce(
        structure_taxonomy_npclassifier_01pathway_s,
        structure_taxonomy_npclassifier_01pathway
      ),
      structure_taxonomy_npclassifier_02superclass = dplyr::coalesce(
        structure_taxonomy_npclassifier_02superclass_s,
        structure_taxonomy_npclassifier_02superclass
      ),
      structure_taxonomy_npclassifier_03class = dplyr::coalesce(
        structure_taxonomy_npclassifier_03class_s,
        structure_taxonomy_npclassifier_03class
      ),
    ) |>
    dplyr::select(
      -structure_taxonomy_npclassifier_01pathway_s, -structure_taxonomy_npclassifier_02superclass_s, -structure_taxonomy_npclassifier_03class_s
    ) |>
    dplyr::left_join(tax_cla) |>
    dplyr::mutate(
      structure_taxonomy_classyfire_chemontid = dplyr::coalesce(
        structure_taxonomy_classyfire_chemontid_i,
        structure_taxonomy_classyfire_chemontid
      ),
      structure_taxonomy_classyfire_01kingdom = dplyr::coalesce(
        structure_taxonomy_classyfire_01kingdom_i,
        structure_taxonomy_classyfire_01kingdom
      ),
      structure_taxonomy_classyfire_02superclass = dplyr::coalesce(
        structure_taxonomy_classyfire_02superclass_i,
        structure_taxonomy_classyfire_02superclass
      ),
      structure_taxonomy_classyfire_03class = dplyr::coalesce(
        structure_taxonomy_classyfire_03class_i,
        structure_taxonomy_classyfire_03class
      ),
      structure_taxonomy_classyfire_04directparent = dplyr::coalesce(
        structure_taxonomy_classyfire_04directparent_i,
        structure_taxonomy_classyfire_04directparent
      )
    ) |>
    dplyr::select(
      -structure_taxonomy_classyfire_chemontid_i, -structure_taxonomy_classyfire_01kingdom_i, -structure_taxonomy_classyfire_02superclass_i, -structure_taxonomy_classyfire_03class_i, -structure_taxonomy_classyfire_04directparent_i
    )

  ## TODO if (quickmode == FALSE){...}

  return(table_final)
}
