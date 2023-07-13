utils::globalVariables(
  c(
    "paths",
    "structure_exact_mass",
    "structure_exact_mass_i",
    "structure_exact_mass_s",
    "structure_inchikey_2D",
    "structure_inchikey_2D_s",
    "structure_molecular_formula",
    "structure_molecular_formula_i",
    "structure_molecular_formula_s",
    "structure_name",
    "structure_name_i",
    "structure_name_s",
    "structure_smiles_2D",
    "structure_smiles_2D_i",
    "structure_taxonomy_classyfire_01kingdom",
    "structure_taxonomy_classyfire_01kingdom_i",
    "structure_taxonomy_classyfire_02superclass",
    "structure_taxonomy_classyfire_02superclass_i",
    "structure_taxonomy_classyfire_03class",
    "structure_taxonomy_classyfire_03class_i",
    "structure_taxonomy_classyfire_04directparent",
    "structure_taxonomy_classyfire_04directparent_i",
    "structure_taxonomy_classyfire_chemontid",
    "structure_taxonomy_classyfire_chemontid_i",
    "structure_taxonomy_npclassifier_01pathway",
    "structure_taxonomy_npclassifier_01pathway_s",
    "structure_taxonomy_npclassifier_02superclass",
    "structure_taxonomy_npclassifier_02superclass_s",
    "structure_taxonomy_npclassifier_03class",
    "structure_taxonomy_npclassifier_03class_s",
    "structure_xlogp",
    "structure_xlogp_i",
    "structure_xlogp_s"
  )
)

#' @title Complement metadata of structures
#'
#' @description This function complement structural metadata
#'
#' @include clean_collapse.R
#'
#' @param df Data frame with structural metadata to be complemented
#' @param str_2D_3D File containing 2D and 3D structures
#' @param str_met File containing structures metadata
#' @param str_nam File containing structures names
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#'
#' @return Data frame with complemented structural metadata
#'
#' @export
#'
#' @examples NULL
complement_metadata_structures <- function(df,
                                           str_2D_3D = paths$
                                             data$
                                             interim$
                                             libraries$
                                             merged$
                                             structures$
                                             dd_ddd,
                                           str_met = paths$
                                             data$
                                             interim$
                                             libraries$
                                             merged$
                                             structures$
                                             metadata,
                                           str_nam = paths$
                                             data$
                                             interim$
                                             libraries$
                                             merged$
                                             structures$
                                             names,
                                           str_tax_cla = paths$
                                             data$
                                             interim$
                                             libraries$
                                             merged$
                                             structures$
                                             taxonomies$
                                             classyfire,
                                           str_tax_npc = paths$
                                             data$
                                             interim$
                                             libraries$
                                             merged$
                                             structures$
                                             taxonomies$
                                             npc) {
  log_debug("Trying to look for already computed metadata")
  dd_ddd <- tidytable::fread(str_2D_3D,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  dd_ddd_s <- dd_ddd |>
    tidytable::select(
      structure_inchikey_2D_s = structure_inchikey_2D,
      structure_smiles_2D
    ) |>
    tidytable::distinct(structure_smiles_2D, .keep_all = TRUE)

  dd_ddd_i <- dd_ddd |>
    tidytable::select(structure_inchikey_2D,
      structure_smiles_2D_i = structure_smiles_2D
    ) |>
    tidytable::distinct(structure_inchikey_2D, .keep_all = TRUE)

  met_2D <- tidytable::fread(str_met,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::left_join(dd_ddd) |>
    tidytable::distinct(
      structure_inchikey_2D,
      structure_smiles_2D,
      structure_exact_mass,
      structure_xlogp,
      structure_molecular_formula
    ) |>
    ## Avoid small discrepancies
    tidytable::distinct(structure_inchikey_2D,
      .keep_all = TRUE
    ) |>
    tidytable::distinct(structure_smiles_2D,
      .keep_all = TRUE
    )

  nam_2D <- tidytable::fread(str_nam,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::left_join(dd_ddd) |>
    tidytable::distinct(
      structure_inchikey_2D,
      structure_smiles_2D,
      structure_name
    ) |>
    dplyr::group_by(
      structure_inchikey_2D,
      structure_smiles_2D
    ) |>
    clean_collapse() |>
    tidytable::tidytable() |>
    tidyft::mutate_vars(is.character, .func = trimws) |>
    ## Avoid small discrepancies
    tidytable::distinct(structure_inchikey_2D,
      .keep_all = TRUE
    ) |>
    tidytable::distinct(structure_smiles_2D,
      .keep_all = TRUE
    )

  tax_cla <- tidytable::fread(str_tax_cla,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::select(
      structure_inchikey_2D,
      structure_taxonomy_classyfire_chemontid_i = structure_taxonomy_classyfire_chemontid,
      structure_taxonomy_classyfire_01kingdom_i = structure_taxonomy_classyfire_01kingdom,
      structure_taxonomy_classyfire_02superclass_i = structure_taxonomy_classyfire_02superclass,
      structure_taxonomy_classyfire_03class_i = structure_taxonomy_classyfire_03class,
      structure_taxonomy_classyfire_04directparent_i = structure_taxonomy_classyfire_04directparent
    ) |>
    tidytable::distinct(structure_inchikey_2D, .keep_all = TRUE)

  tax_npc <- tidytable::fread(str_tax_npc,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::select(
      structure_smiles_2D,
      structure_taxonomy_npclassifier_01pathway_s = structure_taxonomy_npclassifier_01pathway,
      structure_taxonomy_npclassifier_02superclass_s = structure_taxonomy_npclassifier_02superclass,
      structure_taxonomy_npclassifier_03class_s = structure_taxonomy_npclassifier_03class,
    ) |>
    tidytable::distinct(structure_smiles_2D, .keep_all = TRUE)

  met_i <- met_2D |>
    tidytable::select(
      structure_inchikey_2D,
      structure_molecular_formula_i = structure_molecular_formula,
      structure_exact_mass_i = structure_exact_mass,
      structure_xlogp_i = structure_xlogp
    ) |>
    tidytable::distinct(structure_inchikey_2D, .keep_all = TRUE)

  met_s <- met_2D |>
    tidytable::select(
      structure_smiles_2D,
      structure_molecular_formula_s = structure_molecular_formula,
      structure_exact_mass_s = structure_exact_mass,
      structure_xlogp_s = structure_xlogp
    ) |>
    tidytable::distinct(structure_smiles_2D, .keep_all = TRUE)

  nam_i <- nam_2D |>
    tidytable::select(structure_inchikey_2D,
      structure_name_i = structure_name
    ) |>
    tidytable::distinct(structure_inchikey_2D, .keep_all = TRUE)

  nam_s <- nam_2D |>
    tidytable::select(structure_smiles_2D,
      structure_name_s = structure_name
    ) |>
    tidytable::distinct(structure_smiles_2D, .keep_all = TRUE)

  ## Always returning preferentially internal values (smiles > inchikey > external)
  table_final <- df |>
    tidytable::left_join(dd_ddd_i) |>
    tidytable::left_join(dd_ddd_s) |>
    dplyr::mutate(
      structure_smiles_2D = dplyr::coalesce(structure_smiles_2D_i, structure_smiles_2D),
      structure_inchikey_2D = dplyr::coalesce(structure_inchikey_2D_s, structure_inchikey_2D)
    ) |>
    tidytable::select(-structure_smiles_2D_i, -structure_inchikey_2D_s) |>
    tidytable::left_join(met_i) |>
    tidytable::left_join(met_s) |>
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
    tidytable::select(
      -structure_molecular_formula_s, -structure_molecular_formula_i, -structure_exact_mass_s, -structure_exact_mass_i, -structure_xlogp_s, -structure_xlogp_i
    ) |>
    tidytable::left_join(nam_i) |>
    tidytable::left_join(nam_s) |>
    dplyr::mutate(structure_name = dplyr::coalesce(structure_name_s, structure_name_i, structure_name)) |>
    tidytable::select(-structure_name_s, -structure_name_i) |>
    tidytable::left_join(tax_npc) |>
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
    tidytable::select(
      -structure_taxonomy_npclassifier_01pathway_s,
      -structure_taxonomy_npclassifier_02superclass_s,
      -structure_taxonomy_npclassifier_03class_s
    ) |>
    tidytable::left_join(tax_cla) |>
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
    tidytable::select(
      -structure_taxonomy_classyfire_chemontid_i,
      -structure_taxonomy_classyfire_01kingdom_i,
      -structure_taxonomy_classyfire_02superclass_i,
      -structure_taxonomy_classyfire_03class_i,
      -structure_taxonomy_classyfire_04directparent_i
    ) |>
    tidyft::mutate_vars(is.character, .func = function(x) {
      tidytable::na_if(x, "")
    })

  ## TODO if (quickmode == FALSE){...}

  return(table_final)
}
