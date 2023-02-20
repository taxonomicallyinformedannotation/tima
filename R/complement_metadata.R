#' @title Complement metadata
#'
#' @description This function fetches the metadata from Zenodo and joins it with the input data frame.
#'    It also renames and removes unnecessary columns.
#'
#' @param df Data frame with metadata to be complemented
#'
#' @return Data frame with complemented metadata
#'
#' @export
#'
#' @importFrom readr read_delim
#' @importFrom dplyr coalesce distinct left_join mutate select
#'
#' @examples NULL
complement_metadata <- function(df) {
  log_debug("Trying to look for already computed metadata")
  paths <- parse_yaml_paths()
  get_last_version_from_zenodo(
    doi = paths$url$lotus$metadata_doi,
    pattern = paths$urls$lotus$metadata_pattern$structures,
    path = paths$data$source$libraries$structure_metadata
  )

  return(
    df |>
      dplyr::left_join(
        readr::read_delim(
          file = paths$data$source$libraries$structure_metadata,
          col_select = c(
            "inchikey_2D" = "structureCleaned_inchikey2D",
            "smiles_2D_2" = "structureCleaned_smiles2D",
            "structure_name_2" = "structureCleaned_nameTraditional",
            "molecular_formula_2" = "structureCleaned_molecularFormula",
            "structure_exact_mass_2" = "structureCleaned_exactMass",
            "structure_xlogp_2" = "structureCleaned_xlogp"
          )
        ) |>
          dplyr::distinct()
      ) |>
      dplyr::mutate(
        structure_name = dplyr::coalesce(structure_name, structure_name_2),
        smiles_2D = dplyr::coalesce(smiles_2D, smiles_2D_2),
        molecular_formula = dplyr::coalesce(molecular_formula, molecular_formula_2),
        structure_exact_mass = dplyr::coalesce(structure_exact_mass, structure_exact_mass_2),
        structure_xlogp = dplyr::coalesce(structure_xlogp, structure_xlogp_2)
      ) |>
      dplyr::select(
        -structure_name_2,
        -smiles_2D_2,
        -molecular_formula_2,
        -structure_exact_mass_2,
        -structure_xlogp_2
      )
  )
}
