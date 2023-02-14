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
  paths <- parse_yaml_paths()
  # Fetch metadata from Zenodo
  log_debug("Trying to look for already computed metadata")
  get_last_version_from_zenodo(
    doi = paths$url$lotus$metadata_doi,
    pattern = paths$urls$lotus$metadata_pattern$structures,
    path = paths$data$source$libraries$structure_metadata
  )

  # Read metadata from file and keep only distinct rows
  metadata <-
    readr::read_delim(
      file = paths$data$source$libraries$structure_metadata,
      col_select = c(
        "inchikey_2D" = "structureCleaned_inchikey2D",
        "smiles_2D_2" = "structureCleaned_smiles2D",
        "molecular_formula_2" = "structureCleaned_molecularFormula",
        "structure_exact_mass_2" = "structureCleaned_exactMass"
      )
    ) |>
    dplyr::distinct()

  # Join metadata to df, rename columns, and remove unnecessary columns
  df_new <- df |>
    dplyr::left_join(metadata) |>
    dplyr::mutate(
      smiles_2D = dplyr::coalesce(smiles_2D, smiles_2D_2),
      molecular_formula = dplyr::coalesce(molecular_formula, molecular_formula_2),
      structure_exact_mass = dplyr::coalesce(structure_exact_mass, structure_exact_mass_2)
    ) |>
    dplyr::select(
      -smiles_2D_2,
      -molecular_formula_2,
      -structure_exact_mass_2
    )

  return(df_new)
}
