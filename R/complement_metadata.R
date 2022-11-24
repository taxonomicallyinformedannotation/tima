#' @title Complement metadata
#'
#' @param df TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom readr read_delim
#' @importFrom dplyr distinct left_join mutate select
#'
#' @examples TODO
complement_metadata <- function(df) {
  log_debug("Trying to look for already computed metadata")
  get_last_version_from_zenodo(
    doi = paths$url$lotus$metadata_doi,
    pattern = paths$urls$lotus$metadata_pattern$structures,
    path = paths$data$source$libraries$structure_metadata
  )
  
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

  df_new <- df |>
    dplyr::left_join(metadata) |>
    dplyr::mutate(
      smiles_2D = ifelse(
        test = !is.na(smiles_2D),
        yes = smiles_2D,
        no = smiles_2D_2
      ),
      molecular_formula = ifelse(
        test = !is.na(molecular_formula),
        yes = molecular_formula,
        no = molecular_formula_2
      ),
      structure_exact_mass = ifelse(
        test = !is.na(structure_exact_mass),
        yes = structure_exact_mass,
        no = structure_exact_mass_2
      )
    ) |>
    dplyr::select(
      -smiles_2D_2,
      -molecular_formula_2,
      -structure_exact_mass_2
    )

  return(df_new)
}
