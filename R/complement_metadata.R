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
  metadata <-
    readr::read_delim(
      # Suboptimal for now
      file = "https://zenodo.org/record/6786307/files/structure_metadata.tsv.gz",
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
