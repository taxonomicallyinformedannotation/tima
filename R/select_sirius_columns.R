#' @title Select sirius columns
#'
#' @description This function selects sirius columns
#'
#' @param df Dataframe
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
select_sirius_columns <- function(df) {
  df |>
    tidytable::select(
      feature_id,
      structure_name = name,
      structure_smiles_no_stereo = smiles,
      structure_inchikey_no_stereo = InChIkey2D,
      structure_molecular_formula = molecularFormula,
      structure_xlogp = xlogp,
      score_input = ConfidenceScore,
      score_sirius_csi = `CSI:FingerIDScore`
    ) |>
    tidytable::mutate(
      library = "SIRIUS",
      ## TODO library_type = "TODO",
      inchikey = NA_character_,
      smiles = NA_character_
    )
}

#' @title Select sirius columns 2
#'
#' @description This function selects sirius columns (2)
#'
#' @include harmonize_names_sirius.R
#'
#' @param df Dataframe
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
select_sirius_columns_2 <- function(df) {
  df |>
    tidytable::mutate(feature_id = harmonize_names_sirius(id)) |>
    tidytable::mutate(
      structure_exact_mass = as.numeric(ionMass) -
        as.numeric(`massErrorPrecursor(ppm)`) *
          as.numeric(ionMass) *
          1E-6,
      error_mz = as.numeric(ionMass) *
        as.numeric(`massErrorPrecursor(ppm)`) *
        1E-6
    ) |>
    tidytable::distinct(
      feature_id,
      structure_molecular_formula = molecularFormula,
      structure_exact_mass,
      error_mz,
      score_sirius_zodiac = ZodiacScore,
      score_sirius_sirius = SiriusScore,
      score_sirius_tree = TreeScore,
      score_sirius_isotope = IsotopeScore,
      count_peaks_explained = numExplainedPeaks,
      score_sirius_intensity = explainedIntensity
    )
}
