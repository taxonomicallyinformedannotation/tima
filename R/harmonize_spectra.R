#' @title Harmonize spectra
#'
#' @description This function harmonizes spectra headers
#'
#' @param spectra Spectra object to be harmonized
#' @param mode MS ionization mode. Must contain 'pos' or 'neg'
#' @param co_ce Name of the collision energy in mgf
#' @param co_ci Name of the compound id in mgf
#' @param co_em Name of the exact mass in mgf
#' @param co_in Name of the InChI in mgf
#' @param co_ik Name of the InChIKey in mgf
#' @param co_mf Name of the molecular formula in mgf
#' @param co_na Name of the name in mgf
#' @param co_po Name of the polarity in mgf
#' @param co_sm Name of the SMILES in mgf
#' @param co_si Name of the spectrum id in mgf
#' @param co_sp Name of the SPLASH in mgf
#' @param co_sy Name of the synonyms in mgf
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr any_of filter full_join mutate row_number select
#' @importFrom stringr fixed str_remove
#' @importFrom tidyr pivot_wider
#'
#' @examples NULL
harmonize_spectra <- function(spectra,
                              mode,
                              co_ce,
                              co_ci,
                              co_em,
                              co_in,
                              co_ik,
                              co_mf,
                              co_na,
                              co_po,
                              co_sm,
                              co_si,
                              co_sp,
                              co_sy) {
  columns <- c(
    "collision_energy",
    "compound_id",
    "exactmass",
    "formula",
    "inchi",
    "inchikey",
    "name",
    "smiles",
    "spectrum_id",
    "splash",
    "synonyms"
  )
  columns_full <-
    c(
      "collision_energy" = co_ce,
      "compound_id" = co_ci,
      "exactmass" = co_em,
      "formula" = co_mf,
      "inchi" = co_in,
      "inchikey" = co_ik,
      "name" = co_na,
      "smiles" = co_sm,
      "spectrum_id" = co_si,
      "splash" = co_sp,
      "synonyms" = co_sy
    )
  columns_missing <- columns[!columns %in% names(columns_full)]
  names(columns_missing) <- columns_missing

  spectra_missing <- columns_missing |>
    data.frame() |>
    dplyr::mutate(value = NA_character_) |>
    tidyr::pivot_wider(names_from = columns_missing) |>
    dplyr::mutate(join = "x")

  if (!"rtime" %in% colnames(spectra)) {
    log_debug("no retention time found")
    spectra$rtime <- NA_real_
  }

  spectra_filtered <- spectra |>
    dplyr::filter(grepl(
      pattern = mode,
      x = !!as.name(co_po),
      ignore.case = TRUE
    )) |>
    dplyr::select(
      dplyr::any_of(c(columns_full)),
      precursorCharge,
      precursorMz,
      rtime,
      mz,
      intensity
    ) |>
    dplyr::mutate(join = "x")

  spectra_harmonized <- spectra_filtered |>
    dplyr::full_join(spectra_missing) |>
    dplyr::select(
      collision_energy,
      compound_id,
      exactmass,
      formula,
      inchi,
      inchikey,
      name,
      precursorMz,
      precursorCharge,
      smiles,
      spectrum_id,
      splash,
      synonyms,
      rtime,
      mz,
      intensity
    ) |>
    dplyr::mutate(
      exactmass = as.numeric(exactmass),
      spectrum_id = ifelse(
        test = is.na(spectrum_id),
        yes = dplyr::row_number(),
        no = spectrum_id
      ),
      compound_id = ifelse(
        test = is.na(compound_id),
        yes = name,
        no = compound_id
      )
    )

  return(spectra_harmonized)
}
