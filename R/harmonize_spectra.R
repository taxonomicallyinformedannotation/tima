#' @title Harmonize spectra
#'
#' @description TODO
#'
#' @param spectra Spectra object to be harmonized
#' @param colnames Names of the different columns corresponding to standardized headers
#' @param mode MS ionization mode. Must contain 'pos' or 'neg'
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
                              colnames = c(
                                colname_collision_energy = NA,
                                colname_compound_id = NA,
                                colname_exact_mass = "EXACTMASS",
                                colname_formula = "MOLECULAR_FORMULA",
                                colname_inchi = "INCHI",
                                colname_inchikey = "NAME",
                                colname_mode = "IONMODE",
                                colname_name = "NAME",
                                colname_precursorMz = "precursorMz",
                                colname_precursorCharge = "precursorCharge",
                                colname_smiles = "SMILES",
                                colname_spectrum_id = NA,
                                colname_splash = NA,
                                colname_synonyms = NA
                              ),
                              mode = "pos") {
  ## dirty way to get all columns
  colnames_full <- colnames[!is.na(colnames)] |>
    as.character()
  colnames_missing <- colnames[is.na(colnames)] |>
    names() |>
    stringr::str_remove(
      pattern = stringr::fixed(pattern = "colname_")
    )
  colnames[is.na(colnames)] <- colnames[is.na(colnames)] |>
    names() |>
    stringr::str_remove(
      pattern = stringr::fixed(pattern = "colname_")
    )

  spectra_filtered <- spectra |>
    dplyr::filter(grepl(
      pattern = mode,
      x = !!as.name(colnames["colname_mode"]),
      ignore.case = TRUE
    )) |>
    dplyr::select(
      dplyr::any_of(c(colnames_full)),
      ## TODO
      # rtime
      mz,
      intensity
    ) |>
    dplyr::mutate(join = "x")

  spectra_missing <- colnames_missing |>
    data.frame() |>
    dplyr::mutate(value = NA_character_) |>
    tidyr::pivot_wider(names_from = colnames_missing) |>
    dplyr::mutate(join = "x")

  spectra_harmonized <- spectra_filtered |>
    dplyr::full_join(spectra_missing) |>
    dplyr::select(
      collision_energy := !!as.name(colnames["colname_collision_energy"]),
      compound_id := !!as.name(colnames["colname_compound_id"]),
      exactmass := !!as.name(colnames["colname_exact_mass"]),
      formula := !!as.name(colnames["colname_formula"]),
      inchi := !!as.name(colnames["colname_inchi"]),
      inchikey := !!as.name(colnames["colname_inchikey"]),
      name := !!as.name(colnames["colname_name"]),
      precursorMz := !!as.name(colnames["colname_precursorMz"]),
      precursorCharge := !!as.name(colnames["colname_precursorCharge"]),
      smiles := !!as.name(colnames["colname_smiles"]),
      spectrum_id := !!as.name(colnames["colname_spectrum_id"]),
      splash := !!as.name(colnames["colname_splash"]),
      synonyms := !!as.name(colnames["colname_synonyms"]),
      ## TODO
      # rtime
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
