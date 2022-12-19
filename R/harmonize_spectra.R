#' @title Harmonize spectra
#'
#' @param spectra TODO
#' @param colnames TODO
#' @param mode TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom dplyr any_of filter full_join mutate row_number select
#' @importFrom tidyr pivot_wider
#'
#' @examples TODO
harmonize_spectra <- function(spectra,
                              colnames = c(
                                colname_compound_id = NA,
                                colname_exact_mass = "EXACTMASS",
                                colname_formula = "MOLECULAR_FORMULA",
                                colname_inchi = "INCHI",
                                colname_inchikey = "NAME",
                                colname_mode = "IONMODE",
                                colname_name = "NAME",
                                colname_smiles = "SMILES",
                                colname_spectrum_id = NA,
                                colname_synonyms = NA
                              ),
                              mode = "pos") {
  ## dirty way to get all columns
  colnames_full <- colnames[!is.na(colnames)] |>
    as.character()
  colnames_missing <- colnames[is.na(colnames)] |>
    names() |>
    gsub(pattern = "colname_",
         replacement = "")
  colnames[is.na(colnames)] <- colnames[is.na(colnames)] |>
    names() |>
    gsub(pattern = "colname_",
         replacement = "")

  spectra_filtered <- spectra |>
    dplyr::filter(grepl(
      pattern = mode,
      x = !!as.name(colnames["colname_mode"]),
      ignore.case = TRUE
    )) |>
    dplyr::select(dplyr::any_of(c(colnames_full)),
                  precursorMz,
                  precursorCharge,
                  ## TODO
                  # rtime
                  mz,
                  intensity) |>
    dplyr::mutate(join = "x")

  spectra_missing <- colnames_missing |>
    data.frame() |>
    dplyr::mutate(value = NA_character_) |>
    tidyr::pivot_wider(names_from = colnames_missing) |>
    dplyr::mutate(join = "x")

  spectra_harmonized <- spectra_filtered |>
    dplyr::full_join(spectra_missing) |>
    dplyr::select(
      compound_id := !!as.name(colnames["colname_compound_id"]),
      exactmass := !!as.name(colnames["colname_exact_mass"]),
      formula := !!as.name(colnames["colname_formula"]),
      inchi := !!as.name(colnames["colname_inchi"]),
      inchikey := !!as.name(colnames["colname_inchikey"]),
      name := !!as.name(colnames["colname_name"]),
      smiles := !!as.name(colnames["colname_smiles"]),
      spectrum_id := !!as.name(colnames["colname_spectrum_id"]),
      synonyms := !!as.name(colnames["colname_synonyms"]),
      precursorMz,
      precursorCharge,
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

  compounds_harmonized <- spectra_harmonized |>
    dplyr::select(compound_id,
                  name,
                  inchi,
                  inchikey,
                  formula,
                  exactmass,
                  synonyms,
                  smiles)

  return(list(spectra_harmonized, compounds_harmonized))
}
