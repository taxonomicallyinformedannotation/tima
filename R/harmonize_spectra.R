utils::globalVariables(
  c(
    "collision_energy",
    "compound_id",
    "exactmass",
    "formula",
    "inchi",
    "inchi_2D",
    "inchikey",
    "inchikey_2D",
    "intensity",
    "mz",
    "name",
    "precursorCharge",
    "precursorMz",
    "rtime",
    "smiles",
    "smiles_2D",
    "spectrum_id",
    "splash",
    "synonyms",
    "xlogp"
  )
)

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
#' @param co_io Name of the InChI 2D in mgf
#' @param co_ik Name of the InChIKey in mgf
#' @param co_il Name of the InChIKey 2D in mgf
#' @param co_mf Name of the molecular formula in mgf
#' @param co_na Name of the name in mgf
#' @param co_po Name of the polarity in mgf
#' @param co_sm Name of the SMILES in mgf
#' @param co_sn Name of the SMILES 2D in mgf
#' @param co_si Name of the spectrum id in mgf
#' @param co_sp Name of the SPLASH in mgf
#' @param co_sy Name of the synonyms in mgf
#' @param co_xl Name of the xlogp in mgf
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
harmonize_spectra <- function(spectra,
                              mode,
                              co_ce,
                              co_ci,
                              co_em,
                              co_in,
                              co_io,
                              co_ik,
                              co_il,
                              co_mf,
                              co_na,
                              co_po,
                              co_sm,
                              co_sn,
                              co_si,
                              co_sp,
                              co_sy,
                              co_xl) {
  columns <- c(
    "collision_energy",
    "compound_id",
    "exactmass",
    "formula",
    "inchi",
    "inchi_2D",
    "inchikey",
    "inchikey_2D",
    "name",
    "smiles",
    "smiles_2D",
    "spectrum_id",
    "splash",
    "synonyms",
    "xlogp"
  )
  columns_full <-
    c(
      "collision_energy" = co_ce,
      "compound_id" = co_ci,
      "exactmass" = co_em,
      "formula" = co_mf,
      "inchi" = co_in,
      "inchi_2D" = co_io,
      "inchikey" = co_ik,
      "inchikey_2D" = co_il,
      "name" = co_na,
      "smiles" = co_sm,
      "smiles_2D" = co_sn,
      "spectrum_id" = co_si,
      "splash" = co_sp,
      "synonyms" = co_sy,
      "xlogp" = co_xl
    )
  columns_missing <- columns[!columns %in% names(columns_full)]
  names(columns_missing) <- columns_missing

  spectra_missing <- columns_missing |>
    data.frame() |>
    tidytable::tidytable() |>
    tidytable::mutate(value = NA_character_) |>
    tidytable::pivot_wider(names_from = columns_missing) |>
    tidytable::mutate(join = "x")

  if (!"rtime" %in% colnames(spectra)) {
    log_debug("no retention time found")
    spectra$rtime <- NA_real_
  }

  spectra_filtered <- spectra |>
    tidytable::filter(grepl(
      pattern = mode,
      x = !!as.name(co_po),
      ignore.case = TRUE
    )) |>
    tidytable::select(
      tidytable::any_of(c(columns_full)),
      precursorCharge,
      precursorMz,
      rtime,
      mz,
      intensity
    ) |>
    tidytable::mutate(join = "x")

  spectra_harmonized <- spectra_filtered |>
    tidytable::full_join(spectra_missing) |>
    tidytable::tidytable() |>
    tidytable::select(
      collision_energy,
      compound_id,
      exactmass,
      formula,
      inchi,
      inchi_2D,
      inchikey,
      inchikey_2D,
      name,
      precursorMz,
      precursorCharge,
      smiles,
      smiles_2D,
      spectrum_id,
      splash,
      synonyms,
      xlogp,
      rtime,
      mz,
      intensity
    ) |>
    tidytable::mutate(
      exactmass = as.numeric(exactmass),
      spectrum_id = ifelse(
        test = is.na(spectrum_id),
        yes = tidytable::row_number(),
        no = as.numeric(spectrum_id)
      ),
      compound_id = ifelse(
        test = is.na(compound_id),
        yes = name,
        no = compound_id
      )
    )

  return(spectra_harmonized)
}
