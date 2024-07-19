#' @title Harmonize spectra
#'
#' @description This function harmonizes spectra headers
#'
#' @importFrom tidytable any_of as_tidytable filter full_join mutate pivot_wider row_number select
#'
#' @param spectra Spectra object to be harmonized
#' @param metad Metadata to identify the library
#' @param mode MS ionization mode. Must contain 'pos' or 'neg'
#' @param col_ad Name of the adduct in mgf
#' @param col_ce Name of the collision energy in mgf
#' @param col_ci Name of the compound id in mgf
#' @param col_em Name of the exact mass in mgf
#' @param col_in Name of the InChI in mgf
#' @param col_io Name of the InChI without stereo in mgf
#' @param col_ik Name of the InChIKey in mgf
#' @param col_il Name of the InChIKey without stereo in mgf
#' @param col_mf Name of the molecular formula in mgf
#' @param col_na Name of the name in mgf
#' @param col_po Name of the polarity in mgf
#' @param col_sm Name of the SMILES in mgf
#' @param col_sn Name of the SMILES without stereo in mgf
#' @param col_si Name of the spectrum id in mgf
#' @param col_sp Name of the SPLASH in mgf
#' @param col_sy Name of the synonyms in mgf
#' @param col_xl Name of the xlogp in mgf
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
harmonize_spectra <- function(spectra,
                              metad = get(
                                "metad",
                                envir = parent.frame()
                              ),
                              mode,
                              col_ad = get(
                                "col_ad",
                                envir = parent.frame()
                              ),
                              col_ce = get(
                                "col_ce",
                                envir = parent.frame()
                              ),
                              col_ci = get(
                                "col_ci",
                                envir = parent.frame()
                              ),
                              col_em = get(
                                "col_em",
                                envir = parent.frame()
                              ),
                              col_in = get(
                                "col_in",
                                envir = parent.frame()
                              ),
                              col_io = get(
                                "col_io",
                                envir = parent.frame()
                              ),
                              col_ik = get(
                                "col_ik",
                                envir = parent.frame()
                              ),
                              col_il = get(
                                "col_il",
                                envir = parent.frame()
                              ),
                              col_mf = get(
                                "col_mf",
                                envir = parent.frame()
                              ),
                              col_na = get(
                                "col_na",
                                envir = parent.frame()
                              ),
                              col_po = get(
                                "col_po",
                                envir = parent.frame()
                              ),
                              col_sm = get(
                                "col_sm",
                                envir = parent.frame()
                              ),
                              col_sn = get(
                                "col_sn",
                                envir = parent.frame()
                              ),
                              col_si = get(
                                "col_si",
                                envir = parent.frame()
                              ),
                              col_sp = get(
                                "col_sp",
                                envir = parent.frame()
                              ),
                              col_sy = get(
                                "col_sy",
                                envir = parent.frame()
                              ),
                              col_xl = get(
                                "col_xl",
                                envir = parent.frame()
                              )) {
  columns <- c(
    "adduct",
    "collision_energy",
    "compound_id",
    "exactmass",
    "formula",
    "inchi",
    "inchi_no_stereo",
    "inchikey",
    "inchikey_no_stereo",
    "name",
    "smiles",
    "smiles_no_stereo",
    "spectrum_id",
    "splash",
    "synonyms",
    "xlogp"
  )
  columns_full <-
    c(
      "adduct" = col_ad,
      "collision_energy" = col_ce,
      "compound_id" = col_ci,
      "exactmass" = col_em,
      "formula" = col_mf,
      "inchi" = col_in,
      "inchi_no_stereo" = col_io,
      "inchikey" = col_ik,
      "inchikey_no_stereo" = col_il,
      "name" = col_na,
      "smiles" = col_sm,
      "smiles_no_stereo" = col_sn,
      "spectrum_id" = col_si,
      "splash" = col_sp,
      "synonyms" = col_sy,
      "xlogp" = col_xl
    )
  columns_full <- columns_full[!is.na((columns_full))]
  columns_missing <-
    columns[!columns %in% names(columns_full)]
  names(columns_missing) <- columns_missing

  spectra_missing <- columns_missing |>
    data.frame() |>
    as_tidytable() |>
    bind_cols(tidytable(value = NA_character_)) |>
    pivot_wider(names_from = columns_missing) |>
    bind_cols(tidytable(join = "x"))

  spectra_filtered <- spectra |>
    data.frame() |>
    as_tidytable() |>
    filter(
      grepl(
        pattern = mode,
        x = !!as.name(col_po),
        ignore.case = TRUE
      ) | grepl(
        pattern = if (mode == "pos") {
          1
        } else {
          0
        },
        x = !!as.name(col_po),
        ignore.case = TRUE
      )
    ) |>
    select(
      any_of(c(columns_full)),
      precursorCharge,
      precursorMz,
      rtime,
      mz,
      intensity
    ) |>
    mutate(join = "x")

  spectra_harmonized <- spectra_filtered |>
    full_join(spectra_missing) |>
    select(
      adduct,
      collision_energy,
      compound_id,
      exactmass,
      formula,
      inchi,
      inchi_no_stereo,
      inchikey,
      inchikey_no_stereo,
      name,
      precursorMz,
      precursorCharge,
      smiles,
      smiles_no_stereo,
      spectrum_id,
      splash,
      synonyms,
      xlogp,
      rtime,
      mz,
      intensity
    ) |>
    mutate(
      library = metad,
      exactmass = as.numeric(exactmass),
      spectrum_id = ifelse(
        test = is.na(spectrum_id),
        yes = row_number(),
        no = as.numeric(spectrum_id)
      ),
      compound_id = ifelse(
        test = is.na(compound_id),
        yes = name,
        no = compound_id
      )
    ) |>
    data.frame()
  rm(spectra_filtered)

  return(spectra_harmonized)
}
