#' @title Harmonize spectra
#'
#' @description This function harmonizes spectra header fields by mapping
#'     library-specific column names to standardized field names. It handles
#'     different naming conventions across spectral libraries and ensures
#'     consistent metadata structure.
#'
#' @param spectra Data frame containing spectra data to be harmonized
#' @param metad Metadata to identify the library source
#' @param mode MS ionization mode. Must contain 'pos' or 'neg'
#' @param col_ad Name of the adduct field in the input spectra
#' @param col_ce Name of the collision energy field
#' @param col_ci Name of the compound ID field
#' @param col_em Name of the exact mass field
#' @param col_in Name of the InChI field
#' @param col_io Name of the InChI without stereochemistry field
#' @param col_ik Name of the InChIKey field
#' @param col_il Name of the InChIKey connectivity layer field
#' @param col_mf Name of the molecular formula field
#' @param col_na Name of the compound name field
#' @param col_po Name of the polarity field
#' @param col_sm Name of the SMILES field
#' @param col_sn Name of the SMILES without stereochemistry field
#' @param col_si Name of the spectrum ID field
#' @param col_sp Name of the SPLASH field
#' @param col_sy Name of the synonyms field
#' @param col_xl Name of the xLogP field
#'
#' @return A data frame with harmonized column names and standardized field names
#'
#' @examples NULL
harmonize_spectra <- function(
  spectra,
  metad = get("metad", envir = parent.frame()),
  mode,
  col_ad = get("col_ad", envir = parent.frame()),
  col_ce = get("col_ce", envir = parent.frame()),
  col_ci = get("col_ci", envir = parent.frame()),
  col_em = get("col_em", envir = parent.frame()),
  col_in = get("col_in", envir = parent.frame()),
  col_io = get("col_io", envir = parent.frame()),
  col_ik = get("col_ik", envir = parent.frame()),
  col_il = get("col_il", envir = parent.frame()),
  col_mf = get("col_mf", envir = parent.frame()),
  col_na = get("col_na", envir = parent.frame()),
  col_po = get("col_po", envir = parent.frame()),
  col_sm = get("col_sm", envir = parent.frame()),
  col_sn = get("col_sn", envir = parent.frame()),
  col_si = get("col_si", envir = parent.frame()),
  col_sp = get("col_sp", envir = parent.frame()),
  col_sy = get("col_sy", envir = parent.frame()),
  col_xl = get("col_xl", envir = parent.frame())
) {
  # ============================================================================
  # Input Validation
  # ============================================================================

  # Validate mode first (cheapest check)
  if (!grepl("pos|neg", mode, ignore.case = TRUE)) {
    stop("Mode must contain 'pos' or 'neg', got: ", mode)
  }

  # Validate spectra is a data frame
  if (!is.data.frame(spectra)) {
    stop("Input 'spectra' must be a data frame")
  }

  # Validate all column name parameters
  col_params <- list(
    col_ad = col_ad,
    col_ce = col_ce,
    col_ci = col_ci,
    col_em = col_em,
    col_in = col_in,
    col_io = col_io,
    col_ik = col_ik,
    col_il = col_il,
    col_mf = col_mf,
    col_na = col_na,
    col_po = col_po,
    col_sm = col_sm,
    col_sn = col_sn,
    col_si = col_si,
    col_sp = col_sp,
    col_sy = col_sy,
    col_xl = col_xl
  )

  # Check all parameters are single character strings or NULL
  is_valid <- sapply(col_params, function(param) {
    is.null(param) || (is.character(param) && length(param) == 1L)
  })

  if (!all(is_valid)) {
    invalid_params <- names(col_params)[!is_valid]
    stop(
      "Column name parameter(s) must be single character strings or NULL: ",
      paste(invalid_params, collapse = ", ")
    )
  }

  # ============================================================================
  # Harmonize Column Names
  # ============================================================================

  # logger::log_trace("Harmonizing spectra headers for mode: {mode}")

  # Define standard column names
  columns <- c(
    "adduct",
    "collision_energy",
    "compound_id",
    "exactmass",
    "formula",
    "inchi",
    "inchi_no_stereo",
    "inchikey",
    "inchikey_connectivity_layer",
    "name",
    "smiles",
    "smiles_no_stereo",
    "spectrum_id",
    "splash",
    "synonyms",
    "xlogp"
  )

  # Map provided column names to standard names
  columns_full <- c(
    "adduct" = col_ad,
    "collision_energy" = col_ce,
    "compound_id" = col_ci,
    "exactmass" = col_em,
    "formula" = col_mf,
    "inchi" = col_in,
    "inchi_no_stereo" = col_io,
    "inchikey" = col_ik,
    "inchikey_connectivity_layer" = col_il,
    "name" = col_na,
    "smiles" = col_sm,
    "smiles_no_stereo" = col_sn,
    "spectrum_id" = col_si,
    "splash" = col_sp,
    "synonyms" = col_sy,
    "xlogp" = col_xl
  )

  # Remove NA mappings (columns not provided)
  columns_full <- columns_full[!is.na(columns_full)]

  # Identify missing columns that need to be added
  columns_missing <- columns[!columns %in% names(columns_full)]
  names(columns_missing) <- columns_missing

  # Create data frame for missing columns
  spectra_missing <- columns_missing |>
    data.frame() |>
    tidytable::as_tidytable() |>
    tidytable::bind_cols(tidytable::tidytable(value = NA_character_)) |>
    tidytable::pivot_wider(names_from = columns_missing) |>
    tidytable::mutate(join = "x")

  # Ensure precursorCharge column exists
  if (!"precursorCharge" %in% names(spectra)) {
    spectra$precursorCharge <- NA_integer_
  }

  spectra_filtered <- spectra |>
    data.frame() |>
    tidytable::as_tidytable() |>
    tidytable::filter(
      grepl(
        pattern = mode,
        x = !!as.name(col_po),
        ignore.case = TRUE
      ) |
        grepl(
          pattern = if (mode == "pos") {
            1
          } else {
            -1
          },
          x = !!as.name(col_po),
          ignore.case = TRUE
        )
    ) |>
    tidytable::select(
      tidyselect::any_of(c(columns_full)),
      tidyselect::any_of(c("precursorCharge")),
      precursorMz,
      tidyselect::any_of(c("rtime")),
      mz,
      intensity
    ) |>
    tidytable::mutate(
      join = "x",
      precursorCharge = tidytable::coalesce(
        precursorCharge,
        tidytable::if_else(
          condition = grepl("pos", mode, ignore.case = TRUE),
          1L,
          -1L
        )
      )
    )

  spectra_harmonized <- spectra_filtered |>
    tidytable::full_join(spectra_missing) |>
    tidytable::select(tidyselect::any_of(c(
      "adduct",
      "collision_energy",
      "compound_id",
      "exactmass",
      "formula",
      "inchi",
      "inchi_no_stereo",
      "inchikey",
      "inchikey_connectivity_layer",
      "name",
      "precursorMz",
      "precursorCharge",
      "smiles",
      "smiles_no_stereo",
      "spectrum_id",
      "splash",
      "synonyms",
      "xlogp",
      "rtime",
      "mz",
      "intensity"
    ))) |>
    tidytable::mutate(
      library = metad,
      exactmass = as.numeric(exactmass),
      spectrum_id = tidytable::if_else(
        condition = is.na(spectrum_id),
        true = tidytable::row_number() |>
          as.character(),
        false = as.character(spectrum_id)
      ),
      compound_id = tidytable::if_else(
        condition = is.na(compound_id),
        true = name |>
          as.character(),
        false = compound_id |>
          as.character()
      )
    ) |>
    data.frame()
  rm(spectra_filtered)

  return(spectra_harmonized)
}
