#' @title Harmonize spectra metadata
#'
#' @description Harmonizes spectra header fields by mapping library-specific
#'     column names to standardized field names. Handles different naming
#'     conventions across spectral libraries.
#'
#' @include validations_utils.R
#'
#' @param spectra Data frame containing spectra to harmonize
#' @param metad Metadata identifying the library source
#' @param mode MS ionization mode (must contain 'pos' or 'neg')
#' @param col_ad Adduct field name
#' @param col_ce Collision energy field name
#' @param col_ci Compound ID field name
#' @param col_em Exact mass field name
#' @param col_in InChI field name
#' @param col_io InChI without stereochemistry field name
#' @param col_ik InChIKey field name
#' @param col_il InChIKey connectivity layer field name
#' @param col_mf Molecular formula field name
#' @param col_na Compound name field name
#' @param col_po Polarity field name
#' @param col_sm SMILES field name
#' @param col_sn SMILES without stereochemistry field name
#' @param col_si Spectrum ID field name
#' @param col_sp SPLASH field name
#' @param col_sy Synonyms field name
#' @param col_xl xLogP field name
#'
#' @return Data frame with harmonized column names and standardized fields
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' harmonized <- harmonize_spectra(
#'   spectra = raw_spectra,
#'   metad = "GNPS",
#'   mode = "positive",
#'   col_na = "Compound_Name",
#'   col_sm = "SMILES",
#'   col_ik = "InChIKey"
#'   # ... other column mappings
#' )
#' }
harmonize_spectra <- function(
  spectra,
  metad,
  mode,
  col_ad,
  col_ce,
  col_ci,
  col_em,
  col_in,
  col_io,
  col_ik,
  col_il,
  col_mf,
  col_na,
  col_po,
  col_sm,
  col_sn,
  col_si,
  col_sp,
  col_sy,
  col_xl
) {
  # Input Validation ----
  validate_dataframe(spectra, param_name = "spectra")

  # Validate mode contains 'pos' or 'neg'
  if (!grepl("pos|neg", mode, ignore.case = TRUE)) {
    stop(
      "mode must contain 'pos' or 'neg', got: ",
      mode,
      call. = FALSE
    )
  }

  # Validate all column parameters are character strings or NULL
  validate_column_mappings(list(
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
  ))

  # Harmonize Column Names ----

  # log_trace("Harmonizing spectra headers for mode: {mode}")

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
      tidyselect::any_of(x = c(columns_full)),
      tidyselect::any_of(x = c("precursorCharge")),
      precursorMz,
      tidyselect::any_of(x = c("rtime")),
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
    tidytable::full_join(y = spectra_missing) |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
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
        )
      )
    ) |>
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

# Helper Functions ----

#' Validate column mapping parameters
#' @keywords internal
validate_column_mappings <- function(col_params) {
  # Check all parameters are single character strings or NULL
  is_valid <- vapply(
    col_params,
    function(param) {
      is.null(param) || (is.character(param) && length(param) == 1L)
    },
    logical(1)
  )

  if (!all(is_valid)) {
    invalid_params <- names(col_params)[!is_valid]
    stop(
      "Column name parameter(s) must be single character strings or NULL: ",
      paste(invalid_params, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
