#' Harmonize Spectra for a Given Polarity
#'
#' @description Internal helper to harmonize extracted spectra for a specific
#'     polarity mode, applying column mappings and fixing precursor/inchikey issues.
#'
#' @param spectra_extracted List of extracted spectra data frames.
#' @param mode Character "pos" or "neg".
#' @param metad Character library metadata name.
#' @param col_* Character column name mappings (see parent function params).
#'
#' @return Data frame with harmonized spectra.
#' @keywords internal
harmonize_spectra_polarity <- function(
  spectra_extracted,
  mode,
  metad,
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
  purrr::map(
    .x = spectra_extracted,
    .f = harmonize_spectra,
    mode = mode,
    metad = metad,
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
  ) |>
    purrr::map(
      .f = function(x) {
        x |>
          tidytable::rename(precursor_mz = precursorMz) |>
          tidytable::mutate(
            inchikey_connectivity_layer = tidytable::if_else(
              condition = is.na(inchikey),
              true = inchikey_connectivity_layer,
              false = gsub(pattern = "-.*", replacement = "", x = inchikey)
            )
          ) |>
          data.frame()
      }
    ) |>
    purrr::list_rbind()
}

#' Create Empty Spectral Library Template
#'
#' @description Internal helper returning an empty Spectra object with expected
#'     columns when input files are missing.
#'
#' @return Spectra object with one fake spectrum.
#' @keywords internal
create_empty_spectral_library <- function() {
  tidytable::tidytable(
    compound_id = "fake_compound",
    adduct = NA_character_,
    collision_energy = NA_character_,
    exactmass = NA_real_,
    formula = NA_character_,
    inchi = NA_character_,
    inchi_no_stereo = NA_character_,
    inchikey = NA_character_,
    inchikey_connectivity_layer = NA_character_,
    name = NA_character_,
    precursorMz = 0,
    precursorCharge = NA_integer_,
    smiles = NA_character_,
    smiles_no_stereo = NA_character_,
    spectrum_id = NA_integer_,
    splash = NA_character_,
    synonyms = NA_character_,
    xlogp = NA_character_,
    rtime = NA_real_,
    mz = list(c(1, 2, 3)),
    intensity = list(c(1, 2, 3)),
    library = NA_character_,
    precursor_mz = 0
  ) |>
    data.frame() |>
    Spectra::Spectra()
}

#' Create Empty SOP Library Template
#'
#' @description Internal helper returning an empty SOP structure template.
#'
#' @return Data frame with SOP columns, 1 row of NAs.
#' @keywords internal
create_empty_sop_library <- function() {
  tidytable::tidytable(
    structure_inchikey = NA_character_,
    structure_smiles = NA_character_,
    structure_smiles_no_stereo = NA_character_,
    structure_inchikey_connectivity_layer = NA_character_,
    organism_name = NA_character_
  )
}

#' @title Prepare libraries of spectra
#'
#' @description Prepares spectral libraries for matching by importing,
#'     harmonizing, and splitting spectra by polarity. Exports results as
#'     Spectra RDS files (pos/neg) and a structure-organism pair (SOP) table.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Checks if output files already exist (idempotent).
#'   \item Imports spectral data from input files.
#'   \item Extracts and harmonizes spectra for positive and negative modes.
#'   \item Fixes precursor m/z and InChIKey connectivity layer issues.
#'   \item Exports polarity-specific Spectra objects and SOP table.
#'   \item Returns empty templates if input files are missing.
#' }
#'
#' @include export_spectra_rds.R
#' @include extract_spectra.R
#' @include get_default_paths.R
#' @include get_params.R
#' @include harmonize_spectra.R
#' @include import_spectra.R
#'
#' @param input Character vector of file paths containing spectral data.
#' @param nam_lib Character library name for metadata.
#' @param col_ad Name of the adduct column in MGF.
#' @param col_ce Name of the collision energy column in MGF.
#' @param col_ci Name of the compound ID column in MGF.
#' @param col_em Name of the exact mass column in MGF.
#' @param col_in Name of the InChI column in MGF.
#' @param col_io Name of the InChI without stereo column in MGF.
#' @param col_ik Name of the InChIKey column in MGF.
#' @param col_il Name of the InChIKey connectivity layer column in MGF.
#' @param col_mf Name of the molecular formula column in MGF.
#' @param col_na Name of the name column in MGF.
#' @param col_po Name of the polarity column in MGF.
#' @param col_sm Name of the SMILES column in MGF.
#' @param col_sn Name of the SMILES without stereo column in MGF.
#' @param col_si Name of the spectrum ID column in MGF.
#' @param col_sp Name of the SPLASH column in MGF.
#' @param col_sy Name of the synonyms column in MGF.
#' @param col_xl Name of the xlogp column in MGF.
#'
#' @return Character vector with paths to prepared library files (invisible).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' prepare_libraries_spectra()
#' unlink("data", recursive = TRUE)
#' }
prepare_libraries_spectra <-
  function(
    input = get_params(
      step = "prepare_libraries_spectra"
    )$files$libraries$spectral$raw,
    nam_lib = get_params(step = "prepare_libraries_spectra")$names$libraries,
    col_ad = get_params(step = "prepare_libraries_spectra")$names$mgf$adduct,
    col_ce = get_params(
      step = "prepare_libraries_spectra"
    )$names$mgf$collision_energy,
    col_ci = get_params(
      step = "prepare_libraries_spectra"
    )$names$mgf$compound_id,
    col_em = get_params(
      step = "prepare_libraries_spectra"
    )$names$mgf$exact_mass,
    col_in = get_params(step = "prepare_libraries_spectra")$names$mgf$inchi,
    col_io = get_params(
      step = "prepare_libraries_spectra"
    )$names$mgf$inchi_no_stereo,
    col_ik = get_params(step = "prepare_libraries_spectra")$names$mgf$inchikey,
    col_il = get_params(
      step = "prepare_libraries_spectra"
    )$names$mgf$inchikey_connectivity_layer,
    col_mf = get_params(
      step = "prepare_libraries_spectra"
    )$names$mgf$molecular_formula,
    col_na = get_params(step = "prepare_libraries_spectra")$names$mgf$name,
    col_po = get_params(step = "prepare_libraries_spectra")$names$mgf$polarity,
    col_sm = get_params(step = "prepare_libraries_spectra")$names$mgf$smiles,
    col_sn = get_params(
      step = "prepare_libraries_spectra"
    )$names$mgf$smiles_no_stereo,
    col_si = get_params(
      step = "prepare_libraries_spectra"
    )$names$mgf$spectrum_id,
    col_sp = get_params(step = "prepare_libraries_spectra")$names$mgf$splash,
    col_sy = get_params(step = "prepare_libraries_spectra")$names$mgf$synonyms,
    col_xl = get_params(step = "prepare_libraries_spectra")$names$mgf$xlogp
  ) {
    logger::log_info("Preparing spectral library: {nam_lib}")
    # Define output paths ----
    output_pos <- file.path(
      get_default_paths()$data$interim$libraries$spectra$exp$path,
      paste0(nam_lib, "_pos.rds")
    )
    output_neg <- file.path(
      get_default_paths()$data$interim$libraries$spectra$exp$path,
      paste0(nam_lib, "_neg.rds")
    )
    output_sop <- file.path(
      get_default_paths()$data$interim$libraries$sop$path,
      paste0(nam_lib, "_prepared.tsv.gz")
    )
    # Check if already prepared (idempotency) ----
    outputs_exist <- all(file.exists(c(output_pos, output_neg, output_sop)))
    if (outputs_exist) {
      logger::log_info("Library '{nam_lib}' already prepared; skipping")
      export_params(
        parameters = get_params(step = "prepare_libraries_spectra"),
        step = "prepare_libraries_spectra"
      )
      return(invisible(c(
        "pos" = output_pos,
        "neg" = output_neg,
        "sop" = output_sop
      )))
    }
    # Validate input ----
    if (is.null(input)) {
      input <- "fileDoesNotExist"
    }
    inputs_exist <- all(file.exists(input))
    if (!inputs_exist) {
      logger::log_warn(
        "Input file(s) not found; creating empty library template"
      )
      spectra_pos <- create_empty_spectral_library()
      spectra_neg <- create_empty_spectral_library()
      sop <- create_empty_sop_library()
    } else {
      # Import and extract ----
      logger::log_debug("Importing {length(input)} spectral file(s)")
      spectra <- purrr::map(input, import_spectra, combine = FALSE)
      logger::log_debug("Extracting spectra metadata")
      spectra_extracted <- purrr::map(spectra, extract_spectra)
      # Harmonize for both polarities ----
      logger::log_debug("Harmonizing spectra for positive mode")
      spectra_harmonized_pos <- harmonize_spectra_polarity(
        spectra_extracted,
        mode = "pos",
        metad = nam_lib,
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
      )
      spectra_pos <- Spectra::Spectra(spectra_harmonized_pos)
      logger::log_debug("Harmonizing spectra for negative mode")
      spectra_harmonized_neg <- harmonize_spectra_polarity(
        spectra_extracted,
        mode = "neg",
        metad = nam_lib,
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
      )
      spectra_neg <- Spectra::Spectra(spectra_harmonized_neg)
      # Extract SOP table ----
      logger::log_debug("Extracting structure-organism pairs for SOP library")
      sop <- tidytable::bind_rows(
        spectra_harmonized_pos,
        spectra_harmonized_neg
      ) |>
        tidytable::filter(!is.na(inchikey)) |>
        tidytable::distinct(
          structure_inchikey = inchikey,
          structure_inchikey_connectivity_layer = inchikey_connectivity_layer,
          structure_smiles = smiles,
          structure_smiles_no_stereo = smiles_no_stereo,
          structure_molecular_formula = formula,
          structure_exact_mass = exactmass,
          structure_xlogp = xlogp
        ) |>
        tidytable::distinct(
          structure_inchikey,
          structure_inchikey_connectivity_layer,
          structure_smiles,
          structure_smiles_no_stereo,
          .keep_all = TRUE
        ) |>
        tidytable::mutate(organism_name = NA_character_)
      logger::log_debug(
        "SOP table: {nrow(sop)} unique structures (pos={nrow(spectra_harmonized_pos)}, neg={nrow(spectra_harmonized_neg)} spectra)"
      )
    }
    # Export ----
    logger::log_info("Exporting spectral library files")
    export_spectra_rds(file = output_pos, spectra = spectra_pos)
    export_spectra_rds(file = output_neg, spectra = spectra_neg)
    export_output(sop, file = output_sop)
    export_params(
      parameters = get_params(step = "prepare_libraries_spectra"),
      step = "prepare_libraries_spectra"
    )
    logger::log_success("Spectral library '{nam_lib}' prepared successfully")
    invisible(c(
      "pos" = output_pos,
      "neg" = output_neg,
      "sop" = output_sop
    ))
  }
