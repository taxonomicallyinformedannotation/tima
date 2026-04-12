#' Harmonize Spectra for a Given Polarity
#'
#' @description Internal helper to harmonize extracted spectra for a specific
#'     polarity mode, applying column mappings and fixing precursor/inchikey issues.
#'
#' @param spectra_extracted [list] List of extracted spectra data frames.
#' @param mode [character] Character "pos" or "neg".
#' @param metad [character] Character library metadata name.
#' @param col_ad [character] Adduct field name
#' @param col_ce [character] Collision energy field name
#' @param col_ci [character] Compound ID field name
#' @param col_in [character] InChI field name
#' @param col_io [character] InChI without stereochemistry field name
#' @param col_ik [character] InChIKey field name
#' @param col_il [character] InChIKey connectivity layer field name
#' @param col_na [character] Compound name field name
#' @param col_po [character] Polarity field name
#' @param col_sm [character] SMILES field name
#' @param col_sn [character] SMILES without stereochemistry field name
#' @param col_si [character] Spectrum ID field name
#' @param col_sp [character] SPLASH field name
#' @param col_sy [character] Synonyms field name
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
  col_in,
  col_io,
  col_ik,
  col_il,
  col_na,
  col_po,
  col_sm,
  col_sn,
  col_si,
  col_sp,
  col_sy
) {
  purrr::map(
    .x = spectra_extracted,
    .f = harmonize_spectra,
    mode = mode,
    metad = metad,
    col_ad = col_ad,
    col_ce = col_ce,
    col_ci = col_ci,
    col_in = col_in,
    col_io = col_io,
    col_ik = col_ik,
    col_il = col_il,
    col_na = col_na,
    col_po = col_po,
    col_sm = col_sm,
    col_sn = col_sn,
    col_si = col_si,
    col_sp = col_sp,
    col_sy = col_sy
  ) |>
    purrr::map(
      .f = function(x) {
        x |>
          tidytable::rename(precursor_mz = precursorMz) |>
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
    xlogp = NA_real_,
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
    structure_smiles = NA_character_,
    structure_smiles_no_stereo = NA_character_,
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
#' @param input [character] Character vector of file paths containing spectral data.
#' @param nam_lib [character] Character library name for metadata.
#' @param col_ad [character] Name of the adduct column in MGF.
#' @param col_ce [character] Name of the collision energy column in MGF.
#' @param col_ci [character] Name of the compound ID column in MGF.
#' @param col_in [character] Name of the InChI column in MGF.
#' @param col_io [character] Name of the InChI without stereo column in MGF.
#' @param col_ik [character] Name of the InChIKey column in MGF.
#' @param col_il [character] Name of the InChIKey connectivity layer column in MGF.
#' @param col_na [character] Name of the name column in MGF.
#' @param col_po [character] Name of the polarity column in MGF.
#' @param col_sm [character] Name of the SMILES column in MGF.
#' @param col_sn [character] Name of the SMILES without stereo column in MGF.
#' @param col_si [character] Name of the spectrum ID column in MGF.
#' @param col_sp [character] Name of the SPLASH column in MGF.
#' @param col_sy [character] Name of the synonyms column in MGF.
#'
#' @return Character vector with paths to prepared library files (invisible).
#'
#' @family preparation
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
    col_in = get_params(step = "prepare_libraries_spectra")$names$mgf$inchi,
    col_io = get_params(
      step = "prepare_libraries_spectra"
    )$names$mgf$inchi_no_stereo,
    col_ik = get_params(step = "prepare_libraries_spectra")$names$mgf$inchikey,
    col_il = get_params(
      step = "prepare_libraries_spectra"
    )$names$mgf$inchikey_connectivity_layer,
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
    col_sy = get_params(step = "prepare_libraries_spectra")$names$mgf$synonyms
  ) {
    # Initialize logging context
    ctx <- log_operation(
      "prepare_libraries_spectra",
      library_name = nam_lib,
      n_input_files = length(input)
    )

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
      log_complete(
        ctx,
        status = "cached",
        note = "Library already prepared, skipping"
      )
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
      log_warn(
        "Input file(s) not found; creating empty library template"
      )
      spectra_pos <- create_empty_spectral_library()
      spectra_neg <- create_empty_spectral_library()
      sop <- create_empty_sop_library()
    } else {
      # Import and extract ----
      log_metadata(ctx, phase = "importing", n_files = length(input))
      spectra <- purrr::map(.x = input, .f = import_spectra, combine = FALSE)

      log_metadata(ctx, phase = "extracting")
      spectra_extracted <- purrr::map(.x = spectra, .f = extract_spectra)

      # Harmonize for both polarities ----
      log_metadata(ctx, phase = "harmonizing", polarity = "positive")
      spectra_harmonized_pos <- harmonize_spectra_polarity(
        spectra_extracted,
        mode = "pos",
        metad = nam_lib,
        col_ad,
        col_ce,
        col_ci,
        col_in,
        col_io,
        col_ik,
        col_il,
        col_na,
        col_po,
        col_sm,
        col_sn,
        col_si,
        col_sp,
        col_sy
      )
      spectra_pos <- Spectra::Spectra(object = spectra_harmonized_pos)

      log_metadata(
        ctx,
        phase = "harmonizing",
        polarity = "negative",
        n_pos_spectra = length(spectra_pos)
      )
      spectra_harmonized_neg <- harmonize_spectra_polarity(
        spectra_extracted,
        mode = "neg",
        metad = nam_lib,
        col_ad,
        col_ce,
        col_ci,
        col_in,
        col_io,
        col_ik,
        col_il,
        col_na,
        col_po,
        col_sm,
        col_sn,
        col_si,
        col_sp,
        col_sy
      )
      spectra_neg <- Spectra::Spectra(object = spectra_harmonized_neg)

      # Extract SOP table ----
      log_metadata(
        ctx,
        phase = "sop_extraction",
        n_neg_spectra = length(spectra_neg)
      )
      sop <- tidytable::bind_rows(
        spectra_harmonized_pos,
        spectra_harmonized_neg
      ) |>
        tidytable::filter(!is.na(smiles)) |>
        tidytable::distinct(
          structure_smiles = smiles,
          structure_smiles_no_stereo = smiles_no_stereo
        ) |>
        tidytable::distinct(
          structure_smiles,
          structure_smiles_no_stereo
        ) |>
        tidytable::mutate(organism_name = NA_character_)
    }

    # Export ----
    log_metadata(
      ctx,
      phase = "exporting",
      n_unique_structures = nrow(sop),
      n_pos_spectra = length(spectra_pos),
      n_neg_spectra = length(spectra_neg)
    )
    export_spectra_rds(file = output_pos, spectra = spectra_pos)
    export_spectra_rds(file = output_neg, spectra = spectra_neg)
    export_output(sop, file = output_sop)
    export_params(
      parameters = get_params(step = "prepare_libraries_spectra"),
      step = "prepare_libraries_spectra"
    )

    log_complete(
      ctx,
      n_structures = nrow(sop),
      n_spectra_total = length(spectra_pos) + length(spectra_neg),
      files_exported = 3
    )

    invisible(c(
      "pos" = output_pos,
      "neg" = output_neg,
      "sop" = output_sop
    ))
  }
