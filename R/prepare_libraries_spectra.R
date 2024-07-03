#' @title Prepare libraries of spectra
#'
#' @description This function prepares spectra
#'    to be used for spectral matching
#'
#' @include export_spectra_2.R
#' @include extract_spectra.R
#' @include harmonize_spectra.R
#' @include import_spectra.R
#' @include sanitize_spectra.R
#'
#' @param input File containing spectra
#' @param output_neg Negative output file
#' @param output_pos Positive output file
#' @param output_sop SOP output file
#' @param polarity MS polarity
#' @param metad Metadata to identify the library
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
prepare_libraries_spectra <-
  function(input = get_params(step = "prepare_libraries_spectra")$files$libraries$spectral$exp$raw,
           output_neg = get_params(step = "prepare_libraries_spectra")$files$libraries$spectral$exp$neg,
           output_pos = get_params(step = "prepare_libraries_spectra")$files$libraries$spectral$exp$pos,
           output_sop = get_params(step = "prepare_libraries_spectra")$files$libraries$sop$prepared$spectral,
           metad = "myLib",
           col_ad = get_params(step = "prepare_libraries_spectra")$names$mgf$adduct,
           col_ce = get_params(step = "prepare_libraries_spectra")$names$mgf$collision_energy,
           col_ci = get_params(step = "prepare_libraries_spectra")$names$mgf$compound_id,
           col_em = get_params(step = "prepare_libraries_spectra")$names$mgf$exact_mass,
           col_in = get_params(step = "prepare_libraries_spectra")$names$mgf$inchi,
           col_io = get_params(step = "prepare_libraries_spectra")$names$mgf$inchi_no_stereo,
           col_ik = get_params(step = "prepare_libraries_spectra")$names$mgf$inchikey,
           col_il = get_params(step = "prepare_libraries_spectra")$names$mgf$inchikey_no_stereo,
           col_mf = get_params(step = "prepare_libraries_spectra")$names$mgf$molecular_formula,
           col_na = get_params(step = "prepare_libraries_spectra")$names$mgf$name,
           col_po = get_params(step = "prepare_libraries_spectra")$names$mgf$polarity,
           col_sm = get_params(step = "prepare_libraries_spectra")$names$mgf$smiles,
           col_sn = get_params(step = "prepare_libraries_spectra")$names$mgf$smiles_no_stereo,
           col_si = get_params(step = "prepare_libraries_spectra")$names$mgf$spectrum_id,
           col_sp = get_params(step = "prepare_libraries_spectra")$names$mgf$splash,
           col_sy = get_params(step = "prepare_libraries_spectra")$names$mgf$synonyms,
           col_xl = get_params(step = "prepare_libraries_spectra")$names$mgf$xlogp) {
    if (!all(lapply(X = list(output_neg, output_pos), FUN = file.exists) |> unlist())) {
      if (file.exists(input)) {
        log_debug("Importing")
        spectra <- input |>
          import_spectra()

        log_debug("Sanitizing")
        spectra_sanitized <- spectra |>
          sanitize_spectra()
        rm(spectra)
        log_debug("Harmonizing ...")
        log_debug("... pos")
        spectra_harmonized_pos <- spectra_sanitized |>
          extract_spectra() |>
          harmonize_spectra(mode = "pos") |>
          ## TODO report the issue as otherwise precursorMz is lost
          tidytable::mutate(precursor_mz = precursorMz)
        spectra_harmonized_neg <- spectra_sanitized |>
          extract_spectra() |>
          harmonize_spectra(mode = "neg") |>
          ## TODO report the issue as otherwise precursorMz is lost
          tidytable::mutate(precursor_mz = precursorMz)
        rm(spectra_sanitized)
      } else {
        log_debug("Your input file does not exist, returning empty lib instead.")
        spectra_harmonized_pos <- tidytable::tidytable(
          "compound_id" = "fake_compound",
          "adduct" = NA_character_,
          "collision_energy" = NA_character_,
          "exactmass" = NA_real_,
          "formula" = NA_character_,
          "inchi" = NA_character_,
          "inchi_no_stereo" = NA_character_,
          "inchikey" = NA_character_,
          "inchikey_no_stereo" = NA_character_,
          "name" = NA_character_,
          "precursorMz" = 0,
          "precursorCharge" = NA_integer_,
          "smiles" = NA_character_,
          "smiles_no_stereo" = NA_character_,
          "spectrum_id" = NA_integer_,
          "splash" = NA_character_,
          "synonyms" = NA_character_,
          "xlogp" = NA_character_,
          "rtime" = NA_real_,
          "mz" = c(1, 2, 3) |> list(),
          "intensity" = c(1, 2, 3) |> list(),
          "library" = NA_character_,
          "precursor_mz" = 0
        )
        spectra_harmonized_neg <- spectra_harmonized_pos
      }
      log_debug("Extracting structures for the SOP library.")
      sop <- spectra_harmonized_pos |>
        tidytable::bind_rows(spectra_harmonized_neg) |>
        tidytable::distinct(
          structure_inchikey = inchikey,
          structure_smiles = smiles,
          structure_smiles_no_stereo = smiles_no_stereo
        ) |>
        tidytable::mutate(
          structure_inchikey_no_stereo = stringi::stri_sub(
            str = structure_inchikey,
            from = 1,
            to = 14
          ),
          organism_name = NA_character_
        )
      log_debug("Exporting")
      export_output(
        sop,
        file = output_sop
      )
      export_spectra_2(
        file = output_pos,
        spectra = spectra_harmonized_pos,
        meta = metad
      )
      export_spectra_2(
        file = output_neg,
        spectra = spectra_harmonized_neg,
        meta = metad
      )
      rm(spectra_harmonized_pos, spectra_harmonized_neg)
    } else {
      log_debug("Library already exists")
    }
    export_params(
      parameters = get_params(step = "prepare_libraries_spectra"),
      step = "prepare_libraries_spectra"
    )

    return(c(
      "pos" = output_pos,
      "neg" = output_neg,
      "sop" = output_sop
    ))
  }

## See https://github.com/markfairbanks/tidytable/issues/269
.datatable.aware <- TRUE
