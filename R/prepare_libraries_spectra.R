#' @title Prepare libraries of spectra
#'
#' @description This function prepares spectra
#'    to be used for spectral matching
#'
#' @include export_spectra_2.R
#' @include extract_spectra.R
#' @include harmonize_spectra.R
#' @include import_spectra.R
#'
#' @param input File containing spectra
#' @param output Output file
#' @param polarity MS polarity
#' @param metad Metadata to identify the library
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
           output = get_params(step = "prepare_libraries_spectra")$files$libraries$spectral$exp,
           polarity = get_params(step = "prepare_libraries_spectra")$ms$polarity,
           metad = "myLib",
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
    stopifnot(
      "Polarity must be 'pos' or 'neg'." =
        polarity %in% c("pos", "neg")
    )
    if (length(output) > 1) {
      output <- output[output |>
        grepl(pattern = polarity)] |>
        as.character()
    }
    if (!file.exists(output)) {
      if (file.exists(input)) {
        log_debug("Importing")
        spectra <- input |>
          import_spectra()

        log_debug("Harmonizing")
        spectra_harmonized <- spectra |>
          extract_spectra() |>
          harmonize_spectra(mode = polarity) |>
          ## TODO report the issue as otherwise precursorMz is lost
          tidytable::mutate(precursor_mz = precursorMz)
        rm(spectra)
      } else {
        log_debug("Your input file does not exist, returning empty lib instead.")
        spectra_harmonized <- tidytable::tidytable(
          "compound_id" = "fake_compound",
          "collision_energy" = NA_character_,
          "exactmass" = NA_real_,
          "formula" = NA_character_,
          "inchi" = NA_character_,
          "inchi_no_stereo" = NA_character_,
          "inchikey" = NA_character_,
          "inchikey_no_stereo" = NA_character_,
          "name" = NA_character_,
          "precursorMz" = NA_real_,
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
          "precursor_mz" = NA_real_
        )
      }
      log_debug("Exporting")
      export_spectra_2(
        file = output,
        spectra = spectra_harmonized,
        meta = metad
      )
      rm(spectra_harmonized)
    }
    export_params(
      parameters = get_params(step = "prepare_libraries_spectra"),
      step = "prepare_libraries_spectra"
    )

    return(output)
  }
