#' @title Prepare libraries of spectra
#'
#' @description This function prepares spectra
#'    to be used for spectral matching
#'
#' @include export_params.R
#' @include export_spectra_2.R
#' @include extract_spectra.R
#' @include harmonize_spectra.R
#' @include import_spectra.R
#'
#' @param input File containing spectra
#' @param output Output file
#' @param polarity MS polarity
#' @param metad Optional. Metadata as in 'CompoundDb' package
#' @param col_ce Name of the collision energy in mgf
#' @param col_ci Name of the compound id in mgf
#' @param col_em Name of the exact mass in mgf
#' @param col_in Name of the InChI in mgf
#' @param col_io Name of the InChI 2D in mgf
#' @param col_ik Name of the InChIKey in mgf
#' @param col_il Name of the InChIKey 2D in mgf
#' @param col_mf Name of the molecular formula in mgf
#' @param col_na Name of the name in mgf
#' @param col_po Name of the polarity in mgf
#' @param col_sm Name of the SMILES in mgf
#' @param col_sn Name of the SMILES 2D in mgf
#' @param col_si Name of the spectrum id in mgf
#' @param col_sp Name of the SPLASH in mgf
#' @param col_sy Name of the synonyms in mgf
#' @param col_xl Name of the xlogp in mgf
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_libraries_spectra <-
  function(input = params$files$libraries$spectral$exp$raw,
           output = params$files$libraries$spectral$exp,
           polarity = params$ms$polarity,
           metad = NULL,
           col_ce = params$names$mgf$collision_energy,
           col_ci = params$names$mgf$compound_id,
           col_em = params$names$mgf$exact_mass,
           col_in = params$names$mgf$inchi,
           col_io = params$names$mgf$inchi_2D,
           col_ik = params$names$mgf$inchikey,
           col_il = params$names$mgf$inchikey_2D,
           col_mf = params$names$mgf$molecular_formula,
           col_na = params$names$mgf$name,
           col_po = params$names$mgf$polarity,
           col_sm = params$names$mgf$smiles,
           col_sn = params$names$mgf$smiles_2D,
           col_si = params$names$mgf$spectrum_id,
           col_sp = params$names$mgf$splash,
           col_sy = params$names$mgf$synonyms,
           col_xl = params$names$mgf$xlogp,
           parameters = params) {
    stopifnot("Your input file does not exist." = file.exists(input))
    stopifnot(
      "Polarity must be 'pos' or 'neg'." =
        polarity %in% c("pos", "neg")
    )
    params <<- parameters

    if (length(output) > 1) {
      output <- output[output |>
        grepl(pattern = polarity)] |>
        as.character()
    }

    if (!file.exists(output)) {
      log_debug("Importing")
      spectra <- input |>
        import_spectra()

      log_debug("Harmonizing")
      spectra_harmonized <- spectra |>
        extract_spectra() |>
        harmonize_spectra(mode = polarity) |>
        ## TODO report the issue as otherwise precursorMz is lost
        tidytable::mutate(precursor_mz = precursorMz)

      log_debug("Exporting")
      export_spectra_2(
        file = output,
        spectra = spectra_harmonized,
        meta = metad
      )
    }
    export_params(step = "prepare_libraries_spectra")

    return(output)
  }
