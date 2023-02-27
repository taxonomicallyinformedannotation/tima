#' @title Prepare spectral libraries
#'
#' @description This function prepares spectra to be used for spectral matching
#'
#' @param input File containing spectra
#' @param output Output file
#' @param polarity MS polarity
#' @param metad Optional. Metadata as in 'CompoundDb' package
#' @param col_ce Name of the collision energy in mgf
#' @param col_ci Name of the compound id in mgf
#' @param col_em Name of the exact mass in mgf
#' @param col_in Name of the InChI in mgf
#' @param col_ik Name of the InChIKey in mgf
#' @param col_mf Name of the molecular formula in mgf
#' @param col_na Name of the name in mgf
#' @param col_po Name of the polarity in mgf
#' @param col_sm Name of the SMILES in mgf
#' @param col_si Name of the spectrum id in mgf
#' @param col_sp Name of the SPLASH in mgf
#' @param col_sy Name of the synonyms in mgf
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_spectral_libraries <-
  function(input = params$files$libraries$spectral$raw,
           output = params$files$libraries$spectral,
           mgf_colnames = params$names$mgf,
           polarity = params$ms$polarity,
           metad = NULL,
           col_ce = params$names$mgf$collision_energy,
           col_ci = params$names$mgf$compound_id,
           col_em = params$names$mgf$exact_mass,
           col_in = params$names$mgf$inchi,
           col_ik = params$names$mgf$inchikey,
           col_mf = params$names$mgf$molecular_formula,
           col_na = params$names$mgf$name,
           col_po = params$names$mgf$polarity,
           col_sm = params$names$mgf$smiles,
           col_si = params$names$mgf$spectrum_id,
           col_sp = params$names$mgf$splash,
           col_sy = params$names$mgf$synonyms,
           parameters = NULL) {
    stopifnot("Your input file does not exist." = file.exists(input))
    stopifnot("Polarity must be 'pos' or 'neg'." = polarity %in% c("pos", "neg"))
    if (!is.null(parameters)) {
      params <<- parameters
    }

    if (length(output) > 1) {
      output <- output[output |>
                         grepl(pattern=polarity)]
    }

    if (!file.exists(output)) {
      log_debug("Importing")
      spectra <- input |>
        import_spectra()

      log_debug("Harmonizing")
      spectra_harmonized <- spectra |>
        extract_spectra() |>
        harmonize_spectra(
          co_ce = col_ce,
          co_ci = col_ci,
          co_em = col_em,
          co_in = col_in,
          co_ik = col_ik,
          co_mf = col_mf,
          co_na = col_na,
          co_po = col_po,
          co_sm = col_sm,
          co_si = col_si,
          co_sp = col_sp,
          co_sy = col_sy,
          mode = polarity
        )

      log_debug("Exporting")
      export_spectra_2(
        file = output,
        spectra = spectra_harmonized,
        meta = metad
      )
    }
    if (!is.null(parameters)) {
      export_params(step = "prepare_spectral_libraries")
    }

    return(output)
  }
