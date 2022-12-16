#' @title Prepare LOTUS In Silico DataBase
#'
#' @param input_pos TODO
#' @param input_neg TODO
#' @param output_pos TODO
#' @param output_neg TODO
#' @param export_sqlite TODO
#'
#' @return TODO
#'
#' @export
#'
#' @examples TODO
prepare_isdb_lotus <-
  function(input_pos = paths$data$source$spectra$lotus_isdb$pos,
           input_neg = paths$data$source$spectra$lotus_isdb$neg,
           output_pos = paths$data$interim$spectra$lotus$pos,
           output_neg = paths$data$interim$spectra$lotus$neg,
           export_sqlite = TRUE) {
    log_debug("Importing ...")
    log_debug("... positive spectra")
    spectra_pos <- input_pos |>
      import_spectra()
    log_debug("... negative spectra")
    spectra_neg <- input_neg |>
      import_spectra()

    log_debug("Extracting ...")
    log_debug("... positive spectra")
    spectra_extracted_pos <- spectra_pos |>
      extract_spectra()
    log_debug("... negative spectra")
    spectra_extracted_neg <- spectra_neg |>
      extract_spectra()

    log_debug("Exporting ...")
    if (export_sqlite == TRUE) {
      log_debug("Generating metadata ...")
      ## TODO
      log_debug("... positive spectra")
      cmps_pos <- NULL
      metad_pos <- NULL
      log_debug("... negative spectra")
      cmps_neg <- NULL
      metad_neg <- NULL

      log_debug("... positive spectra")
      spectra_extracted_pos |>
        export_spectra(
          file = output_pos |>
            gsub(
              pattern = ".mgf",
              replacement = ".sqlite",
              fixed = TRUE
            ),
          cmps = cmps_pos,
          metad = metad_pos
        )

      log_debug("... negative spectra")
      spectra_extracted_neg |>
        export_spectra(
          file = output_neg |>
            gsub(
              pattern = ".mgf",
              replacement = ".sqlite",
              fixed = TRUE
            ),
          cmps = cmps_neg,
          metad = metad_neg
        )
    }
    log_debug("... positive spectra")
    spectra_extracted_pos |>
      export_spectra(file = output_pos)

    log_debug("... negative spectra")
    spectra_extracted_neg |>
      export_spectra(file = output_neg)
  }
