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
#' @importFrom CompoundDb make_metadata
#' @importFrom curl curl_fetch_memory
#' @importFrom dplyr mutate rename select
#' @importFrom jsonlite fromJSON
#'
#' @examples TODO
prepare_isdb_lotus <-
  function(input_pos = paths$data$source$spectra$lotus$pos,
           input_neg = paths$data$source$spectra$lotus$neg,
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
    create_dir(export = output_pos)
    if (export_sqlite == TRUE) {
      log_debug("Generating metadata ...")
      ## Probably better to store it before
      req <-
        curl::curl_fetch_memory(url = "https://zenodo.org/api/records/5607185")
      content <- jsonlite::fromJSON(txt = rawToChar(req$content))
      version <- ifelse(
        !is.null(content$metadata$version),
        content$metadata$version,
        content$metadata$relations$version[1, 1]
      )
      metad <- CompoundDb::make_metadata(
        source = "LOTUS",
        url = "https://doi.org/10.5281/zenodo.5607185",
        source_version = version,
        source_date = content[["metadata"]][["publication_date"]],
        organism = "Life"
      )

      log_debug("... positive spectra")
      spectra_extracted_pos <- spectra_extracted_pos |>
        dplyr::rename(compound_id = FILENAME) |>
        dplyr::mutate(spectrum_id = dplyr::row_number())
      cmps_pos <- spectra_extracted_pos |>
        dplyr::mutate(
          exactmass = as.numeric(EXACTMASS),
          synonyms = NA_character_
        ) |>
        dplyr::select(
          compound_id = compound_id,
          name = compound_id,
          inchi = INCHI,
          ## Not ideal
          inchikey = compound_id,
          formula = MOLECULAR_FORMULA,
          exactmass,
          synonyms,
          smiles = SMILES
        )

      log_debug("... negative spectra")
      spectra_extracted_neg <- spectra_extracted_neg |>
        dplyr::rename(compound_id = FILENAME) |>
        dplyr::mutate(spectrum_id = dplyr::row_number())
      cmps_neg <- spectra_extracted_neg |>
        dplyr::mutate(
          exactmass = as.numeric(EXACTMASS),
          synonyms = NA_character_
        ) |>
        dplyr::select(
          compound_id = compound_id,
          name = compound_id,
          inchi = INCHI,
          ## Not ideal
          inchikey = compound_id,
          formula = MOLECULAR_FORMULA,
          exactmass,
          synonyms,
          smiles = SMILES
        )

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
          metad = metad
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
          metad = metad
        )
    }
    log_debug("... positive spectra")
    spectra_extracted_pos |>
      export_spectra(file = output_pos)

    log_debug("... negative spectra")
    spectra_extracted_neg |>
      export_spectra(file = output_neg)
  }
