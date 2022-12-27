#' @title Prepare LOTUS In Silico DataBase
#'
#' @description This function prepares the LOTUS in silico generated spectra to be used for spectral matching
#'
#' @param input_pos File containing positive ionization LOTUS ISDB
#' @param input_neg File containing negative ionization LOTUS ISDB
#' @param output_pos Output for positive mode
#' @param output_neg Output for negative mode
#' @param export_sqlite Boolean. Export to SQLite? TRUE or FALSE
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom CompoundDb make_metadata
#' @importFrom curl curl_fetch_memory
#' @importFrom dplyr mutate rename select
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr drop_na
#'
#' @examples NULL
prepare_isdb_lotus <-
  function(input_pos = paths$data$source$spectra$lotus$pos,
           input_neg = paths$data$source$spectra$lotus$neg,
           output_pos = paths$data$interim$spectra$lotus$pos,
           output_neg = paths$data$interim$spectra$lotus$neg,
           export_sqlite = TRUE) {
    if (export_sqlite == TRUE) {
      output_pos <- output_pos |>
        gsub(
          pattern = ".mgf",
          replacement = ".sqlite",
          fixed = TRUE
        )
      output_neg <- output_neg |>
        gsub(
          pattern = ".mgf",
          replacement = ".sqlite",
          fixed = TRUE
        )
    }

    if (!file.exists(output_pos) ||
      !file.exists(output_neg)) {
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
    }

    if (!file.exists(output_pos)) {
      log_debug("Positive mode")
      log_debug("Importing")
      spectra_pos <- input_pos |>
        import_spectra()

      log_debug("Harmonizing")
      spectra_harmonized_pos <- spectra_pos |>
        extract_spectra() |>
        harmonize_spectra()

      log_debug("Exporting")
      export_spectra_2(
        file = output_pos,
        spectra = spectra_harmonized_pos,
        meta = metad
      )
    }
    if (!file.exists(output_neg)) {
      log_debug("Negative mode")
      log_debug("Importing")
      spectra_neg <- input_neg |>
        import_spectra()

      log_debug("Harmonizing")
      spectra_harmonized_neg <- spectra_neg |>
        extract_spectra() |>
        harmonize_spectra(mode = "neg")

      log_debug("Exporting")
      export_spectra_2(
        file = output_neg,
        spectra = spectra_harmonized_neg,
        meta = metad
      )
    }
  }
