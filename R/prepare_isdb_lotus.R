#' @title Prepare LOTUS In Silico DataBase
#'
#' @description This function prepares the LOTUS in silico generated spectra to be used for spectral matching
#'
#' @param input File containing LOTUS ISDB
#' @param output Output file
#' @param polarity MS polarity
#' @param export_sqlite Boolean. Export to SQLite? TRUE or FALSE
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom CompoundDb make_metadata
#' @importFrom dplyr filter
#' @importFrom jsonlite fromJSON
#'
#' @examples NULL
prepare_isdb_lotus <-
  function(input = paths$data$source$spectra$lotus$pos,
           output = paths$data$interim$spectra$lotus$pos,
           polarity = "pos",
           export_sqlite = TRUE) {
    if (export_sqlite == TRUE) {
      output <- output |>
        gsub(
          pattern = ".mgf",
          replacement = ".sqlite",
          fixed = TRUE
        )
    }

    if (!file.exists(output)) {
      log_debug("Generating metadata ...")
      content <- jsonlite::fromJSON(txt = "https://zenodo.org/api/records/5607185")
      metad <- CompoundDb::make_metadata(
        source = "LOTUS",
        url = "https://doi.org/10.5281/zenodo.5607185",
        source_version = content$doi_url,
        source_date = content[["metadata"]][["publication_date"]],
        organism = "Life"
      )
    }

    if (!file.exists(output)) {
      log_debug("Importing")
      spectra <- input |>
        import_spectra()

      log_debug("Harmonizing")
      spectra_harmonized <- spectra |>
        extract_spectra() |>
        harmonize_spectra(mode = polarity)

      log_debug("Exporting")
      export_spectra_2(
        file = output,
        spectra = spectra_harmonized,
        meta = metad
      )
    }

    return(output)
  }
