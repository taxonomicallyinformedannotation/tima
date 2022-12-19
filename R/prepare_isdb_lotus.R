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
#' @importFrom tidyr drop_na
#'
#' @examples TODO
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
      create_dir(export = output_pos)
      if (nrow(spectra_harmonized_pos |>
        tidyr::drop_na(compound_id)) != 0) {
        spectra_harmonized_pos |>
          export_spectra(
            file = output_pos,
            metad = metad
          )
      }
    } else {
      log_debug(
        "There is already a positive library with the same name existing, to avoid any conflict please remove it."
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
      create_dir(export = output_neg)
      if (nrow(spectra_harmonized_neg |>
        tidyr::drop_na(compound_id)) != 0) {
        spectra_harmonized_neg |>
          export_spectra(
            file = output_neg,
            metad = metad
          )
      }
    } else {
      log_debug(
        "There is already a negative library with the same name existing, to avoid any conflict please remove it."
      )
    }
  }
