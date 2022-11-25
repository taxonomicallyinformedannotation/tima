start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("Converts the predicted spectra from HMDB \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

paths <- parse_yaml_paths()

#' @title Convert HMDB In Silico DataBase
#'
#' @param input TODO
#' @param metadata TODO
#' @param output TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom dplyr left_join mutate
#' @importFrom MsBackendMgf MsBackendMgf
#' @importFrom readr read_lines read_tsv write_tsv
#' @importFrom Spectra export Spectra
#' @importFrom tibble enframe
#'
#' @examples TODO
convert_hmdb_isdb <-
  function(input = paths$data$source$libraries$hmdb_isdb,
           metadata = paths$data$interim$libraries$hmdb_minimal,
           output = paths$data$interim$libraries$hmdb_isdb) {
    proton <- 1.007276

    df_meta <- readr::read_tsv(metadata)

    files <- unzip(zipfile = input, list = TRUE)

    # files <- files |>
    #   dplyr::sample_n(100)

    ## COMMENT (AR): Quite long
    list <-
      lapply(
        X = files$Name,
        FUN = function(x) {
          readr::read_lines(file = unz(description = input, filename = x))
        }
      )

    names(list) <- files$Name |>
      gsub(pattern = "_.*", replacement = "")

    df_peaks <- list |>
      tibble::enframe() |>
      dplyr::left_join(
        y = df_meta |>
          dplyr::mutate(inchikey_2D = gsub(
            pattern = "-.*",
            replacement = "",
            x = inchikey
          )),
        by = c("name" = "accession")
      )

    spd <- data.frame(
      ## Check if positive
      precursorMz = df_peaks$monisotopic_molecular_weight + proton,
      precursorCharge = 1L,
      FILENAME = df_peaks$inchikey_2D,
      MOLECULAR_FORMULA = df_peaks$chemical_formula,
      SEQ = "*..*",
      IONMODE = "POSITIVE",
      EXACTMASS = df_peaks$monisotopic_molecular_weight,
      NAME = df_peaks$inchikey_2D,
      SMILES = df_peaks$smiles,
      INCHI = df_peaks$inchi,
      ## Check if positive
      LIBRARYQUALITY = "In-silico ESI-MS/MS [M+H]+ Spectra PREDICTED BY CFM-ID X.X.X"
      # msLevel = 2L
    )

    spd$mz <- lapply(
      X = df_peaks$value,
      FUN = function(x) {
        gsub(
          pattern = " .*",
          replacement = "",
          x = x
        ) |>
          as.numeric()
      }
    )

    spd$intensity <- lapply(
      X = df_peaks$value,
      FUN = function(x) {
        gsub(
          pattern = ".* ",
          replacement = "",
          x = x
        ) |>
          as.numeric()
      }
    )

    Spectra::Spectra(object = spd) |>
      Spectra::export(backend = MsBackendMgf::MsBackendMgf(), file = output)
  }

convert_hmdb_isdb()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
