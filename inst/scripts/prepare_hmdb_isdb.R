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

#' @title Prepare HMDB In Silico DataBase
#'
#' @param input TODO
#' @param metadata TODO
#' @param output_pos TODO
#' @param output_neg TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom CompoundDb msms_spectra_hmdb
#' @importFrom dplyr distinct filter left_join mutate pull select
#' @importFrom MsBackendMgf MsBackendMgf
#' @importFrom readr read_tsv
#' @importFrom Spectra export Spectra
#'
#' @examples TODO
prepare_hmdb_isdb <-
  function(input = paths$data$source$libraries$hmdb_isdb,
           metadata = paths$data$interim$libraries$hmdb_minimal,
           output_pos = paths$data$interim$libraries$hmdb_isdb$pos,
           output_neg = paths$data$interim$libraries$hmdb_isdb$neg) {
    log_debug("Loading standardization function (temp)")
    source(file = "inst/scripts/standardize.R")

    log_debug("Loading proton mass")
    proton <- readr::read_tsv(file = paths$inst$extdata$adducts) |>
      dplyr::filter(adduct == "H (proton)") |>
      dplyr::pull("mass")

    log_debug("Loading metadata")
    df_meta <- readr::read_tsv(file = metadata)

    log_debug("Unzipping")
    newdir <-
      gsub(
        pattern = ".zip",
        replacement = "",
        x = input,
        fixed = TRUE
      )
    unzip(zipfile = input, exdir = newdir)

    log_debug("Loading spectra (might take long)")
    spctra <- CompoundDb::msms_spectra_hmdb(x = newdir)

    log_debug("Adding metadata")
    spctra_enhanced <- spctra |>
      dplyr::left_join(
        y = df_meta,
        by = c("compound_id" = "accession")
      ) |>
      dplyr::filter(!is.na(smiles)) |>
      dplyr::select(-original_spectrum_id, -spectrum_id) |>
      dplyr::distinct() |>
      dplyr::mutate(
        precursor_mz = ifelse(
          test = polarity == 1,
          yes = monisotopic_molecular_weight + proton,
          no = monisotopic_molecular_weight - proton
        ),
        ionmode = ifelse(
          test = polarity == 1,
          yes = "POSITIVE",
          no = "NEGATIVE"
        ),
        precursor_charge = ifelse(
          test = polarity == 1,
          yes = 1L,
          no = -1L
        )
      )

    log_debug("Standardizing 2D chemical structures")
    smiles <- unique(spctra_enhanced$smiles)
    df_clean <- lapply(X = smiles, FUN = standardize_smiles) |>
      dplyr::bind_rows()
    spctra_enhanced <- spctra_enhanced |>
      dplyr::left_join(df_clean)

    log_debug("Formatting")
    spd <- data.frame(
      precursorMz = spctra_enhanced$precursor_mz,
      precursorCharge = spctra_enhanced$precursor_charge,
      FILENAME = spctra_enhanced$inchikey_2D,
      MOLECULAR_FORMULA = spctra_enhanced$chemical_formula,
      SEQ = "*..*",
      IONMODE = spctra_enhanced$ionmode,
      EXACTMASS = spctra_enhanced$monisotopic_molecular_weight,
      NAME = spctra_enhanced$inchikey_2D,
      SMILES = spctra_enhanced$smiles_2D,
      INCHI = spctra_enhanced$inchi_2D,
      LIBRARYQUALITY = "PREDICTED",
      SPLASH = spctra_enhanced$splash,
      COLLISION_ENERGY = spctra_enhanced$collision_energy
      # msLevel = 2L
    )

    spd$mz <- spctra_enhanced$mz
    spd$intensity <- spctra_enhanced$intensity

    log_debug("Filtering positive")
    spd_pos <- spd |>
      dplyr::filter(IONMODE == "POSITIVE")

    log_debug("Filtering negative")
    spd_neg <- spd |>
      dplyr::filter(IONMODE == "NEGATIVE")

    log_debug("Exporting")
    sps_pos <- Spectra::Spectra(object = spd_pos) |>
      Spectra::export(backend = MsBackendMgf::MsBackendMgf(), file = output_pos)

    sps_neg <- Spectra::Spectra(object = spd_neg) |>
      Spectra::export(backend = MsBackendMgf::MsBackendMgf(), file = output_neg)

    log_debug("Deleting unzipped directory")
    unlink(newdir, recursive = TRUE)
  }

prepare_hmdb_isdb()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
