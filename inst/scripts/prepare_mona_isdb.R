start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("Prepares spectra from MONA (MassBank of North America) \n"),
  crayon::red("They have to be previously downloaded manually. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

paths <- parse_yaml_paths()

#' @title Prepare MONA In Silico DataBase
#'
#' @param input TODO
#' @param output_pos TODO
#' @param output_neg TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom CompoundDb import_mona_sdf
#' @importFrom dplyr bind_rows filter left_join mutate
#' @importFrom MsBackendMgf MsBackendMgf
#' @importFrom Spectra export Spectra
#'
#' @examples TODO
prepare_mona_isdb <-
  function(input = paths$data$source$libraries$mona_lcmsms,
           output_pos = paths$data$interim$libraries$mona_isdb$pos,
           output_neg = paths$data$interim$libraries$mona_isdb$neg) {
    ## SDF import from zip not working
    log_debug("Loading spectra (might take long)")
    mona_data <- CompoundDb::import_mona_sdf(x = input, nonStop = TRUE)

    cpd <- mona_data$compound

    spctra <- mona_data$msms_spectrum

    log_debug("Adding metadata")
    spctra_enhanced <- spctra |>
      dplyr::left_join(cpd) |>
      dplyr::filter(spectrum_type == "MS2") |>
      ## COMMENT (AR): CURATING NON-STANDARD InChIs
      dplyr::mutate(inchi = gsub(
        pattern = "InChI=1/",
        replacement = "InChI=1S/",
        x = inchi,
        fixed = TRUE
      )) |>
      ## COMMENT (AR): Dropped the idea, too many things to do (mainly replace u by ?)
      ## COMMENT (AR): No way
      dplyr::mutate(inchi = gsub(
        pattern = " ",
        replacement = "",
        x = inchi,
        fixed = TRUE
      )) |>
      dplyr::filter(!is.na(smiles) | !is.na(inchi)) |>
      ## COMMENT (AR): FILTERING INVALID InChIs
      dplyr::filter(grepl(pattern = "InChI=1S/", x = inchi)) |>
      dplyr::filter(!grepl(
        pattern = ".",
        x = inchi,
        fixed = TRUE
      )) |>
      # dplyr::filter(!grepl(pattern = "Aux", x = inchi)) |>
      dplyr::mutate(ionmode = ifelse(
        test = polarity == 1,
        yes = "POSITIVE",
        no = "NEGATIVE"
      )) |>
      ## COMMENT (AR): FILTERING MISSING PRECURSOR MZS
      dplyr::filter(!is.na(precursor_mz))

    log_debug("Standardizing 2D chemical structures")
    inchis <- unique(spctra_enhanced$inchi)

    df_clean <- lapply(X = inchis, FUN = standardize_inchi) |>
      dplyr::bind_rows()

    spctra_enhanced <- spctra_enhanced |>
      dplyr::left_join(df_clean)

    log_debug("Cleaning charges")
    spctra_enhanced_1 <- spctra_enhanced |>
      dplyr::filter(
        grepl(pattern = "]\\+\\+", x = precursor_type) |
          grepl(pattern = "]2\\+", x = precursor_type)
      ) |>
      dplyr::mutate(precursorCharge = 2L)
    spctra_enhanced_2 <- spctra_enhanced |>
      dplyr::filter(
        grepl(pattern = "]\\+$", x = precursor_type) |
          grepl(pattern = "]1\\+", x = precursor_type) |
          grepl(pattern = "M\\+H$", x = precursor_type) |
          grepl(pattern = "M\\+K$", x = precursor_type) |
          grepl(pattern = "M\\+Na$", x = precursor_type) |
          grepl(pattern = "M\\+NH4$", x = precursor_type) |
          grepl(pattern = "M\\+$", x = precursor_type)
      ) |>
      dplyr::mutate(precursorCharge = 1L)
    spctra_enhanced_3 <- spctra_enhanced |>
      dplyr::filter(
        grepl(pattern = "]\\-\\-", x = precursor_type) |
          grepl(pattern = "]2\\-", x = precursor_type)
      ) |>
      dplyr::mutate(precursorCharge = -2L)
    spctra_enhanced_4 <- spctra_enhanced |>
      dplyr::filter(
        grepl(pattern = "]\\-$", x = precursor_type) |
          grepl(pattern = "]1\\-", x = precursor_type) |
          grepl(pattern = "M\\-H$", x = precursor_type) |
          grepl(pattern = "M\\+Cl$", x = precursor_type)
      ) |>
      dplyr::mutate(precursorCharge = -1L)

    spctra_enhanced_5 <- spctra_enhanced |>
      dplyr::anti_join(spctra_enhanced_1) |>
      dplyr::anti_join(spctra_enhanced_2) |>
      dplyr::anti_join(spctra_enhanced_3) |>
      dplyr::anti_join(spctra_enhanced_4) |>
      dplyr::mutate(diff = exactmass - precursor_mz) |>
      dplyr::mutate(precursorCharge = ifelse(
        test = ionmode == "POSITIVE",
        yes = 1L,
        no = -1L
      ))

    log_debug(
      nrow(spctra_enhanced_5),
      "spectra were not unambiguously attributed, trying the same by default base on ion type, might induce some errors"
    )

    spctra_enhanced_final <- dplyr::bind_rows(
      spctra_enhanced_1,
      spctra_enhanced_2,
      spctra_enhanced_3,
      spctra_enhanced_4,
      spctra_enhanced_5
    )

    spd <- data.frame(
      precursorMz = spctra_enhanced_final$precursor_mz,
      precursorCharge = spctra_enhanced_final$precursorCharge,
      FILENAME = spctra_enhanced_final$inchikey_2D,
      MOLECULAR_FORMULA = spctra_enhanced_final$formula,
      SEQ = "*..*",
      IONMODE = spctra_enhanced_final$ionmode,
      EXACTMASS = spctra_enhanced_final$exactmass,
      NAME = spctra_enhanced_final$inchikey_2D,
      SMILES = spctra_enhanced_final$smiles_2D,
      INCHI = spctra_enhanced_final$inchi_2D,
      # LIBRARYQUALITY = "PREDICTED",
      # SPLASH = spctra_enhanced_final$splash,
      COLLISION_ENERGY = spctra_enhanced_final$collision_energy
      # msLevel = 2L
    )

    spd$mz <- spctra_enhanced_final$mz
    spd$intensity <- spctra_enhanced_final$intensity

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
  }

prepare_mona_isdb()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
