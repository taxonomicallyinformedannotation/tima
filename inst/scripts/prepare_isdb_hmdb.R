start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

paths <- parse_yaml_paths()

log_debug(
  "This script",
  crayon::green("Converts the predicted spectra from HMDB \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

#' @title Prepare HMDB In Silico DataBase
#'
#' @param input Input file
#' @param metadata Metadata
#' @param output_pos Output for positive spectra
#' @param output_neg Output for negative spectra
#' @param export_sqlite Boolean. Save as sqlite instead of mgf
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom CompoundDb msms_spectra_hmdb
#' @importFrom dplyr distinct filter left_join mutate pull select
#' @importFrom MsBackendMgf MsBackendMgf
#' @importFrom readr read_tsv
#' @importFrom Spectra export Spectra
#'
#' @examples NULL
prepare_isdb_hmdb <-
  function(input = paths$data$source$spectra$hmdb_isdb,
           metadata = paths$data$interim$libraries$hmdb_minimal,
           output_pos = paths$data$interim$spectra$hmdb$pos,
           output_neg = paths$data$interim$spectra$hmdb$neg,
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
    colnames_hmdb <- c(
      colname_collision_energy = "collision_energy",
      colname_compound_id = NA,
      colname_exact_mass = "monisotopic_molecular_weight",
      colname_formula = "chemical_formula",
      colname_inchi = "inchi_2D",
      colname_inchikey = "inchikey_2D",
      colname_mode = "ionmode",
      colname_name = "inchikey_2D",
      colname_precursorMz = "precursor_mz",
      colname_precursorCharge = "precursor_charge",
      colname_smiles = "smiles_2D",
      colname_spectrum_id = NA,
      colname_splash = "splash",
      colname_synonyms = NA
    )

    log_debug("Positive")
    spectra_harmonized_pos <- spctra_enhanced |>
      harmonize_spectra(
        colnames = colnames_hmdb,
        mode = "pos"
      )

    log_debug("Negative")
    spectra_harmonized_neg <- spctra_enhanced |>
      harmonize_spectra(
        colnames = colnames_hmdb,
        mode = "neg"
      )

    log_debug("Exporting")
    export_spectra_2(
      file = output_pos,
      spectra = spectra_harmonized_pos,
      meta = NULL
    )
    export_spectra_2(
      file = output_neg,
      spectra = spectra_harmonized_neg,
      meta = NULL
    )

    log_debug("Deleting unzipped directory")
    unlink(newdir, recursive = TRUE)
  }

prepare_isdb_hmdb()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
