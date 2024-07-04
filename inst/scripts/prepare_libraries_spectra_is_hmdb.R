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
#' @examples NULL
prepare_isdb_hmdb <-
  function(input = paths$data$source$libraries$spectra$is$hmdb,
           metadata = paths$data$interim$libraries$hmdb_minimal,
           output_pos = paths$data$interim$libraries$spectra$is$hmdb$pos,
           output_neg = paths$data$interim$libraries$spectra$is$hmdb$neg) {
    log_debug("Loading standardization function (temp)")
    source(file = "inst/scripts/standardize.R")

    log_debug("Loading proton mass")
    proton <- MetaboCoreUtils::calculateMass("H") - 5.485799E-4

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
      tidytable::left_join(
        y = df_meta,
        by = c("compound_id" = "accession")
      ) |>
      tidytable::filter(!is.na(smiles)) |>
      tidytable::select(-original_spectrum_id, -spectrum_id) |>
      tidytable::distinct() |>
      tidytable::mutate(
        precursorMz = ifelse(
          test = polarity == 1,
          yes = monisotopic_molecular_weight + proton,
          no = monisotopic_molecular_weight - proton
        ),
        ionmode = ifelse(
          test = polarity == 1,
          yes = "POSITIVE",
          no = "NEGATIVE"
        ),
        precursorCharge = ifelse(
          test = polarity == 1,
          yes = 1L,
          no = -1L
        )
      )

    log_debug("Standardizing chemical structures without stereo")
    smiles <- unique(spctra_enhanced$smiles)
    df_clean <- lapply(X = smiles, FUN = standardize_smiles) |>
      tidytable::bind_rows()
    spctra_enhanced <- spctra_enhanced |>
      tidytable::left_join(df_clean)

    col_args <- list(
      co_ce = "collision_energy",
      co_ci = "compound_id",
      co_em = "monisotopic_molecular_weight",
      co_mf = "chemical_formula",
      co_in = "inchi",
      co_io = "inchi_no_stereo",
      co_ik = "inchikey",
      co_il = "inchikey_no_stereo",
      co_po = "ionmode",
      co_na = "name",
      co_sm = "smiles",
      co_sn = "smiles_no_stereo",
      co_si = NULL,
      co_sp = "splash",
      co_sy = "iupac_name",
      co_xl = NULL
    )
    log_debug("Positive")
    spectra_harmonized_pos <-
      do.call(
        what = harmonize_spectra,
        args = c(col_args,
          spectra = spectra_enhanced,
          mode = "pos"
        )
      )

    log_debug("Negative")
    spectra_harmonized_neg <-
      do.call(
        what = harmonize_spectra,
        args = c(col_args,
          spectra = spectra_enhanced,
          mode = "neg"
        )
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
