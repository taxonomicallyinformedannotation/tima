#' @title Sanitize spectra benchmark
#'
#' @description This function sanitizes the benchmark spectra
#'
#' @details Because they are still quite dirty
#'
#' @include extract_spectra.R
#' @include harmonize_spectra.R
#' @include normalize_peaks.R
#' @include remove_above_precursor.R
#'
#' @param sp Spectra
#' @param mgf_pos_path Path to store the positive spectra
#' @param mgf_neg_path Path to store the negative spectra
#' @param meta_pos_path Path to store the positive metadata
#' @param meta_neg_path Path to store the negative metadata
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
sanitize_spectra_benchmark <-
  function(sp,
           mgf_pos_path,
           mgf_neg_path,
           meta_pos_path = "data/interim/benchmark/benchmark_meta_pos.tsv",
           meta_neg_path = "data/interim/benchmark/benchmark_meta_neg.tsv") {
    sp$precursorMz <- as.numeric(sp$PRECURSOR_MZ)
    sp$precursorCharge <- as.integer(sp$CHARGE)
    sp$acquisitionNum <- as.integer(sp$SCANS)
    sp$scanIndex <- as.integer(sp$SCANS)

    sp_clean <- sp |>
      Spectra::addProcessing(remove_above_precursor(),
        spectraVariables = c("precursorMz")
      ) |>
      Spectra::addProcessing(normalize_peaks()) |>
      Spectra::applyProcessing()

    adduct <- sp_clean$ADDUCT
    inchikey <- sp_clean$inchikey
    instrument <- sp_clean$SOURCE_INSTRUMENT
    # fragments <- sp_clean$NUM.PEAKS
    fragments <- lapply(sp_clean@backend@peaksData, length) |>
      as.character() |>
      as.numeric() / 2
    # pepmass <- gsub("\\[|\\]", "", sp_clean$PARENT_MASS)
    pepmass <- sp_clean$PEPMASS
    smiles <- sp_clean$smiles
    ccmslib <- sp_clean$SPECTRUMID
    charge <- sp_clean$precursorCharge

    df_meta <- tidytable::tidytable(
      adduct,
      inchikey,
      instrument,
      fragments,
      pepmass,
      smiles,
      ccmslib,
      charge
    ) |>
      tidyft::mutate_vars(
        is.character,
        .func = function(x) {
          tidytable::na_if(x, "")
        }
      )

    df_clean <- df_meta |>
      dplyr::filter(!is.na(inchikey)) |>
      dplyr::filter(fragments > 5) |>
      dplyr::filter(fragments <= 1000) |>
      dplyr::filter(!grepl(
        pattern = "QQQ",
        x = instrument,
        fixed = TRUE
      )) |>
      dplyr::mutate(mass = pepmass) |>
      tidyr::separate(
        col = mass,
        sep = "\\.",
        into = c("a", "b")
      ) |>
      dplyr::filter(!is.na(b)) |>
      dplyr::filter(stringr::str_length(b) > 1) |>
      dplyr::select(-a, -b) |>
      dplyr::mutate(inchikey_2D = gsub(
        pattern = "-.*",
        replacement = "",
        x = inchikey
      )) |>
      dplyr::distinct(inchikey_2D, adduct, .keep_all = TRUE) |>
      dplyr::mutate(mz = pepmass) |>
      dplyr::group_by(inchikey_2D) |>
      ## Weird way to have some kind of retention time
      dplyr::mutate(rt = dplyr::cur_group_id()) |>
      dplyr::ungroup()

    df_clean_neg <- df_clean |>
      dplyr::filter(grepl(
        pattern = "-",
        x = charge,
        fixed = TRUE
      )) |>
      dplyr::mutate(feature_id = dplyr::row_number())

    df_clean_pos <- df_clean |>
      dplyr::anti_join(df_clean_neg) |>
      dplyr::mutate(feature_id = dplyr::row_number())

    sp_pos <- sp_clean[sp_clean$SPECTRUMID %in% df_clean_pos$ccmslib]
    sp_neg <- sp_clean[sp_clean$SPECTRUMID %in% df_clean_neg$ccmslib]
    sp_pos$feature_id <- df_clean_pos$feature_id
    sp_neg$feature_id <- df_clean_neg$feature_id
    sp_pos$SCANS <- df_clean_pos$feature_id
    sp_neg$SCANS <- df_clean_neg$feature_id

    spectra_harmonized_pos <- sp_pos |>
      extract_spectra() |>
      dplyr::mutate(polarity = "pos") |>
      harmonize_spectra(
        col_ce = NA,
        col_ci = NA,
        col_em = "PARENT_MASS",
        col_in = "inchi",
        col_io = NA,
        col_ik = "inchikey",
        col_il = NA,
        # col_mf = "formula",
        col_mf = NA,
        col_na = "name",
        col_po = "polarity",
        col_sm = "smiles",
        col_sn = NA,
        col_si = "SPECTRUMID",
        col_sp = NA,
        col_sy = NA,
        col_xl = NA,
        mode = "pos"
      )

    spectra_harmonized_neg <- sp_neg |>
      extract_spectra() |>
      dplyr::mutate(polarity = "neg") |>
      harmonize_spectra(
        col_ce = NA,
        col_ci = NA,
        col_em = "PARENT_MASS",
        col_in = "inchi",
        col_io = NA,
        col_ik = "inchikey",
        col_il = NA,
        # col_mf = "formula",
        col_mf = NA,
        col_na = "name",
        col_po = "polarity",
        col_sm = "smiles",
        col_sn = NA,
        col_si = "SPECTRUMID",
        col_sp = NA,
        col_sy = NA,
        col_xl = NA,
        mode = "neg"
      )

    log_debug("Exporting")
    spectra_harmonized_pos |>
      Spectra::Spectra() |>
      Spectra::export(
        backend = MsBackendMgf::MsBackendMgf(),
        file = mgf_pos_path
      )
    spectra_harmonized_neg |>
      Spectra::Spectra() |>
      Spectra::export(
        backend = MsBackendMgf::MsBackendMgf(),
        file = mgf_neg_path
      )
    df_clean_pos |>
      export_output(meta_pos_path)
    df_clean_neg |>
      export_output(meta_neg_path)

    return(
      c(
        "spectra_pos" = mgf_pos_path,
        "spectra_neg" = mgf_neg_path,
        "meta_pos" = meta_pos_path,
        "meta_neg" = meta_neg_path
      )
    )
  }
