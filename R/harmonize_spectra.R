#' @title Harmonize spectra metadata
#'
#' @description Harmonizes spectra header fields by mapping library-specific
#'     column names to standardized field names. Handles different naming
#'     conventions across spectral libraries.
#'
#' @include validations_utils.R
#' @include logs_utils.R
#' @include process_smiles.R
#' @include predicates_utils.R
#' @include validations_params.R
#'
#' @param spectra Data frame containing spectra to harmonize
#' @param metad Metadata identifying the library source
#' @param mode MS ionization mode (must contain 'pos' or 'neg')
#' @param col_ad Adduct field name
#' @param col_ce Collision energy field name
#' @param col_ci Compound ID field name
#' @param col_in InChI field name
#' @param col_io InChI without stereochemistry field name
#' @param col_ik InChIKey field name
#' @param col_il InChIKey connectivity layer field name
#' @param col_na Compound name field name
#' @param col_po Polarity field name
#' @param col_sm SMILES field name
#' @param col_sn SMILES without stereochemistry field name
#' @param col_si Spectrum ID field name
#' @param col_sp SPLASH field name
#' @param col_sy Synonyms field name
#'
#' @return Data frame with harmonized column names and standardized fields
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' harmonized <- harmonize_spectra(
#'   spectra = raw_spectra,
#'   metad = "GNPS",
#'   mode = "positive",
#'   col_na = "Compound_Name",
#'   col_sm = "SMILES",
#'   col_ik = "InChIKey"
#'   # ... other column mappings
#' )
#' }
harmonize_spectra <- function(
  spectra,
  metad,
  mode,
  col_ad,
  col_ce,
  col_ci,
  col_in,
  col_io,
  col_ik,
  col_il,
  col_na,
  col_po,
  col_sm,
  col_sn,
  col_si,
  col_sp,
  col_sy
) {
  ctx <- log_operation(
    "harmonize_spectra",
    n_spectra = nrow(spectra),
    mode = mode
  )

  validate_dataframe(spectra, param_name = "spectra")

  # Validate mode contains 'pos' or 'neg'
  if (!grepl("pos|neg", mode, ignore.case = TRUE)) {
    cli::cli_abort(
      c(
        "mode must contain 'pos' or 'neg'",
        "x" = as.character(mode)
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate all column parameters are character strings or NULL
  validate_column_mappings(
    list(
      col_ad = col_ad,
      col_ce = col_ce,
      col_ci = col_ci,
      col_in = col_in,
      col_io = col_io,
      col_ik = col_ik,
      col_il = col_il,
      col_na = col_na,
      col_po = col_po,
      col_sm = col_sm,
      col_sn = col_sn,
      col_si = col_si,
      col_sp = col_sp,
      col_sy = col_sy
    )
  )

  columns <- c(
    "adduct",
    "collision_energy",
    "compound_id",
    "inchi",
    "inchi_no_stereo",
    "inchikey",
    "inchikey_connectivity_layer",
    "name",
    "smiles",
    "smiles_no_stereo",
    "spectrum_id",
    "splash",
    "synonyms"
  )

  # Map provided column names to standard names
  columns_full <- c(
    "adduct" = col_ad,
    "collision_energy" = col_ce,
    "compound_id" = col_ci,
    "inchi" = col_in,
    "inchi_no_stereo" = col_io,
    "inchikey" = col_ik,
    "inchikey_connectivity_layer" = col_il,
    "name" = col_na,
    "smiles" = col_sm,
    "smiles_no_stereo" = col_sn,
    "spectrum_id" = col_si,
    "splash" = col_sp,
    "synonyms" = col_sy
  )

  columns_full <- columns_full[!is.na(columns_full)]

  # Identify missing columns that need to be added
  columns_missing <- columns[!columns %in% names(columns_full)]

  is_pos <- grepl("pos", mode, ignore.case = TRUE)
  default_charge <- if (is_pos) 1L else -1L
  target_num_po <- if (is_pos) 1 else -1

  # Filter polarity on unique values of col_po before copying list columns
  if (col_po %in% names(spectra)) {
    po_vec <- spectra[[col_po]]
    u_po <- unique(po_vec)
    u_keep <- grepl(pattern = mode, x = u_po, ignore.case = TRUE) |
      (suppressWarnings(as.numeric(u_po)) %in% target_num_po)
    if (all(u_keep)) {
      keep_idx <- seq_len(nrow(spectra))
    } else if (!any(u_keep)) {
      keep_idx <- integer(0L)
    } else {
      m_po <- if (anyNA(u_po)) {
        match(po_vec, u_po)
      } else {
        fastmatch::fmatch(po_vec, u_po)
      }
      keep_idx <- which(u_keep[m_po])
    }
  } else {
    keep_idx <- integer(0L)
  }

  n_keep <- length(keep_idx)
  if (n_keep == 0L && length(columns_missing) > 0L) {
    names(columns_missing) <- columns_missing
    spectra_missing <- columns_missing |>
      data.frame() |>
      tidytable::as_tidytable() |>
      tidytable::bind_cols(tidytable::tidytable(value = NA_character_)) |>
      tidytable::pivot_wider(names_from = columns_missing) |>
      tidytable::mutate(join = "x")

    if (!"precursorCharge" %in% names(spectra)) {
      spectra$precursorCharge <- NA_integer_
    }

    spectra_filtered <- spectra |>
      data.frame(check.names = FALSE) |>
      tidytable::as_tidytable()

    for (.col in intersect(c("mz", "intensity"), names(spectra_filtered))) {
      if (!is.list(spectra_filtered[[.col]])) {
        spectra_filtered[[.col]] <- as.list(spectra_filtered[[.col]])
      }
    }

    spectra_filtered <- spectra_filtered |>
      tidytable::filter(
        grepl(
          pattern = mode,
          x = !!as.name(col_po),
          ignore.case = TRUE
        ) |
          suppressWarnings(as.numeric(!!as.name(col_po))) %in% target_num_po
      ) |>
      tidytable::select(
        tidyselect::any_of(x = columns_full),
        tidyselect::any_of(x = "precursorCharge"),
        precursorMz,
        tidyselect::any_of(x = "rtime"),
        mz,
        intensity
      ) |>
      tidytable::mutate(
        join = "x",
        precursorCharge = tidytable::coalesce(precursorCharge, default_charge)
      )

    spectra_harmonized <- spectra_filtered |>
      tidytable::full_join(y = spectra_missing) |>
      tidytable::select(tidyselect::any_of(
        x = c(
          "adduct",
          "collision_energy",
          "compound_id",
          "inchi",
          "inchi_no_stereo",
          "inchikey",
          "inchikey_connectivity_layer",
          "name",
          "precursorMz",
          "precursorCharge",
          "smiles",
          "smiles_no_stereo",
          "spectrum_id",
          "splash",
          "synonyms",
          "rtime",
          "mz",
          "intensity"
        )
      )) |>
      tidytable::mutate(
        library = metad,
        spectrum_id = tidytable::if_else(
          condition = is.na(spectrum_id),
          true = tidytable::row_number() |>
            as.character(),
          false = as.character(spectrum_id)
        ),
        compound_id = tidytable::if_else(
          condition = is.na(compound_id),
          true = name |>
            as.character(),
          false = compound_id |>
            as.character()
        )
      ) |>
      data.frame(check.names = FALSE)

    for (.col in intersect(c("mz", "intensity"), names(spectra_harmonized))) {
      if (!is.list(spectra_harmonized[[.col]])) {
        spectra_harmonized[[.col]] <- as.list(spectra_harmonized[[.col]])
      }
    }
    rm(spectra_filtered)
  } else {
    target_order <- c(
      "adduct",
      "collision_energy",
      "compound_id",
      "inchi",
      "inchi_no_stereo",
      "inchikey",
      "inchikey_connectivity_layer",
      "name",
      "precursorMz",
      "precursorCharge",
      "smiles",
      "smiles_no_stereo",
      "spectrum_id",
      "splash",
      "synonyms",
      if ("rtime" %in% names(spectra)) "rtime",
      "mz",
      "intensity",
      "library"
    )
    out_cols <- vector("list", length(target_order))
    names(out_cols) <- target_order

    for (std_nm in names(columns_full)) {
      src_nm <- columns_full[[std_nm]]
      if (src_nm %in% names(spectra)) {
        out_cols[[std_nm]] <- spectra[[src_nm]][keep_idx]
      } else {
        out_cols[[std_nm]] <- rep(NA_character_, n_keep)
      }
    }
    for (miss_nm in columns_missing) {
      out_cols[[miss_nm]] <- rep(NA_character_, n_keep)
    }

    out_cols[["precursorMz"]] <- spectra[["precursorMz"]][keep_idx]
    if ("precursorCharge" %in% names(spectra)) {
      pc <- spectra[["precursorCharge"]][keep_idx]
      na_pc <- is.na(pc)
      if (any(na_pc)) {
        pc[na_pc] <- default_charge
      }
      out_cols[["precursorCharge"]] <- pc
    } else {
      out_cols[["precursorCharge"]] <- rep(default_charge, n_keep)
    }

    if ("rtime" %in% names(spectra)) {
      out_cols[["rtime"]] <- spectra[["rtime"]][keep_idx]
    }
    for (.col in c("mz", "intensity")) {
      col_val <- spectra[[.col]][keep_idx]
      if (!is.list(col_val)) {
        col_val <- as.list(col_val)
      }
      out_cols[[.col]] <- col_val
    }

    out_cols[["library"]] <- rep(metad, n_keep)

    sid <- out_cols[["spectrum_id"]]
    if (all(is.na(sid))) {
      out_cols[["spectrum_id"]] <- as.character(seq_len(n_keep))
    } else {
      sid_chr <- as.character(sid)
      na_sid <- is.na(sid_chr)
      if (any(na_sid)) {
        sid_chr[na_sid] <- as.character(which(na_sid))
      }
      out_cols[["spectrum_id"]] <- sid_chr
    }

    cid <- out_cols[["compound_id"]]
    nm_chr <- as.character(out_cols[["name"]])
    if (all(is.na(cid))) {
      out_cols[["compound_id"]] <- nm_chr
    } else {
      cid_chr <- as.character(cid)
      na_cid <- is.na(cid_chr)
      if (any(na_cid)) {
        cid_chr[na_cid] <- nm_chr[na_cid]
      }
      out_cols[["compound_id"]] <- cid_chr
    }

    spectra_harmonized <- tidytable::as_tidytable(out_cols)
  }

  smiles_harmonized <- spectra_harmonized |>
    process_smiles(smiles_colname = "smiles")
  colnames(smiles_harmonized) <- c(
    "smiles_old",
    colnames(smiles_harmonized)[-1L] |>
      gsub(pattern = "structure_", replacement = "", fixed = TRUE)
  )
  smiles_harmonized <- smiles_harmonized |>
    tidytable::rename(smiles = smiles_old, smiles_new = smiles)

  if (
    n_keep > 0L &&
      nrow(smiles_harmonized) > 0L &&
      !anyDuplicated(smiles_harmonized$smiles)
  ) {
    sm_vec <- spectra_harmonized$smiles
    u_sm <- smiles_harmonized$smiles
    m_sm <- if (anyNA(u_sm) || anyNA(sm_vec)) {
      match(sm_vec, u_sm)
    } else {
      fastmatch::fmatch(sm_vec, u_sm)
    }
    valid_m <- which(!is.na(m_sm))
    ord <- order(m_sm[valid_m], method = "radix")
    row_idx <- valid_m[ord]
    sm_idx <- m_sm[row_idx]

    drop_cols <- c(colnames(smiles_harmonized)[-1L], "smiles")
    keep_base_cols <- setdiff(names(spectra_harmonized), drop_cols)
    base_list <- lapply(
      as.list(spectra_harmonized)[keep_base_cols],
      `[`,
      row_idx
    )
    sm_cols <- setdiff(colnames(smiles_harmonized), "smiles")
    sm_list <- lapply(as.list(smiles_harmonized)[sm_cols], `[`, sm_idx)
    names(sm_list)[names(sm_list) == "smiles_new"] <- "smiles"
    spectra_harmonized <- tidytable::as_tidytable(c(base_list, sm_list))
  } else {
    spectra_harmonized <- spectra_harmonized |>
      tidytable::select(
        -tidyselect::any_of(colnames(smiles_harmonized)[-1L])
      ) |>
      tidytable::inner_join(smiles_harmonized) |>
      tidytable::select(-smiles) |>
      tidytable::rename(smiles = smiles_new)
  }
  rm(smiles_harmonized)

  log_complete(ctx, n_harmonized = nrow(spectra_harmonized))

  spectra_harmonized
}


#' Validate column mapping parameters
#' @keywords internal
validate_column_mappings <- function(col_params) {
  validate_all_with_predicate(
    x_list = col_params,
    predicate_fn = is_null_or_single_string,
    error_prefix = "Column name parameter(s)"
  )
}
