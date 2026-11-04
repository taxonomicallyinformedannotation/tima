# ── Internal helpers ──

#' Expand summarized (pipe-separated) results to one row per candidate
#' @keywords internal
.mztab_expand_summarized <- function(results) {
  # Heuristic: if any candidate/rank/score column contains "|", the table was
  # written with summarize = TRUE. Expand to long form.
  multi_cols <- colnames(results)[grepl(
    "^candidate|^rank|^score",
    colnames(results),
    perl = TRUE
  )]
  multi_cols <- intersect(multi_cols, colnames(results))

  # Check if any cell contains pipe separator
  has_multivalue <- length(multi_cols) > 0L &&
    any(
      vapply(
        X = multi_cols,
        FUN = function(col) {
          any(
            grepl("|", as.character(results[[col]]), fixed = TRUE),
            na.rm = TRUE
          )
        },
        FUN.VALUE = logical(1L)
      )
    )
  if (!has_multivalue) {
    return(results)
  }

  # Work in plain data.frame for predictable behavior
  results <- as.data.frame(
    results,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  id_cols <- setdiff(colnames(results), multi_cols)
  n_rows <- nrow(results)

  # Split each multi-valued column once (C-backed strsplit)
  split_values <- lapply(multi_cols, function(col) {
    values <- as.character(results[[col]])
    parts <- strsplit(values, "|", fixed = TRUE)
    parts[is.na(values) | !nzchar(values)] <- list(NA_character_)
    parts
  })

  # Compute number of output rows per input row (elementwise max across cols)
  lengths_list <- lapply(split_values, lengths)
  if (length(lengths_list) == 0L) {
    return(results)
  } else if (length(lengths_list) == 1L) {
    n_vals_per_row <- pmax(unlist(lengths_list), 1L)
  } else {
    n_vals_per_row <- do.call(pmax, c(lengths_list, list(1L)))
  }
  n_vals_per_row[n_vals_per_row < 1L] <- 1L

  row_idx <- rep.int(seq_len(n_rows), n_vals_per_row)

  out <- vector("list", length(id_cols) + length(multi_cols))
  names(out) <- c(id_cols, multi_cols)

  # Expand id (non-multi) columns by simple indexing
  for (col in id_cols) {
    out[[col]] <- results[[col]][row_idx]
  }

  # Expand multi-value columns directly (only adjusting rows with mismatched length)
  for (col_idx in seq_along(multi_cols)) {
    parts <- split_values[[col_idx]]
    col_lens <- lengths_list[[col_idx]]
    mismatch <- which(col_lens != n_vals_per_row)
    if (length(mismatch) > 0L) {
      for (i in mismatch) {
        parts[[i]] <- parts[[i]][seq_len(n_vals_per_row[[i]])]
      }
    }
    out[[multi_cols[[col_idx]]]] <- unlist(parts, use.names = FALSE)
  }

  out_df <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  original_colnames <- colnames(results)
  # Safe rm() — some variables are only defined conditionally; use
  # intersect to avoid "object not found" warnings
  rm(
    list = intersect(
      c(
        "out",
        "results",
        "multi_cols",
        "id_cols",
        "split_values",
        "lengths_list",
        "n_vals_per_row",
        "row_idx",
        "col_idx",
        "parts",
        "col",
        "n_rows"
      ),
      ls()
    )
  )
  out_df[, original_colnames, drop = FALSE]
}

#' Build SMF (Small Molecule Feature) table – one row per feature_id
#' @keywords internal
.mztab_build_smf <- function(results) {
  results <- as.data.frame(
    results,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  feature_id_vals <- as.character(results$feature_id)
  valid_feature <- !is.na(feature_id_vals) &
    nzchar(feature_id_vals) &
    feature_id_vals != "null"
  feature_ids <- unique(feature_id_vals[valid_feature])

  if (length(feature_ids) == 0L) {
    return(data.frame(
      SMF_ID = character(0),
      SML_ID_REFS = character(0),
      SME_ID_REFS = character(0),
      SME_ID_REF_ambiguity_code = character(0),
      exp_mass_to_charge = character(0),
      charge = character(0),
      retention_time_in_seconds = character(0),
      feature_id = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  first_idx <- match(feature_ids, feature_id_vals)
  first_rows <- results[first_idx, , drop = FALSE]

  canonical_feat_cols <- c("feature_id", "feature_mz", "feature_rt")
  feature_level_passthrough <- intersect(
    c("component_id", "candidates_evaluated", "candidates_best"),
    colnames(results)
  )
  extra_feat_cols <- setdiff(
    c(
      grep("^feature_", colnames(results), value = TRUE),
      feature_level_passthrough
    ),
    canonical_feat_cols
  )

  smf_canonical <- data.frame(
    SMF_ID = as.character(seq_along(feature_ids)),
    SML_ID_REFS = NA_character_,
    SME_ID_REFS = NA_character_,
    SME_ID_REF_ambiguity_code = "null",
    exp_mass_to_charge = if ("feature_mz" %in% colnames(first_rows)) {
      as.character(first_rows$feature_mz)
    } else {
      rep("null", length(feature_ids))
    },
    charge = "null",
    retention_time_in_seconds = if ("feature_rt" %in% colnames(first_rows)) {
      suppressWarnings(as.character(as.numeric(first_rows$feature_rt) * 60))
    } else {
      rep("null", length(feature_ids))
    },
    feature_id = feature_ids,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (col in extra_feat_cols) {
    if (col %in% colnames(first_rows)) {
      val <- as.character(first_rows[[col]])
      val[
        is.na(val) | !nzchar(val) | val == "NA" | val == "null" | val == "NULL"
      ] <- "null"
    } else {
      val <- rep("null", length(feature_ids))
    }

    base_name <- .mztab_opt_colname(col)
    out_name <- base_name
    idx <- 1L
    while (out_name %in% colnames(smf_canonical)) {
      idx <- idx + 1L
      out_name <- paste0(base_name, "_", idx)
    }
    smf_canonical[[out_name]] <- val
  }

  rm(
    list = intersect(
      c(
        "first_rows",
        "feature_id_vals",
        "valid_feature",
        "feature_ids",
        "first_idx",
        "canonical_feat_cols",
        "feature_level_passthrough",
        "extra_feat_cols",
        "col",
        "val",
        "base_name",
        "out_name",
        "idx"
      ),
      ls()
    )
  )

  smf_canonical
}

#' Empty SME schema (column names + types) with zero rows
#' @keywords internal
.mztab_empty_sme <- function() {
  tidytable::tidytable(
    SME_ID = character(0),
    evidence_input_id = character(0),
    database_identifier = character(0),
    chemical_formula = character(0),
    smiles = character(0),
    inchi = character(0),
    chemical_name = character(0),
    uri = character(0),
    derivatized_form = character(0),
    adduct_ion = character(0),
    exp_mass_to_charge = character(0),
    charge = character(0),
    theoretical_mass_to_charge = character(0),
    spectra_ref = character(0),
    identification_method = character(0),
    ms_level = character(0),
    rank = character(0),
    "id_confidence_measure[1]" = character(0),
    "id_confidence_measure[2]" = character(0),
    "id_confidence_measure[3]" = character(0),
    "id_confidence_measure[4]" = character(0),
    feature_id = character(0)
  )
}

#' Build SME (Small Molecule Evidence) table – one row per evidence
#' @keywords internal
.mztab_build_sme <- function(results, smf_table, xrefs_index = NULL) {
  ann <- as.data.frame(results, stringsAsFactors = FALSE, check.names = FALSE)
  n_rows <- nrow(ann)
  if (n_rows == 0L) {
    return(.mztab_empty_sme())
  }

  ann_signal_cols <- unique(c(
    grep("^candidate_", colnames(ann), value = TRUE),
    intersect(
      c(
        "rank_initial",
        "rank_final",
        "score_initial",
        "score_biological",
        "score_chemical",
        "score_final",
        "annotation_note"
      ),
      colnames(ann)
    )
  ))

  has_annotation <- if (length(ann_signal_cols) == 0L) {
    rep(FALSE, n_rows)
  } else {
    # Vectorized rowSums check instead of Reduce("|", lapply())
    mat <- vapply(
      ann_signal_cols,
      function(col) {
        x <- as.character(ann[[col]])
        !(is.na(x) | x == "NA" | x == "null" | x == "NULL" | !nzchar(x))
      },
      logical(n_rows)
    )
    # vapply returns a matrix (n_rows × n_cols) when n_rows > 1, but a
    # named vector (n_cols) when n_rows == 1. Ensure matrix shape.
    if (!is.matrix(mat)) {
      mat <- matrix(mat, nrow = n_rows, ncol = length(ann_signal_cols))
    }
    rowSums(mat) > 0L
  }

  if (!any(has_annotation)) {
    return(.mztab_empty_sme())
  }

  ann <- ann[has_annotation, , drop = FALSE]

  id_lookup <- stats::setNames(
    as.character(smf_table$SMF_ID),
    smf_table$feature_id
  )

  .mztab_safe_scalar <- function(x, n = NULL) {
    if (is.null(x) || length(x) == 0L) {
      if (is.null(n)) {
        n <- nrow(ann)
      }
      return(rep("null", n))
    }

    x <- as.character(x)
    if (!is.null(n) && length(x) != n) {
      x <- rep(x, length.out = n)
    }
    x[is.na(x) | !nzchar(x)] <- "null"
    x
  }

  exact_mass_raw <- suppressWarnings(as.numeric(.mztab_safe_scalar(ann[[
    "candidate_structure_exact_mass"
  ]])))
  adduct_raw <- .mztab_safe_scalar(ann[["candidate_adduct"]])
  theo_mz <- rep("null", length(exact_mass_raw))
  valid_idx <- !is.na(exact_mass_raw) &
    exact_mass_raw > 0 &
    adduct_raw != "null" &
    nzchar(adduct_raw)
  if (any(valid_idx)) {
    theo_vals <- calculate_mz_from_mass_batch(
      neutral_masses = exact_mass_raw[valid_idx],
      adducts = adduct_raw[valid_idx]
    )
    theo_vals[!is.finite(theo_vals)] <- NA_real_
    theo_vals <- suppressWarnings(as.character(round(theo_vals, 6)))
    theo_vals[is.na(theo_vals) | !nzchar(theo_vals)] <- "null"
    theo_mz[valid_idx] <- theo_vals
  }

  lib_col <- .mztab_safe_scalar(ann[["candidate_library"]])
  charge_from_adduct <- .mztab_charge_from_adduct(adduct_raw)
  # MS1-level evidence only comes from TIMA's mass-based annotation source
  # (candidate_library == "TIMA MS1"); external spectral libraries are MS2.
  # Match the canonical label case-insensitively rather than a loose "ms1"
  # substring, which would misclassify e.g. a library named "MS1QuantDB".
  ms_level_val <- ifelse(
    grepl("tima ms1", lib_col, ignore.case = TRUE),
    "[MS, MS:1000579, MS1 spectrum, ]",
    "[MS, MS:1000580, MS2 spectrum, ]"
  )

  raw_spectra_ref <- .mztab_safe_scalar(ann[["candidate_spectrum_id"]])
  spectra_ref_formatted <- ifelse(
    raw_spectra_ref == "null" | is.na(raw_spectra_ref),
    "null",
    paste0("ms_run[1]:", raw_spectra_ref)
  )

  inchikey_col <- .mztab_safe_scalar(ann[[
    "candidate_structure_inchikey_connectivity_layer"
  ]])
  uri_col <- .mztab_safe_scalar(ann[["candidate_structure_uri"]])
  if (!is.null(xrefs_index) && length(xrefs_index) > 0L) {
    xref_resolved <- .mztab_resolve_xref_fields(
      inchikeys = inchikey_col,
      xrefs_index = xrefs_index,
      fallback = inchikey_col
    )
    uri_from_xrefs <- xref_resolved$uri
    database_identifier_col <- xref_resolved$database_identifier
    uri_col <- ifelse(
      uri_col == "null" | is.na(uri_col),
      uri_from_xrefs,
      uri_col
    )
  } else {
    database_identifier_col <- inchikey_col
  }

  feature_ids <- .mztab_safe_scalar(ann[["feature_id"]])
  evidence_input_id <- id_lookup[feature_ids]
  evidence_input_id[is.na(evidence_input_id)] <- "null"

  sme <- list(
    SME_ID = seq_len(nrow(ann)),
    evidence_input_id = evidence_input_id,
    database_identifier = database_identifier_col,
    chemical_formula = .mztab_safe_scalar(ann[[
      "candidate_structure_molecular_formula"
    ]]),
    smiles = .mztab_safe_scalar(ann[["candidate_structure_smiles_no_stereo"]]),
    inchi = .mztab_safe_scalar(ann[["candidate_structure_inchi"]]),
    chemical_name = .mztab_safe_scalar(ann[["candidate_structure_name"]]),
    uri = uri_col,
    derivatized_form = rep("null", nrow(ann)),
    adduct_ion = adduct_raw,
    exp_mass_to_charge = .mztab_safe_scalar(ann[["feature_mz"]]),
    charge = charge_from_adduct,
    theoretical_mass_to_charge = theo_mz,
    spectra_ref = spectra_ref_formatted,
    identification_method = .mztab_library_to_identification_method(lib_col),
    ms_level = ms_level_val,
    rank = .mztab_safe_scalar(ann[["rank_final"]]),
    "id_confidence_measure[1]" = .mztab_safe_scalar(ann[["score_final"]]),
    "id_confidence_measure[2]" = .mztab_safe_scalar(ann[["score_biological"]]),
    "id_confidence_measure[3]" = .mztab_safe_scalar(ann[["score_chemical"]]),
    "id_confidence_measure[4]" = .mztab_safe_scalar(ann[[
      "candidate_score_similarity"
    ]]),
    feature_id = feature_ids
  )
  sme <- as.data.frame(sme, stringsAsFactors = FALSE, check.names = FALSE)

  if (all(sme[["id_confidence_measure[4]"]] == "null", na.rm = TRUE)) {
    sme[["id_confidence_measure[4]"]] <- NULL
  }

  consumed_cols <- c(
    "feature_id",
    "candidate_structure_molecular_formula",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_inchi",
    "candidate_structure_name",
    "candidate_structure_uri",
    "candidate_adduct",
    "feature_mz",
    "candidate_structure_exact_mass",
    "candidate_spectrum_id",
    "candidate_library",
    "rank_final",
    "score_final",
    "score_biological",
    "score_chemical",
    "candidate_score_similarity",
    grep("^feature_", colnames(ann), value = TRUE),
    intersect(
      c("component_id", "candidates_evaluated", "candidates_best"),
      colnames(ann)
    )
  )
  if (is.null(xrefs_index) || length(xrefs_index) == 0L) {
    consumed_cols <- c(
      consumed_cols,
      "candidate_structure_inchikey_connectivity_layer"
    )
  }
  extra_cols <- setdiff(colnames(ann), consumed_cols)
  if (length(extra_cols) > 0L) {
    base_names <- vapply(
      extra_cols,
      .mztab_opt_colname,
      character(1L),
      USE.NAMES = FALSE
    )
    final_names <- make.unique(c(colnames(sme), base_names), sep = "_")[
      (length(colnames(sme)) + 1L):(length(colnames(sme)) + length(base_names))
    ]
    for (i in seq_along(extra_cols)) {
      sme[[final_names[[i]]]] <- .mztab_safe_scalar(ann[[extra_cols[[i]]]])
    }
  }

  # Safe rm() — some variables are only defined conditionally; use
  # intersect to avoid "object not found" warnings
  rm(
    list = intersect(
      c(
        "ann",
        "ann_signal_cols",
        "has_annotation",
        "id_lookup",
        "exact_mass_raw",
        "adduct_raw",
        "theo_mz",
        "valid_idx",
        "theo_vals",
        "lib_col",
        "charge_from_adduct",
        "ms_level_val",
        "raw_spectra_ref",
        "spectra_ref_formatted",
        "inchikey_col",
        "uri_col",
        "xref_resolved",
        "database_identifier_col",
        "feature_ids",
        "evidence_input_id",
        "consumed_cols",
        "extra_cols",
        "base_names",
        "final_names",
        "i"
      ),
      ls()
    )
  )

  tidytable::as_tidytable(sme)
}

#' Build SML (Small Molecule Summary) table.
#'
#' SML rows are emitted per feature and retain multi-candidate ambiguity using
#' pipe-aligned values across identifier columns.
#' @keywords internal
.mztab_build_sml <- function(
  results,
  smf_table,
  sme_table,
  xrefs_index = NULL
) {
  if (nrow(sme_table) == 0L) {
    # No annotations; return skeleton SML (one unannotated row per feature)
    return(tidytable::tidytable(
      SML_ID = as.character(seq_len(nrow(smf_table))),
      SMF_ID_REFS = as.character(smf_table$SMF_ID),
      SME_ID_REFS = "null",
      database_identifier = "null",
      chemical_formula = "null",
      smiles = "null",
      inchi = "null",
      chemical_name = "null",
      uri = "null",
      theoretical_neutral_mass = "null",
      adduct_ions = "null",
      reliability = "null",
      best_id_confidence_measure = "id_confidence_measure[1]",
      best_id_evidence_value = "null"
    ))
  }

  results_src <- as.data.frame(
    results,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  sme_src <- as.data.frame(
    sme_table,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  .mztab_norm_scalar <- function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x)] <- "null"
    x
  }

  sme_fids_chr <- as.character(sme_src$feature_id)
  valid_sme_mask <- !is.na(sme_fids_chr) &
    nzchar(sme_fids_chr) &
    sme_fids_chr != "null"
  feature_ids <- unique(sme_fids_chr[valid_sme_mask])
  n_fids <- length(feature_ids)

  if (n_fids == 0L) {
    return(tidytable::tidytable(
      SML_ID = character(0),
      SMF_ID_REFS = character(0),
      SME_ID_REFS = character(0),
      database_identifier = character(0),
      chemical_formula = character(0),
      smiles = character(0),
      inchi = character(0),
      chemical_name = character(0),
      uri = character(0),
      theoretical_neutral_mass = character(0),
      adduct_ions = character(0),
      reliability = character(0),
      best_id_confidence_measure = character(0),
      best_id_evidence_value = character(0)
    ))
  }

  # 1. Pre-compute theoretical_neutral_mass per feature_id in a single vectorized pass
  theoretical_neutral_mass <- rep("null", n_fids)
  if (
    nrow(results_src) > 0L &&
      "feature_id" %in% colnames(results_src) &&
      "candidate_structure_exact_mass" %in% colnames(results_src)
  ) {
    res_fid_idx <- fastmatch::fmatch(
      as.character(results_src$feature_id),
      feature_ids
    )
    valid_res <- which(!is.na(res_fid_idx))
    if (length(valid_res) > 0L) {
      if ("score_final" %in% colnames(results_src)) {
        rs <- suppressWarnings(as.numeric(results_src$score_final[valid_res]))
        rs[is.na(rs)] <- -Inf
        ord_res <- order(res_fid_idx[valid_res], -rs)
        valid_res_sorted <- valid_res[ord_res]
        first_res <- valid_res_sorted[
          !duplicated(res_fid_idx[valid_res_sorted])
        ]
      } else {
        first_res <- valid_res[!duplicated(res_fid_idx[valid_res])]
      }
      em <- suppressWarnings(
        as.numeric(results_src$candidate_structure_exact_mass[first_res])
      )
      fin <- is.finite(em)
      if (any(fin)) {
        theoretical_neutral_mass[res_fid_idx[first_res[fin]]] <-
          as.character(round(em[fin], 6))
      }
    }
  }

  # 2. Map SMF_ID_REFS once
  smf_fids_chr <- as.character(smf_table$feature_id)
  smf_ids_chr <- as.character(smf_table$SMF_ID)
  smf_match <- fastmatch::fmatch(feature_ids, smf_fids_chr)
  smf_id_refs <- smf_ids_chr[smf_match]
  smf_id_refs[is.na(smf_id_refs) | !nzchar(smf_id_refs)] <- "null"

  # 3. Restrict sme_src to valid feature_ids and normalize columns once
  if (!all(valid_sme_mask)) {
    sme_src <- sme_src[valid_sme_mask, , drop = FALSE]
    sme_fids_chr <- sme_fids_chr[valid_sme_mask]
  }
  fid_idx <- fastmatch::fmatch(sme_fids_chr, feature_ids)
  counts <- tabulate(fid_idx, nbins = n_fids)

  db_vec <- .mztab_norm_scalar(sme_src$database_identifier)
  cf_vec <- .mztab_norm_scalar(sme_src$chemical_formula)
  sm_vec <- .mztab_norm_scalar(sme_src$smiles)
  in_vec <- .mztab_norm_scalar(sme_src$inchi)
  cn_vec <- .mztab_norm_scalar(sme_src$chemical_name)
  ur_vec <- .mztab_norm_scalar(sme_src$uri)
  ad_vec <- .mztab_norm_scalar(sme_src$adduct_ion)
  sme_id_vec <- .mztab_norm_scalar(sme_src$SME_ID)
  method_vec <- .mztab_norm_scalar(sme_src$identification_method)
  score_raw <- suppressWarnings(
    as.numeric(sme_src[["id_confidence_measure[1]"]])
  )

  if (all(counts == 1L)) {
    ord <- match(seq_len(n_fids), fid_idx)
    best_score <- score_raw[ord]
    best_score[!is.finite(best_score)] <- NA_real_
    score_val <- ifelse(
      is.na(best_score),
      "null",
      as.character(round(best_score, 6))
    )
    best_method <- method_vec[ord]
    reliability <- as.character(.mztab_score_to_reliability(
      best_score,
      best_method
    ))
    sme_ids <- sme_id_vec[ord]
    sme_id_refs <- ifelse(sme_ids != "null", sme_ids, "null")

    return(data.frame(
      SML_ID = as.character(seq_len(n_fids)),
      SMF_ID_REFS = smf_id_refs,
      SME_ID_REFS = sme_id_refs,
      database_identifier = db_vec[ord],
      chemical_formula = cf_vec[ord],
      smiles = sm_vec[ord],
      inchi = in_vec[ord],
      chemical_name = cn_vec[ord],
      uri = ur_vec[ord],
      theoretical_neutral_mass = theoretical_neutral_mass,
      adduct_ions = ad_vec[ord],
      reliability = reliability,
      best_id_confidence_measure = rep("id_confidence_measure[1]", n_fids),
      best_id_evidence_value = score_val,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  # General case: sort all rows by (fid_idx, rank_num, -score_num, sme_num)
  rank_num <- suppressWarnings(as.numeric(sme_src$rank))
  rank_num[is.na(rank_num)] <- Inf
  score_num <- score_raw
  score_num[!is.finite(score_num)] <- -Inf
  sme_num <- suppressWarnings(as.integer(sme_src$SME_ID))
  sme_num[is.na(sme_num)] <- .Machine$integer.max

  ord <- order(fid_idx, rank_num, -score_num, sme_num)
  fid_ord <- fid_idx[ord]

  # Deduplicate 7-column tuple only for features with > 1 SME row
  multi_row_mask <- counts[fid_ord] > 1L
  if (any(multi_row_mask)) {
    m_ord <- ord[multi_row_mask]
    tuple_key <- paste(
      fid_idx[m_ord],
      db_vec[m_ord],
      cf_vec[m_ord],
      sm_vec[m_ord],
      in_vec[m_ord],
      cn_vec[m_ord],
      ur_vec[m_ord],
      ad_vec[m_ord],
      sep = "||"
    )
    keep_ord <- rep(TRUE, length(ord))
    keep_ord[multi_row_mask] <- !duplicated(tuple_key)
    ord <- ord[keep_ord]
    fid_ord <- fid_ord[keep_ord]
  }

  dedup_counts <- tabulate(fid_ord, nbins = n_fids)
  first_pos <- which(!duplicated(fid_ord))
  best_idx <- ord[first_pos]

  best_score <- score_raw[best_idx]
  best_score[!is.finite(best_score)] <- NA_real_
  score_val <- ifelse(
    is.na(best_score),
    "null",
    as.character(round(best_score, 6))
  )
  best_method <- method_vec[best_idx]
  reliability <- as.character(.mztab_score_to_reliability(
    best_score,
    best_method
  ))

  out_db <- db_vec[best_idx]
  out_cf <- cf_vec[best_idx]
  out_sm <- sm_vec[best_idx]
  out_in <- in_vec[best_idx]
  out_cn <- cn_vec[best_idx]
  out_ur <- ur_vec[best_idx]
  out_ad <- ad_vec[best_idx]
  out_sme_refs <- ifelse(
    sme_id_vec[best_idx] != "null",
    sme_id_vec[best_idx],
    "null"
  )

  # For features with > 1 deduplicated candidate, collapse with "|"
  if (any(dedup_counts > 1L)) {
    multi_mask <- dedup_counts[fid_ord] > 1L
    m_ord <- ord[multi_mask]
    m_fid <- fid_ord[multi_mask]
    u_m_fid <- unique(m_fid)
    m_groups <- split(seq_along(m_ord), factor(m_fid, levels = u_m_fid))

    collapse_vec <- function(v) {
      vapply(
        m_groups,
        function(g) paste(v[g], collapse = "|"),
        character(1L),
        USE.NAMES = FALSE
      )
    }

    out_db[u_m_fid] <- collapse_vec(db_vec[m_ord])
    out_cf[u_m_fid] <- collapse_vec(cf_vec[m_ord])
    out_sm[u_m_fid] <- collapse_vec(sm_vec[m_ord])
    out_in[u_m_fid] <- collapse_vec(in_vec[m_ord])
    out_cn[u_m_fid] <- collapse_vec(cn_vec[m_ord])
    out_ur[u_m_fid] <- collapse_vec(ur_vec[m_ord])
    out_ad[u_m_fid] <- collapse_vec(ad_vec[m_ord])

    m_sme_ids <- sme_id_vec[m_ord]
    out_sme_refs[u_m_fid] <- vapply(
      m_groups,
      function(g) {
        ids <- unique(m_sme_ids[g])
        ids <- ids[ids != "null"]
        if (length(ids) > 0L) paste(ids, collapse = "|") else "null"
      },
      character(1L),
      USE.NAMES = FALSE
    )
  }

  data.frame(
    SML_ID = as.character(seq_len(n_fids)),
    SMF_ID_REFS = smf_id_refs,
    SME_ID_REFS = out_sme_refs,
    database_identifier = out_db,
    chemical_formula = out_cf,
    smiles = out_sm,
    inchi = out_in,
    chemical_name = out_cn,
    uri = out_ur,
    theoretical_neutral_mass = theoretical_neutral_mass,
    adduct_ions = out_ad,
    reliability = reliability,
    best_id_confidence_measure = rep("id_confidence_measure[1]", n_fids),
    best_id_evidence_value = score_val,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
