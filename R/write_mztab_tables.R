# ── Internal helpers ──────────────────────────────────────────────────────────

#' Safely extract a column or return "null" vector
#' @keywords internal
.mztab_pluck <- function(df, col) {
  if (col %in% colnames(df)) {
    as.character(df[[col]])
  } else {
    rep("null", nrow(df))
  }
}

#' Safely extract a column, replacing NA/null with "null"
#' @keywords internal
.mztab_pluck_na <- function(df, col) {
  x <- .mztab_pluck(df, col)
  x[is.na(x) | x == "NA" | x == "null" | !nzchar(x)] <- "null"
  x
}

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

  # safe_fread() may return a data.table; convert once
  results <- as.data.frame(
    results,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  id_cols <- setdiff(colnames(results), multi_cols)

  # Pre-compute the number of values per row
  n_vals_per_row <- vapply(
    seq_len(nrow(results)),
    function(i) {
      max(
        vapply(
          multi_cols,
          function(col) {
            v <- results[[col]][i]
            if (is.na(v) || !nzchar(v)) {
              1L
            } else {
              length(strsplit(v, "|", fixed = TRUE)[[1L]])
            }
          },
          FUN.VALUE = integer(1L)
        )
      )
    },
    FUN.VALUE = integer(1L)
  )

  rows <- lapply(seq_len(nrow(results)), function(i) {
    n_vals <- n_vals_per_row[i]
    if (n_vals <= 1L) {
      return(results[i, , drop = FALSE])
    }

    base <- results[rep(i, n_vals), id_cols, drop = FALSE]
    expanded <- lapply(multi_cols, function(col) {
      v <- results[[col]][i]
      if (is.na(v) || !nzchar(v)) {
        return(rep(NA_character_, n_vals))
      }
      parts <- strsplit(v, "|", fixed = TRUE)[[1L]]
      if (length(parts) < n_vals) {
        parts <- c(parts, rep(NA_character_, n_vals - length(parts)))
      }
      parts[seq_len(n_vals)]
    })
    names(expanded) <- multi_cols
    expanded_df <- as.data.frame(expanded, stringsAsFactors = FALSE)

    cbind(base, expanded_df)[, colnames(results), drop = FALSE]
  })

  tidytable::as_tidytable(tidytable::bind_rows(rows))
}

#' Build SMF (Small Molecule Feature) table – one row per feature_id
#' @keywords internal
.mztab_build_smf <- function(results) {
  # Assign sequential integer SMF_ID to each unique feature_id.
  feature_ids <- unique(results$feature_id[!is.na(results$feature_id)])

  smf <- tidytable::tidytable(
    SMF_ID = seq_along(feature_ids),
    feature_id = feature_ids
  )

  # Canonical feature columns that become proper mzTab-M SMF fields.
  canonical_feat_cols <- c("feature_id", "feature_mz", "feature_rt")

  # All feature_* columns present in results (beyond the canonical three) are
  # surfaced as opt_global_* in the SMF section.  They are feature-level data
  # and must not land in the per-evidence SME section.
  # TIMA summary columns below are also feature-level and must survive export.
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

  # Attach mz, rt and any extra feature metadata from the first occurrence per
  # feature_id in results.
  feat_meta_cols <- c(canonical_feat_cols, extra_feat_cols)
  feat_meta <- results |>
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::select(
      tidyselect::any_of(feat_meta_cols)
    )

  smf <- smf |>
    tidytable::left_join(feat_meta, by = "feature_id")

  # Convert RT to seconds for mzTab-M.
  if ("feature_rt" %in% colnames(smf)) {
    smf <- smf |>
      tidytable::mutate(
        retention_time_in_seconds = suppressWarnings(
          as.character(as.numeric(feature_rt) * 60)
        )
      )
  } else {
    smf$retention_time_in_seconds <- "null"
  }

  # Build canonical portion of SMF.
  smf_canonical <- smf |>
    tidytable::transmute(
      SMF_ID = as.character(SMF_ID),
      SML_ID_REFS = NA_character_, # filled later
      SME_ID_REFS = NA_character_, # filled later from SME feature links
      SME_ID_REF_ambiguity_code = "null",
      exp_mass_to_charge = if ("feature_mz" %in% colnames(smf)) {
        feature_mz
      } else {
        "null"
      },
      charge = "null",
      retention_time_in_seconds,
      feature_id = feature_id
    )

  # Append opt_global_* columns for extra feature-level metadata.
  if (length(extra_feat_cols) > 0L) {
    used_names <- colnames(smf_canonical)

    # Build all opt_global column names first
    opt_col_mapping <- lapply(extra_feat_cols, function(col) {
      base_name <- .mztab_opt_colname(col)
      out_name <- base_name
      idx <- 1L
      while (out_name %in% used_names) {
        idx <- idx + 1L
        out_name <- paste0(base_name, "_", idx)
      }
      used_names <<- c(used_names, out_name)
      list(col = col, out_name = out_name)
    })

    # Vectorized extraction and assignment
    for (mapping in opt_col_mapping) {
      col <- mapping$col
      out_name <- mapping$out_name
      val <- if (col %in% colnames(smf)) {
        v <- as.character(smf[[col]])
        v[is.na(v) | !nzchar(v) | v == "NA"] <- "null"
        v
      } else {
        rep("null", nrow(smf_canonical))
      }
      smf_canonical[[out_name]] <- val
    }
  }

  smf_canonical
}

#' Build SME (Small Molecule Evidence) table – one row per evidence
#' @keywords internal
.mztab_build_sme <- function(results, smf_table, xrefs_index = NULL) {
  ann <- as.data.frame(results, stringsAsFactors = FALSE, check.names = FALSE)
  ann_signal_cols <- unique(c(
    grep("^candidate_", colnames(results), value = TRUE),
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
      colnames(results)
    )
  ))

  has_annotation <- if (length(ann_signal_cols) == 0L) {
    rep(FALSE, nrow(results))
  } else {
    Reduce(
      `|`,
      lapply(ann_signal_cols, function(col) {
        x <- as.character(results[[col]])
        !(is.na(x) | x == "NA" | x == "null" | x == "NULL" | !nzchar(x))
      })
    )
  }

  if (!any(has_annotation)) {
    # No candidates at all – return a minimal empty SME
    return(tidytable::tidytable(
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
    ))
  }

  ann <- results[has_annotation, , drop = FALSE]

  # Map feature_id -> SMF_ID
  id_lookup <- stats::setNames(
    as.character(smf_table$SMF_ID),
    smf_table$feature_id
  )

  # Compute theoretical m/z from neutral mass + adduct when possible.
  # Vectorized approach: process all rows at once
  exact_mass_raw <- suppressWarnings(as.numeric(.mztab_pluck(
    ann,
    "candidate_structure_exact_mass"
  )))
  adduct_raw <- .mztab_pluck(ann, "candidate_adduct")

  theo_mz <- mapply(
    function(m, ads) {
      if (!is.na(m) && m > 0 && !is.na(ads) && ads != "null" && nzchar(ads)) {
        val <- tryCatch(
          calculate_mz_from_mass(neutral_mass = m, adduct_string = ads),
          error = function(...) NA_real_ # mute: harmless calculation error
        )
        if (!is.na(val) && val > 0) {
          return(as.character(round(val, 6)))
        }
      }
      "null"
    },
    exact_mass_raw,
    adduct_raw,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  # Determine ms_level from candidate_library
  lib_col <- .mztab_pluck(ann, "candidate_library")
  charge_from_adduct <- .mztab_charge_from_adduct(.mztab_pluck(
    ann,
    "candidate_adduct"
  ))
  ms_level_val <- tidytable::case_when(
    grepl("MS1|ms1|TIMA MS1", lib_col, ignore.case = TRUE) ~
      "[MS, MS:1000579, MS1 spectrum, ]",
    grepl(
      "MS2|ms2|spectral|gnps|mztab|sirius|mzmine|mzMine",
      lib_col,
      ignore.case = TRUE
    ) ~
      "[MS, MS:1000580, MS2 spectrum, ]",
    TRUE ~ "[MS, MS:1000580, MS2 spectrum, ]"
  )

  # Format spectra_ref as ms_run[1]:{spectrum_native_id}.
  # The mzTab-M spec requires this prefix so downstream tools can locate
  # spectra back to the declared ms_run[1] block.
  raw_spectra_ref <- .mztab_pluck_na(ann, "candidate_spectrum_id")
  spectra_ref_formatted <- ifelse(
    raw_spectra_ref == "null" | is.na(raw_spectra_ref),
    "null",
    paste0("ms_run[1]:", raw_spectra_ref)
  )

  # Resolve URIs from xrefs (prefer Wikidata, then ChEBI, then first available)
  inchikey_col <- .mztab_pluck_na(
    ann,
    "candidate_structure_inchikey_connectivity_layer"
  )
  uri_col <- .mztab_pluck_na(ann, "candidate_structure_uri")
  if (!is.null(xrefs_index) && length(xrefs_index) > 0L) {
    uri_from_xrefs <- vapply(
      X = inchikey_col,
      FUN = function(ik) {
        if (is.na(ik) || ik == "null" || !nzchar(ik)) {
          return("null")
        }
        rows <- xrefs_index[[ik]]
        if (is.null(rows) || nrow(rows) == 0L) {
          return("null")
        }
        .mztab_pick_best_uri(rows)
      },
      FUN.VALUE = character(1L)
    )
    # Override bare "null" from original uri with xref-resolved URIs
    uri_col <- ifelse(
      uri_col == "null" | is.na(uri_col),
      uri_from_xrefs,
      uri_col
    )
  }

  # Prefer prefixed database identifiers (e.g. CHEBI:12345) when xrefs are
  # available; otherwise fall back to the connectivity-layer InChIKey fragment.
  database_identifier_col <- inchikey_col
  if (!is.null(xrefs_index) && length(xrefs_index) > 0L) {
    database_identifier_col <- vapply(
      X = inchikey_col,
      FUN = function(ik) {
        .mztab_pick_best_database_identifier(
          rows = xrefs_index[[ik]],
          fallback = ik
        )
      },
      FUN.VALUE = character(1L)
    )
  }

  sme <- tidytable::tidytable(
    SME_ID = as.character(seq_len(nrow(ann))),
    evidence_input_id = id_lookup[.mztab_pluck(ann, "feature_id")],
    database_identifier = database_identifier_col,
    chemical_formula = .mztab_pluck_na(
      ann,
      "candidate_structure_molecular_formula"
    ),
    smiles = .mztab_pluck_na(ann, "candidate_structure_smiles_no_stereo"),
    inchi = .mztab_pluck_na(ann, "candidate_structure_inchi"),
    chemical_name = .mztab_pluck_na(ann, "candidate_structure_name"),
    uri = uri_col,
    derivatized_form = "null",
    adduct_ion = .mztab_pluck_na(ann, "candidate_adduct"),
    exp_mass_to_charge = .mztab_pluck_na(ann, "feature_mz"),
    charge = charge_from_adduct,
    theoretical_mass_to_charge = theo_mz,
    spectra_ref = spectra_ref_formatted,
    identification_method = .mztab_library_to_identification_method(.mztab_pluck(
      ann,
      "candidate_library"
    )),
    ms_level = ms_level_val,
    rank = .mztab_pluck_na(ann, "rank_final"),
    "id_confidence_measure[1]" = .mztab_pluck_na(ann, "score_final"),
    "id_confidence_measure[2]" = .mztab_pluck_na(ann, "score_biological"),
    "id_confidence_measure[3]" = .mztab_pluck_na(ann, "score_chemical"),
    # [4] spectral similarity – TIMA user-namespace (TIMA:004).
    # No PSI-MS accession covers composite metabolomics similarity scores;
    # using the TIMA user-CV keeps the namespace consistent with measures 1-3.
    "id_confidence_measure[4]" = .mztab_pluck_na(
      ann,
      "candidate_score_similarity"
    ),
    feature_id = .mztab_pluck(ann, "feature_id")
  )

  # Drop [4] column when no spectral similarity data is present to keep the
  # metadata section in sync (declared only when the column is non-trivial).
  if (all(sme[["id_confidence_measure[4]"]] == "null", na.rm = TRUE)) {
    sme[["id_confidence_measure[4]"]] <- NULL
  }

  # Append opt_global_* columns for unmapped candidate-level metadata
  # Columns directly mapped to canonical mzTab SME fields or that belong
  # semantically to the SMF (feature) level rather than SME (evidence) level.
  # Feature-level columns are attached to the SMF section instead – they must
  # not appear as opt_global_* in SME to avoid redundancy and consumer confusion.
  consumed_cols <- c(
    # ── canonical SME mappings ───────────────────────────────────────────────
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
    # [4] spectral similarity → id_confidence_measure[4]
    "candidate_score_similarity",
    # ── feature-level columns that belong in SMF, not SME ───────────────────
    # Matched by prefix so future feature_* columns are automatically excluded.
    grep("^feature_", colnames(ann), value = TRUE),
    # TIMA feature-level summary columns exported to SMF as opt_global_*.
    intersect(
      c("component_id", "candidates_evaluated", "candidates_best"),
      colnames(ann)
    )
  )
  # When xrefs drive canonical database_identifier, still preserve the original
  # TIMA InChIKey source column as opt_global_* for full reporting parity.
  if (is.null(xrefs_index) || length(xrefs_index) == 0L) {
    consumed_cols <- c(
      consumed_cols,
      "candidate_structure_inchikey_connectivity_layer"
    )
  }
  extra_cols <- setdiff(colnames(ann), consumed_cols)
  if (length(extra_cols) > 0L) {
    used_names <- colnames(sme)

    # Generate base names for all extra columns
    base_names <- vapply(
      extra_cols,
      .mztab_opt_colname,
      character(1L),
      USE.NAMES = FALSE
    )

    # Use make.unique to efficiently handle name collisions with existing columns
    all_candidate_names <- c(used_names, base_names)
    unique_mapping <- make.unique(all_candidate_names, sep = "_")

    # Extract the final unique names (skip the used_names prefix)
    final_names <- unique_mapping[seq_along(base_names) + length(used_names)]

    # Build mapping list
    opt_col_mapping <- mapply(
      function(col, out_name) list(col = col, out_name = out_name),
      extra_cols,
      final_names,
      SIMPLIFY = FALSE
    )

    # Extraction and assignment
    for (mapping in opt_col_mapping) {
      sme[[mapping$out_name]] <- .mztab_pluck_na(ann, mapping$col)
    }
  }

  sme
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

  .mztab_join_aligned <- function(df, row_order, col) {
    vals <- .mztab_norm_scalar(df[[col]][row_order])
    if (length(vals) == 0L) {
      return("null")
    }
    paste(vals, collapse = "|")
  }

  smf_id_lookup <- stats::setNames(
    as.character(smf_table$SMF_ID),
    as.character(smf_table$feature_id)
  )

  feature_ids <- unique(as.character(sme_src$feature_id))
  feature_ids <- feature_ids[
    !is.na(feature_ids) & nzchar(feature_ids) & feature_ids != "null"
  ]
  if (length(feature_ids) == 0L) {
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

  feature_groups <- split(
    seq_len(nrow(sme_src)),
    as.character(sme_src$feature_id)
  )
  feature_groups <- feature_groups[feature_ids]

  results_feature_groups <- NULL
  if (
    nrow(results_src) > 0L &&
      "feature_id" %in% colnames(results_src) &&
      "candidate_structure_exact_mass" %in% colnames(results_src)
  ) {
    results_feature_groups <- split(
      seq_len(nrow(results_src)),
      as.character(results_src$feature_id)
    )
  }

  sml_rows <- lapply(seq_along(feature_ids), function(j) {
    fid <- feature_ids[[j]]
    idx <- feature_groups[[fid]]
    grp <- sme_src[idx, , drop = FALSE]

    rank_num <- suppressWarnings(as.numeric(grp$rank))
    rank_num[is.na(rank_num)] <- Inf
    score_num <- suppressWarnings(as.numeric(grp[["id_confidence_measure[1]"]]))
    score_num[!is.finite(score_num)] <- -Inf
    sme_num <- suppressWarnings(as.integer(grp$SME_ID))
    sme_num[is.na(sme_num)] <- .Machine$integer.max

    order_idx <- order(rank_num, -score_num, sme_num)
    if (length(order_idx) == 0L) {
      order_idx <- seq_len(nrow(grp))
    }

    # Deduplicate identical candidate tuples while keeping a deterministic order.
    tuple_cols <- c(
      "database_identifier",
      "chemical_formula",
      "smiles",
      "inchi",
      "chemical_name",
      "uri",
      "adduct_ion"
    )
    tuple_parts <- lapply(tuple_cols, function(col) {
      .mztab_norm_scalar(grp[[col]])
    })
    tuple_key <- do.call(paste, c(tuple_parts, list(sep = "||")))
    tuple_key <- tuple_key[order_idx]
    order_idx <- order_idx[!duplicated(tuple_key)]
    if (length(order_idx) == 0L) {
      order_idx <- 1L
    }

    best_local <- order_idx[[1L]]
    best_score <- suppressWarnings(as.numeric(grp[[
      "id_confidence_measure[1]"
    ]][[best_local]]))
    if (!is.finite(best_score)) {
      best_score <- NA_real_
    }
    score_val <- if (!is.na(best_score)) {
      as.character(round(best_score, 6))
    } else {
      "null"
    }

    best_method <- .mztab_norm_scalar(grp$identification_method[[best_local]])
    reliability <- as.character(.mztab_score_to_reliability(
      best_score,
      best_method
    ))

    smf_id <- as.character(smf_id_lookup[[fid]])
    if (is.null(smf_id) || is.na(smf_id) || !nzchar(smf_id)) {
      smf_id <- "null"
    }

    sme_ids <- unique(.mztab_norm_scalar(grp$SME_ID[order_idx]))
    sme_ids <- sme_ids[sme_ids != "null"]
    sme_id_refs <- if (length(sme_ids) > 0L) {
      paste(sme_ids, collapse = "|")
    } else {
      "null"
    }

    theoretical_neutral_mass <- "null"
    if (!is.null(results_feature_groups)) {
      ridx <- results_feature_groups[[fid]]
      if (length(ridx) > 0L) {
        if ("score_final" %in% colnames(results_src)) {
          rs <- suppressWarnings(as.numeric(results_src$score_final[ridx]))
          rs[is.na(rs)] <- -Inf
          ridx <- ridx[order(-rs)]
        }
        em <- suppressWarnings(as.numeric(results_src$candidate_structure_exact_mass[ridx[[
          1L
        ]]]))
        if (length(em) == 1L && is.finite(em)) {
          theoretical_neutral_mass <- as.character(round(em, 6))
        }
      }
    }

    tidytable::tidytable(
      SML_ID = as.character(j),
      SMF_ID_REFS = smf_id,
      SME_ID_REFS = sme_id_refs,
      database_identifier = .mztab_join_aligned(
        grp,
        order_idx,
        "database_identifier"
      ),
      chemical_formula = .mztab_join_aligned(
        grp,
        order_idx,
        "chemical_formula"
      ),
      smiles = .mztab_join_aligned(grp, order_idx, "smiles"),
      inchi = .mztab_join_aligned(grp, order_idx, "inchi"),
      chemical_name = .mztab_join_aligned(grp, order_idx, "chemical_name"),
      uri = .mztab_join_aligned(grp, order_idx, "uri"),
      theoretical_neutral_mass = theoretical_neutral_mass,
      adduct_ions = .mztab_join_aligned(grp, order_idx, "adduct_ion"),
      reliability = reliability,
      best_id_confidence_measure = "id_confidence_measure[1]",
      best_id_evidence_value = score_val
    )
  })

  tidytable::bind_rows(sml_rows)
}

#' Read base mzTab while preserving unmanaged lines
#'
#' Collects passthrough content as every non-blank line that does not belong to
#' the managed TIMA sections (`MTD`, `SMH/SML`, `SFH/SMF`, `SEH/SME`). This
#' includes custom extension blocks and comment rows from anywhere in the file.
#' Passthrough lines are emitted before the TIMA-managed metadata section.
#'
#' @keywords internal
