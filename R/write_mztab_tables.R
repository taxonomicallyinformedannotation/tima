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

  smf_canonical
}

#' Build SME (Small Molecule Evidence) table – one row per evidence
#' @keywords internal
.mztab_build_sme <- function(results, smf_table, xrefs_index = NULL) {
  ann <- as.data.frame(results, stringsAsFactors = FALSE, check.names = FALSE)
  n_rows <- nrow(ann)
  if (n_rows == 0L) {
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
    any_col <- lapply(ann_signal_cols, function(col) {
      x <- ann[[col]]
      x <- as.character(x)
      !(is.na(x) | x == "NA" | x == "null" | x == "NULL" | !nzchar(x))
    })
    Reduce(`|`, any_col)
  }

  if (!any(has_annotation)) {
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
  ms_level_val <- ifelse(
    grepl("MS1|ms1|TIMA MS1", lib_col, ignore.case = TRUE),
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

  .mztab_norm_scalar_vec <- function(x) {
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
  group_sizes <- lengths(feature_groups)

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

  if (all(group_sizes == 1L)) {
    row_idx <- unlist(feature_groups[feature_ids], use.names = FALSE)
    grp <- sme_src[row_idx, , drop = FALSE]
    grp_feature_ids <- feature_ids

    best_score <- suppressWarnings(as.numeric(grp[["id_confidence_measure[1]"]]))
    best_score[!is.finite(best_score)] <- NA_real_
    score_val <- ifelse(
      is.na(best_score),
      "null",
      as.character(round(best_score, 6))
    )

    best_method <- .mztab_norm_scalar_vec(grp$identification_method)
    reliability <- vapply(
      seq_along(best_score),
      function(i) {
        as.character(.mztab_score_to_reliability(best_score[[i]], best_method[[i]]))
      },
      FUN.VALUE = character(1L)
    )

    smf_ids <- vapply(
      grp_feature_ids,
      function(fid) {
        smf_id <- as.character(smf_id_lookup[[fid]])
        if (is.null(smf_id) || is.na(smf_id) || !nzchar(smf_id)) {
          smf_id <- "null"
        }
        smf_id
      },
      FUN.VALUE = character(1L)
    )

    sme_ids <- .mztab_norm_scalar_vec(grp$SME_ID)
    sme_id_refs <- ifelse(sme_ids != "null", sme_ids, "null")

    theoretical_neutral_mass <- rep("null", length(grp_feature_ids))
    if (!is.null(results_feature_groups)) {
      for (i in seq_along(grp_feature_ids)) {
        fid <- grp_feature_ids[[i]]
        ridx <- results_feature_groups[[fid]]
        if (length(ridx) > 0L) {
          if ("score_final" %in% colnames(results_src)) {
            rs <- suppressWarnings(as.numeric(results_src$score_final[ridx]))
            rs[is.na(rs)] <- -Inf
            ridx <- ridx[order(-rs)]
          }
          em <- suppressWarnings(as.numeric(results_src$candidate_structure_exact_mass[ridx[[1L]]]))
          if (length(em) == 1L && is.finite(em)) {
            theoretical_neutral_mass[[i]] <- as.character(round(em, 6))
          }
        }
      }
    }

    return(as.data.frame(
      data.frame(
        SML_ID = as.character(seq_along(grp_feature_ids)),
        SMF_ID_REFS = smf_ids,
        SME_ID_REFS = sme_id_refs,
        database_identifier = .mztab_norm_scalar_vec(grp$database_identifier),
        chemical_formula = .mztab_norm_scalar_vec(grp$chemical_formula),
        smiles = .mztab_norm_scalar_vec(grp$smiles),
        inchi = .mztab_norm_scalar_vec(grp$inchi),
        chemical_name = .mztab_norm_scalar_vec(grp$chemical_name),
        uri = .mztab_norm_scalar_vec(grp$uri),
        theoretical_neutral_mass = theoretical_neutral_mass,
        adduct_ions = .mztab_norm_scalar_vec(grp$adduct_ion),
        reliability = reliability,
        best_id_confidence_measure = "id_confidence_measure[1]",
        best_id_evidence_value = score_val,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  sml_rows <- vector("list", length(feature_ids))
  for (j in seq_along(feature_ids)) {
    fid <- feature_ids[[j]]
    idx <- feature_groups[[fid]]
    grp <- sme_src[idx, , drop = FALSE]

    if (nrow(grp) == 1L) {
      order_idx <- 1L
      best_local <- 1L
      best_score <- suppressWarnings(as.numeric(grp[[
        "id_confidence_measure[1]"
      ]][[1L]]))
      if (!is.finite(best_score)) {
        best_score <- NA_real_
      }
      score_val <- if (!is.na(best_score)) {
        as.character(round(best_score, 6))
      } else {
        "null"
      }

      best_method <- .mztab_norm_scalar(grp$identification_method[[1L]])
      reliability <- as.character(.mztab_score_to_reliability(
        best_score,
        best_method
      ))

      smf_id <- as.character(smf_id_lookup[[fid]])
      if (is.null(smf_id) || is.na(smf_id) || !nzchar(smf_id)) {
        smf_id <- "null"
      }

      sme_ids <- .mztab_norm_scalar(grp$SME_ID[[1L]])
      sme_id_refs <- if (length(sme_ids) > 0L && sme_ids != "null") {
        sme_ids
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

      sml_rows[[j]] <- list(
        SML_ID = as.character(j),
        SMF_ID_REFS = smf_id,
        SME_ID_REFS = sme_id_refs,
        database_identifier = .mztab_norm_scalar(grp$database_identifier[[1L]]),
        chemical_formula = .mztab_norm_scalar(grp$chemical_formula[[1L]]),
        smiles = .mztab_norm_scalar(grp$smiles[[1L]]),
        inchi = .mztab_norm_scalar(grp$inchi[[1L]]),
        chemical_name = .mztab_norm_scalar(grp$chemical_name[[1L]]),
        uri = .mztab_norm_scalar(grp$uri[[1L]]),
        theoretical_neutral_mass = theoretical_neutral_mass,
        adduct_ions = .mztab_norm_scalar(grp$adduct_ion[[1L]]),
        reliability = reliability,
        best_id_confidence_measure = "id_confidence_measure[1]",
        best_id_evidence_value = score_val
      )
      next
    }

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

    sml_rows[[j]] <- list(
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
  }

  out <- data.table::rbindlist(sml_rows, use.names = TRUE, fill = TRUE)
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Read base mzTab while preserving unmanaged lines
#'
#' Collects passthrough content as every non-blank line that does not belong to
#' the managed TIMA sections (`MTD`, `SMH/SML`, `SFH/SMF`, `SEH/SME`). This
#' includes custom extension blocks and comment rows from anywhere in the file.
#' Passthrough lines are emitted before the TIMA-managed metadata section.
#'
#' @keywords internal
