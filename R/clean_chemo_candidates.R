#' Resolve tied candidates via cross-feature neutral-mass anchors
#'
#' @include clean_chemo_preprocessing.R
#' @include constants.R
#' @include logs_utils.R
#'
#' @description Splits candidates per (feature_id, candidate_adduct,
#'     rank_final) into untied rows (always kept) and tied rows. A tied
#'     group is collapsed to just the candidates whose InChIKey matches
#'     a rank-1 InChIKey of ANOTHER feature at the same neutral mass
#'     (within tolerance). Tied groups with NO cross-feature anchor are
#'     NOT dropped: they are kept as-is if `.n_per_group <= max_per_score`,
#'     otherwise sampled down to `max_per_score` (RT-error candidates
#'     prioritized when the column is available).
#'
#' @param df_tied Data frame with the tied candidate rows to evaluate.
#' @param anchor_tbl Data frame with cross-feature neutral-mass anchor rows.
#' @param tol Numeric mass tolerance in Daltons for matching anchors.
#' @param rt_tol Numeric retention-time tolerance in minutes for anchor
#'   matching when feature RT values are available.
#' @param has_rt_feature_col Logical, whether the input data frames include an
#'   RT column that can be used for anchor matching.
#'
#' @return Logical vector marking rows that have a valid cross-feature anchor.
#' @keywords internal

.match_tied_rows_to_anchors <- function(
  df_tied,
  anchor_tbl,
  tol,
  rt_tol,
  has_rt_feature_col
) {
  if (nrow(df_tied) == 0L || nrow(anchor_tbl) == 0L) {
    return(rep(FALSE, nrow(df_tied)))
  }

  row_mass_col <- if (".candidate_anchor_mass" %in% names(df_tied)) {
    ".candidate_anchor_mass"
  } else {
    ".candidate_M"
  }
  anchor_mass_col <- if (".candidate_anchor_mass" %in% names(anchor_tbl)) {
    ".candidate_anchor_mass"
  } else {
    ".candidate_M"
  }

  row_dt <- data.frame(
    row_id = seq_len(nrow(df_tied)),
    row_IK = as.character(
      df_tied$candidate_structure_inchikey_connectivity_layer
    ),
    row_fid = as.character(df_tied$feature_id),
    row_rt = if (has_rt_feature_col) {
      as.numeric(df_tied$rt)
    } else {
      NA_real_
    },
    row_mass = as.numeric(df_tied[[row_mass_col]]),
    row_lower = as.numeric(df_tied[[row_mass_col]]) - tol,
    row_upper = as.numeric(df_tied[[row_mass_col]]) + tol,
    stringsAsFactors = FALSE
  )

  anchor_dt <- data.frame(
    anchor_IK = as.character(
      anchor_tbl$candidate_structure_inchikey_connectivity_layer
    ),
    anchor_fid = as.character(anchor_tbl$feature_id),
    anchor_rt = if (has_rt_feature_col) {
      as.numeric(anchor_tbl$rt)
    } else {
      NA_real_
    },
    anchor_mass = as.numeric(anchor_tbl[[anchor_mass_col]]),
    stringsAsFactors = FALSE
  )

  row_dt <- row_dt[!is.na(row_dt$row_mass), ]
  anchor_dt <- anchor_dt[
    !is.na(anchor_dt$anchor_mass) & !is.na(anchor_dt$anchor_IK),
  ]

  if (nrow(row_dt) == 0L || nrow(anchor_dt) == 0L) {
    return(rep(FALSE, nrow(df_tied)))
  }

  anchor_match <- rep(FALSE, nrow(row_dt))

  # Pre-group anchors by InChIKey for O(1) lookup
  anchor_groups <- split(seq_len(nrow(anchor_dt)), anchor_dt$anchor_IK)
  row_groups <- split(seq_len(nrow(row_dt)), row_dt$row_IK)

  common_IK <- intersect(names(row_groups), names(anchor_groups))
  if (length(common_IK) == 0L) {
    return(rep(FALSE, nrow(df_tied)))
  }

  # Pre-sort anchors by mass within each InChIKey group
  for (ik in common_IK) {
    anchor_idx <- anchor_groups[[ik]]
    anchor_mass_sorted <- anchor_dt$anchor_mass[anchor_idx]
    anchor_order <- order(anchor_mass_sorted)
    anchor_groups[[ik]] <- list(
      idx = anchor_idx[anchor_order],
      mass = anchor_mass_sorted[anchor_order],
      fid = anchor_dt$anchor_fid[anchor_idx[anchor_order]],
      rt = anchor_dt$anchor_rt[anchor_idx[anchor_order]]
    )
  }

  # Vectorized matching using findInterval
  for (ik in common_IK) {
    row_idx <- row_groups[[ik]]
    anchor_info <- anchor_groups[[ik]]

    row_mass <- row_dt$row_mass[row_idx]
    row_lower <- row_dt$row_lower[row_idx]
    row_upper <- row_dt$row_upper[row_idx]
    row_fids <- row_dt$row_fid[row_idx]
    row_rts <- row_dt$row_rt[row_idx]

    valid_rows <- which(
      is.finite(row_mass) & is.finite(row_lower) & is.finite(row_upper)
    )
    if (length(valid_rows) == 0L) {
      next
    }

    row_idx <- row_idx[valid_rows]
    row_mass <- row_mass[valid_rows]
    row_lower <- row_lower[valid_rows]
    row_upper <- row_upper[valid_rows]
    row_fids <- row_fids[valid_rows]
    row_rts <- row_rts[valid_rows]

    n_anchor <- length(anchor_info$mass)
    start_idx <- findInterval(
      row_lower,
      anchor_info$mass,
      left.open = FALSE,
      rightmost.closed = TRUE
    ) +
      1L
    start_idx <- pmin(pmax(start_idx, 1L), n_anchor + 1L)
    end_idx <- findInterval(
      row_upper,
      anchor_info$mass,
      left.open = TRUE,
      rightmost.closed = TRUE
    )
    end_idx[!is.finite(end_idx)] <- 0L
    end_idx[end_idx < 1L] <- 0L
    end_idx[end_idx > n_anchor] <- n_anchor

    keep_mask <- start_idx <= end_idx & end_idx >= 1L
    if (!any(keep_mask)) {
      next
    }

    row_idx <- row_idx[keep_mask]
    row_fids <- row_fids[keep_mask]
    row_rts <- row_rts[keep_mask]
    start_idx <- start_idx[keep_mask]
    end_idx <- end_idx[keep_mask]

    if (has_rt_feature_col) {
      # RT-aware matching: check if any anchor in window has different fid and matching RT
      for (ri in seq_along(row_idx)) {
        if (is.na(row_rts[[ri]])) {
          anchor_match[row_idx[[ri]]] <- TRUE
          next
        }

        candidate_start <- start_idx[[ri]]
        candidate_end <- end_idx[[ri]]
        if (candidate_start > candidate_end) {
          next
        }

        anchor_window <- seq.int(candidate_start, candidate_end)
        candidate_anchor_fids <- anchor_info$fid[anchor_window]
        candidate_anchor_rts <- anchor_info$rt[anchor_window]
        keep_mask_inner <- candidate_anchor_fids != row_fids[[ri]]
        candidate_anchor_fids <- candidate_anchor_fids[keep_mask_inner]
        candidate_anchor_rts <- candidate_anchor_rts[keep_mask_inner]

        if (length(candidate_anchor_fids) == 0L) {
          next
        }

        rt_ok <- is.na(candidate_anchor_rts) |
          abs(candidate_anchor_rts - row_rts[[ri]]) <= rt_tol
        if (any(rt_ok)) {
          anchor_match[row_idx[[ri]]] <- TRUE
        }
      }
    } else {
      # No RT column: just check for any anchor with different fid
      for (ri in seq_along(row_idx)) {
        candidate_start <- start_idx[[ri]]
        candidate_end <- end_idx[[ri]]
        if (candidate_start > candidate_end) {
          next
        }

        anchor_window <- seq.int(candidate_start, candidate_end)
        candidate_anchor_fids <- anchor_info$fid[anchor_window]
        candidate_anchor_fids <- candidate_anchor_fids[
          candidate_anchor_fids != row_fids[[ri]]
        ]
        if (length(candidate_anchor_fids) > 0L) {
          anchor_match[row_idx[[ri]]] <- TRUE
        }
      }
    }
  }

  out <- rep(FALSE, nrow(df_tied))
  out[row_dt$row_id] <- anchor_match
  out
}

#' Sample tied candidates within each feature/adduct group
#'
#' @description Keeps the best-supported candidate rows within each
#'   feature/adduct group, optionally collapsing rank-1 anchors to preserve
#'   cross-feature consensus candidates before sampling.
#'
#' @param df Data frame with ranked candidates.
#' @param max_per_score Integer, maximum candidates to keep per group.
#' @param seed Integer, random seed used for reproducibility when sampling.
#' @param apply_anchor_collapsing Logical, whether to preserve candidates that
#'   match cross-feature neutral-mass anchors before sampling.
#'
#' @return List with the sampled data frame, the number of sampled groups, and
#'   per-group annotation notes.
#' @keywords internal
sample_candidates_per_group <- function(
  df,
  max_per_score,
  seed = 42L,
  apply_anchor_collapsing = TRUE
) {
  # New policy:
  # - Only tied rows (same feature_id, candidate_adduct, rank_final) are sampled.
  # - Untied rows are always kept.
  # - At most `max_per_score` rows are kept per (feature_id, candidate_adduct, rank_final).
  # - Cluster-anchor promoted rows are preserved before sampling.
  # - Sampling prefers MS2 evidence and RT-priority where available.

  if (nrow(df) == 0L) {
    return(list(
      df = df,
      n_sampled_features = 0L,
      annotation_notes = tidytable::tidytable()
    ))
  }

  # Ensure rank_final exists (rank_and_deduplicate should create it)
  if (!"rank_final" %in% names(df)) {
    df$rank_final <- NA_integer_
  }

  # Compute per-score-group sizes (feature/adduct/score-rank) only once
  df <- df |>
    tidytable::mutate(
      .n_per_score = tidytable::n(),
      .by = c(feature_id, candidate_adduct, rank_final),
      .anchor_match = FALSE
    )

  has_ik_col <- "candidate_structure_inchikey_connectivity_layer" %in% names(df)
  has_M_cols <- "mz" %in% names(df) && "candidate_adduct" %in% names(df)
  has_rt_feature_col <- "rt" %in% names(df)

  if (has_M_cols) {
    df <- df |>
      tidytable::mutate(
        .candidate_M = compute_candidate_M(mz, candidate_adduct)
      )
  }

  df_anchor_kept <- tidytable::tidytable()
  if (apply_anchor_collapsing && has_ik_col && has_M_cols) {
    # anchors must come from strong rank-1 singletons (best-supported)
    anchor_tbl_base <- df |>
      tidytable::filter(
        rank_final == 1L,
        .n_per_score == 1L,
        !is.na(candidate_structure_inchikey_connectivity_layer),
        !is.na(.candidate_M)
      ) |>
      tidytable::arrange(.candidate_M)

    anchor_tbl <- if (has_rt_feature_col) {
      anchor_tbl_base |>
        tidytable::select(
          .candidate_M,
          candidate_structure_inchikey_connectivity_layer,
          feature_id,
          rt
        )
    } else {
      anchor_tbl_base |>
        tidytable::select(
          .candidate_M,
          candidate_structure_inchikey_connectivity_layer,
          feature_id
        )
    }

    if (nrow(anchor_tbl) > 0L) {
      tol <- NEUTRAL_MASS_MATCH_TOLERANCE_DA
      rt_tol <- DEFAULT_HE_MAX_RT_ERROR_MIN
      anchor_match <- .match_tied_rows_to_anchors(
        df_tied = df,
        anchor_tbl = anchor_tbl,
        tol = tol,
        rt_tol = rt_tol,
        has_rt_feature_col = has_rt_feature_col
      )
      df <- df |>
        tidytable::mutate(.anchor_match = anchor_match)

      has_consensus_flag <- "cluster_consensus_promoted_from_anchor" %in%
        names(df)
      df_anchor_kept <- df |>
        tidytable::filter(.anchor_match) |>
        tidytable::arrange(
          feature_id,
          candidate_adduct,
          rank_final,
          tidytable::desc(score_weighted_chemo),
          tidytable::desc(candidate_score_pseudo_initial)
        ) |>
        tidytable::mutate(
          cluster_consensus_promoted_from_anchor = if (has_consensus_flag) {
            cluster_consensus_promoted_from_anchor
          } else {
            FALSE
          },
          annotation_note = paste0(
            "Kept candidate matching best-supported InChIKey from sibling ",
            "feature with same neutral mass (different adduct)"
          )
        )
    }
  }

  # Remove whole adduct groups that have preserved anchors so sampling does not
  # accidentally drop an anchor
  anchor_group_keys <- df |>
    tidytable::filter(.anchor_match) |>
    tidytable::distinct(feature_id, candidate_adduct)

  df_remaining <- if (nrow(anchor_group_keys) > 0L) {
    df |>
      tidytable::anti_join(
        y = anchor_group_keys,
        by = c("feature_id", "candidate_adduct")
      )
  } else {
    df
  }

  # Count how many distinct tied groups require sampling
  n_sampled_features <- df_remaining |>
    tidytable::filter(.n_per_score > max_per_score) |>
    tidytable::distinct(feature_id) |>
    nrow()

  has_rt_col <- "candidate_structure_error_rt" %in% names(df_remaining)

  # Partition into: untied (single candidate per score), small tied groups (<=max), and large tied groups (>max)
  df_untied <- df_remaining |>
    tidytable::filter(.n_per_score == 1L) |>
    tidytable::as_tidytable()

  df_keep_remaining <- df_remaining |>
    tidytable::filter(.n_per_score > 1L & .n_per_score <= max_per_score) |>
    tidytable::as_tidytable()

  df_needs_sampling <- df_remaining |>
    tidytable::filter(.n_per_score > max_per_score) |>
    tidytable::as_tidytable()

  # Ordering function: prefer MS2 evidence, then anchors, RT-priority, then scores
  sort_candidates_for_sampling <- function(tbl, prefer_rt = FALSE) {
    if (nrow(tbl) == 0L) {
      return(tbl)
    }

    order_terms <- list()

    # Prefer rows with MS2 evidence: candidate_score_similarity or candidate_score_sirius_csi or confidence
    has_ms2 <- NULL
    if ("candidate_score_similarity" %in% names(tbl)) {
      order_terms[[length(order_terms) + 1L]] <- -suppressWarnings(as.numeric(
        tbl$candidate_score_similarity
      ))
    }
    if ("candidate_score_sirius_csi" %in% names(tbl)) {
      order_terms[[length(order_terms) + 1L]] <- -suppressWarnings(as.numeric(
        tbl$candidate_score_sirius_csi
      ))
    }
    if ("candidate_score_sirius_confidence" %in% names(tbl)) {
      order_terms[[length(order_terms) + 1L]] <- -suppressWarnings(as.numeric(
        tbl$candidate_score_sirius_confidence
      ))
    }

    if (isTRUE(prefer_rt) && ".rt_priority" %in% names(tbl)) {
      order_terms[[length(order_terms) + 1L]] <- -as.integer(tbl$.rt_priority)
    }

    if ("cluster_consensus_promoted_from_anchor" %in% names(tbl)) {
      order_terms[[length(order_terms) + 1L]] <- -as.integer(
        !is.na(tbl$cluster_consensus_promoted_from_anchor) &
          tbl$cluster_consensus_promoted_from_anchor
      )
    }

    # Fallback ordering by weighted chemo score and supporting evidence
    score_columns <- c(
      "score_weighted_chemo",
      "score_weighted_chemo_coverage",
      "candidate_score_pseudo_initial",
      "candidate_score_similarity_forward",
      "candidate_score_similarity_reverse"
    )
    for (score_col in score_columns) {
      if (score_col %in% names(tbl)) {
        order_terms[[
          length(order_terms) + 1L
        ]] <- -suppressWarnings(as.numeric(tbl[[score_col]]))
      }
    }

    if (length(order_terms) == 0L) {
      return(tbl)
    }

    ord <- do.call(
      order,
      c(order_terms, list(na.last = TRUE, method = "radix"))
    )
    tbl[ord, , drop = FALSE]
  }

  # Sample only within each tied score group (feature_id, candidate_adduct, rank_final)
  if (nrow(df_needs_sampling) > 0L) {
    if (has_rt_col) {
      df_needs_sampling <- df_needs_sampling |>
        tidytable::mutate(.rt_priority = !is.na(candidate_structure_error_rt))
    }

    # Preserve the original tied-group sizes for annotation notes
    group_sizes <- df_remaining |>
      tidytable::distinct(
        feature_id,
        candidate_adduct,
        rank_final,
        .n_per_score
      ) |>
      tidytable::mutate(n_per_score = .n_per_score) |>
      tidytable::select(-.n_per_score)

    # Use tidytable for deterministic grouped sampling after ordering
    df_ns <- df_needs_sampling

    # Create temporary ordering columns (negative values so ascending sort prefers high scores)
    if ("candidate_score_similarity" %in% names(df_ns)) {
      df_ns <- df_ns |>
        tidytable::mutate(.ord1 = -as.numeric(candidate_score_similarity))
    }
    if ("candidate_score_sirius_csi" %in% names(df_ns)) {
      df_ns <- df_ns |>
        tidytable::mutate(.ord2 = -as.numeric(candidate_score_sirius_csi))
    }
    if ("candidate_score_sirius_confidence" %in% names(df_ns)) {
      df_ns <- df_ns |>
        tidytable::mutate(
          .ord3 = -as.numeric(candidate_score_sirius_confidence)
        )
    }
    if (has_rt_col) {
      df_ns <- df_ns |> tidytable::mutate(.ord_rt = -as.integer(.rt_priority))
    }
    if ("cluster_consensus_promoted_from_anchor" %in% names(df_ns)) {
      df_ns <- df_ns |>
        tidytable::mutate(
          .ord_cons = -as.integer(
            !is.na(cluster_consensus_promoted_from_anchor) &
              cluster_consensus_promoted_from_anchor
          )
        )
    }
    if ("score_weighted_chemo" %in% names(df_ns)) {
      df_ns <- df_ns |>
        tidytable::mutate(.ord_sc = -as.numeric(score_weighted_chemo))
    }
    if ("score_weighted_chemo_coverage" %in% names(df_ns)) {
      df_ns <- df_ns |>
        tidytable::mutate(.ord_cov = -as.numeric(score_weighted_chemo_coverage))
    }
    if ("candidate_score_pseudo_initial" %in% names(df_ns)) {
      df_ns <- df_ns |>
        tidytable::mutate(.ord_pi = -as.numeric(candidate_score_pseudo_initial))
    }

    # Determine ordering columns present
    ord_cols <- intersect(
      c(
        '.ord1',
        '.ord2',
        '.ord3',
        '.ord_rt',
        '.ord_cons',
        '.ord_sc',
        '.ord_cov',
        '.ord_pi'
      ),
      names(df_ns)
    )

    # Order by grouping keys and the computed ordering columns
    order_terms <- c('feature_id', 'candidate_adduct', 'rank_final', ord_cols)
    ord_list <- lapply(order_terms, function(x) df_ns[[x]])
    ord <- do.call(order, c(ord_list, list(na.last = TRUE, method = "radix")))
    df_ns <- df_ns[ord, , drop = FALSE]

    # Keep top `max_per_score` within each tied score group (feature_id, candidate_adduct, rank_final)
    df_sampled <- df_ns |>
      tidytable::group_by(feature_id, candidate_adduct, rank_final) |>
      tidytable::slice_head(n = max_per_score) |>
      tidytable::ungroup()

    # Clean temporary ordering columns
    tmp_ord_cols <- intersect(
      c(
        '.ord1',
        '.ord2',
        '.ord3',
        '.ord_rt',
        '.ord_cons',
        '.ord_sc',
        '.ord_cov',
        '.ord_pi'
      ),
      names(df_sampled)
    )
    if (length(tmp_ord_cols) > 0) {
      df_sampled <- df_sampled |>
        tidytable::select(-tidyselect::all_of(tmp_ord_cols))
    }
    if ('.rt_priority' %in% names(df_sampled)) {
      df_sampled <- df_sampled |> tidytable::select(-.rt_priority)
    }

    # Ensure tidytable result
    df_sampled <- tidytable::as_tidytable(df_sampled)

    # Annotate sampling notes using the original group size
    df_sampled <- df_sampled |>
      tidytable::left_join(
        y = group_sizes,
        by = c("feature_id", "candidate_adduct", "rank_final")
      ) |>
      tidytable::mutate(
        annotation_note = paste0(
          "Sampled ",
          max_per_score,
          " of ",
          .n_per_score,
          " tied candidates"
        )
      )
  } else {
    df_sampled <- tidytable::tidytable()
  }

  df_result <- tidytable::bind_rows(
    df_untied,
    df_keep_remaining,
    df_anchor_kept,
    df_sampled
  ) |>
    tidytable::select(
      -tidyselect::any_of(c(
        ".n_per_score",
        "n_per_score",
        ".anchor_match",
        ".candidate_M"
      ))
    )

  annotation_notes_lookup <- tidytable::tidytable()
  if ("annotation_note" %in% names(df_result)) {
    annotation_notes_lookup <- df_result |>
      tidytable::filter(!is.na(annotation_note)) |>
      tidytable::distinct(feature_id, candidate_adduct, annotation_note) |>
      tidytable::group_by(feature_id, candidate_adduct) |>
      tidytable::summarize(
        annotation_note = paste(unique(annotation_note), collapse = " | "),
        .groups = "drop"
      )

    df_result <- df_result |>
      tidytable::select(-tidyselect::any_of("annotation_note"))
  }

  list(
    df = df_result,
    n_sampled_features = n_sampled_features,
    annotation_notes = annotation_notes_lookup
  )
}

#' Remove Compound Names from Results
#'
#' @description Internal helper to optionally remove compound names from
#'     all result tiers.
#'
#' @param results_list Named list with full, filtered, mini data frames
#' @param compounds_names Logical, if FALSE remove names
#'
#' @return Modified results list
#' @keywords internal
remove_compound_names <- function(results_list, compounds_names) {
  if (!compounds_names) {
    results_list$mini <- results_list$mini |>
      tidytable::select(
        -tidyselect::any_of(x = "candidate_structure_name")
      )
    results_list$filtered <- results_list$filtered |>
      tidytable::select(
        -tidyselect::any_of(x = "candidate_structure_name")
      )
    results_list$full <- results_list$full |>
      tidytable::select(
        -tidyselect::any_of(x = "candidate_structure_name")
      )
  }
  results_list
}

#' Coerce score columns to numeric
#'
#' @description Internal helper that normalizes score-like columns loaded as
#'     character values (e.g., from TSV/CSV inputs) before numeric operations.
#'
#' @param annot_table_wei_chemo Data frame with annotation scores
#'
#' @return Data frame with existing score columns coerced to numeric
#' @keywords internal
coerce_score_columns <- function(annot_table_wei_chemo) {
  score_columns <- c(
    "score_biological",
    "score_chemical",
    "score_weighted_bio",
    "score_weighted_bio_coverage",
    "score_weighted_chemo",
    "score_weighted_chemo_coverage",
    "candidate_score_pseudo_initial",
    "candidate_score_similarity",
    "candidate_score_similarity_forward",
    "candidate_score_similarity_reverse",
    "candidate_score_sirius_csi",
    "candidate_score_sirius_confidence"
  )

  cols_to_convert <- intersect(score_columns, names(annot_table_wei_chemo))
  if (length(cols_to_convert) == 0L) {
    return(annot_table_wei_chemo)
  }

  annot_table_wei_chemo |>
    tidytable::mutate(
      tidytable::across(
        .cols = tidyselect::all_of(cols_to_convert),
        .fns = as.numeric
      )
    )
}

#' Prepare ranked candidate tables for downstream tiers
#'
#' @param annot_table_wei_chemo Data frame with normalized score columns
#' @param minimal_ms1_bio Numeric threshold for biological score
#' @param minimal_ms1_chemo Numeric threshold for chemical score
#' @param minimal_ms1_condition Character filter mode ("OR" or "AND")
#' @param best_percentile Numeric percentile threshold
#' @param max_per_score Integer max candidates per score group
#' @param components_table Optional data frame with feature_id and component_id
#' @param enforce_cluster_consensus Logical, apply cluster-consensus promotion
#'     before sampling and percentile filtering
#'
#' @return Named list with ranked/percentile tables and candidate counts
#' @keywords internal
prepare_ranked_candidates <- function(
  annot_table_wei_chemo,
  minimal_ms1_bio,
  minimal_ms1_chemo,
  minimal_ms1_condition,
  best_percentile,
  max_per_score,
  components_table = NULL,
  enforce_cluster_consensus = TRUE
) {
  df_base <- filter_ms1_annotations(
    annot_table_wei_chemo = annot_table_wei_chemo,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    minimal_ms1_condition = minimal_ms1_condition
  )

  df_ranked <- rank_and_deduplicate(df_base)

  # Cluster-consensus promotion is enabled by default for the candidate
  # ranking pipeline. The main clean_chemo workflow can disable it if a caller
  # needs the legacy behavior, but the promotion path is kept so consensus-
  # supported candidates survive sampling/percentile filtering when no stronger
  # local candidate exists.
  if (
    enforce_cluster_consensus &&
      !is.null(components_table) &&
      nrow(components_table) > 0L
  ) {
    df_ranked <- enforce_cluster_entity_consensus(
      df_ranked,
      components_table
    )
    log_debug("Applied cluster-level entity consensus filtering")
  }

  sampling_result <- sample_candidates_per_group(
    df = df_ranked,
    max_per_score = max_per_score,
    seed = 42L,
    apply_anchor_collapsing = (
      enforce_cluster_consensus && !is.null(components_table) &&
        nrow(components_table) > 0L
    )
  )

  df_ranked <- sampling_result$df
  df_percentile <- apply_percentile_filter(
    df = df_ranked,
    best_percentile = best_percentile
  )

  list(
    df_ranked = df_ranked,
    df_percentile = df_percentile,
    results_candidates = count_candidates(df_ranked, df_percentile),
    n_sampled_features = sampling_result$n_sampled_features,
    annotation_notes_lookup = sampling_result$annotation_notes
  )
}

#' Build mini-tier taxonomy table
#'
#' @param df_percentile Percentile-filtered candidate table
#' @param score_chemical_cla_kingdom ClassyFire kingdom weight
#' @param score_chemical_cla_superclass ClassyFire superclass weight
#' @param score_chemical_cla_class ClassyFire class weight
#' @param score_chemical_cla_parent ClassyFire direct parent weight
#' @param score_chemical_npc_pathway NPClassifier pathway weight
#' @param score_chemical_npc_superclass NPClassifier superclass weight
#' @param score_chemical_npc_class NPClassifier class weight
#'
#' @return Mini-tier taxonomy table keyed by feature_id
#' @keywords internal
build_mini_taxonomy_table <- function(
  df_percentile,
  score_chemical_cla_kingdom,
  score_chemical_cla_superclass,
  score_chemical_cla_class,
  score_chemical_cla_parent,
  score_chemical_npc_pathway,
  score_chemical_npc_superclass,
  score_chemical_npc_class
) {
  if (!"score_weighted_chemo" %in% names(df_percentile)) {
    df_percentile$score_weighted_chemo <- NA_real_
  }
  required_tax_cols <- c(
    "candidate_structure_tax_cla_04dirpar",
    "candidate_structure_tax_cla_03cla",
    "candidate_structure_tax_cla_02sup",
    "candidate_structure_tax_cla_01kin",
    "candidate_structure_tax_npc_03cla",
    "candidate_structure_tax_npc_02sup",
    "candidate_structure_tax_npc_01pat"
  )
  missing_tax_cols <- setdiff(required_tax_cols, names(df_percentile))
  if (length(missing_tax_cols) > 0L) {
    for (col in missing_tax_cols) {
      df_percentile[[col]] <- NA_character_
    }
  }
  required_pred_tax_cols <- c(
    "feature_pred_tax_cla_01kin_val",
    "feature_pred_tax_cla_01kin_score",
    "feature_pred_tax_cla_02sup_val",
    "feature_pred_tax_cla_02sup_score",
    "feature_pred_tax_cla_03cla_val",
    "feature_pred_tax_cla_03cla_score",
    "feature_pred_tax_cla_04dirpar_val",
    "feature_pred_tax_cla_04dirpar_score",
    "feature_pred_tax_npc_01pat_val",
    "feature_pred_tax_npc_01pat_score",
    "feature_pred_tax_npc_02sup_val",
    "feature_pred_tax_npc_02sup_score",
    "feature_pred_tax_npc_03cla_val",
    "feature_pred_tax_npc_03cla_score"
  )
  missing_pred_tax_cols <- setdiff(required_pred_tax_cols, names(df_percentile))
  if (length(missing_pred_tax_cols) > 0L) {
    missing_pred_val_cols <- grep("_val$", missing_pred_tax_cols, value = TRUE)
    missing_pred_score_cols <- grep(
      "_score$",
      missing_pred_tax_cols,
      value = TRUE
    )
    if (length(missing_pred_val_cols) > 0L) {
      for (col in missing_pred_val_cols) {
        df_percentile[[col]] <- NA_character_
      }
    }
    if (length(missing_pred_score_cols) > 0L) {
      for (col in missing_pred_score_cols) {
        df_percentile[[col]] <- NA_real_
      }
    }
  }

  normalize_tax_label <- function(x) {
    vals <- trimws(as.character(x))
    vals[
      vals %in% c("", "notClassified", "empty", "N/A", "null")
    ] <- NA_character_
    vals
  }

  # Prepare all candidates with their structure taxonomy scored by
  # score_weighted_chemo so they compete on equal footing with predicted labels.
  df_all <- df_percentile |>
    tidytable::mutate(
      score_weighted_chemo = as.numeric(score_weighted_chemo),
      has_inchikey = !is.na(candidate_structure_inchikey_connectivity_layer),
      tidytable::across(
        .cols = tidyselect::starts_with(match = "candidate_structure_tax_"),
        .fns = normalize_tax_label
      ),
      .label_cla_str = tidytable::coalesce(
        candidate_structure_tax_cla_04dirpar,
        candidate_structure_tax_cla_03cla,
        candidate_structure_tax_cla_02sup,
        candidate_structure_tax_cla_01kin
      ),
      .label_npc_str = tidytable::coalesce(
        candidate_structure_tax_npc_03cla,
        candidate_structure_tax_npc_02sup,
        candidate_structure_tax_npc_01pat
      )
    )

  # has_inchikey flag at feature level
  df_has_ik <- df_all |>
    tidytable::summarize(
      has_inchikey = any(has_inchikey, na.rm = TRUE),
      .by = feature_id
    )

  # Best structure label per feature (highest score_weighted_chemo wins)
  df_str_cla <- df_all |>
    tidytable::filter(!is.na(.label_cla_str)) |>
    tidytable::arrange(tidytable::desc(score_weighted_chemo)) |>
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::select(
      feature_id,
      label_classyfire_str = .label_cla_str,
      score_cla_str = score_weighted_chemo
    )

  df_str_npc <- df_all |>
    tidytable::filter(!is.na(.label_npc_str)) |>
    tidytable::arrange(tidytable::desc(score_weighted_chemo)) |>
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::select(
      feature_id,
      label_npclassifier_str = .label_npc_str,
      score_npc_str = score_weighted_chemo
    )

  # Best predicted label per feature (highest prediction score wins)
  df_pred_tax <- df_percentile |>
    tidytable::select(
      tidyselect::contains(match = c("feature_id", "feature_pred"))
    ) |>
    tidytable::mutate(
      tidytable::across(
        tidyselect::contains(match = "score"),
        as.numeric
      )
    )

  if (nrow(df_pred_tax) > 0L) {
    weights <- list(
      w_cla_kin = score_chemical_cla_kingdom,
      w_cla_sup = score_chemical_cla_superclass,
      w_cla_cla = score_chemical_cla_class,
      w_cla_par = score_chemical_cla_parent,
      w_npc_pat = score_chemical_npc_pathway,
      w_npc_sup = score_chemical_npc_superclass,
      w_npc_cla = score_chemical_npc_class
    )

    df_pred_cla <- compute_classyfire_taxonomy(df_pred_tax, weights) |>
      tidytable::arrange(tidytable::desc(score_classyfire)) |>
      tidytable::distinct(feature_id, .keep_all = TRUE)

    df_pred_npc <- compute_npclassifier_taxonomy(df_pred_tax, weights) |>
      tidytable::arrange(tidytable::desc(score_npclassifier)) |>
      tidytable::distinct(feature_id, .keep_all = TRUE)
  } else {
    df_pred_cla <- tidytable::tidytable(
      feature_id = character(0),
      label_classyfire_predicted = character(0),
      score_classyfire = numeric(0)
    )
    df_pred_npc <- tidytable::tidytable(
      feature_id = character(0),
      label_npclassifier_predicted = character(0),
      score_npclassifier = numeric(0)
    )
  }

  # pmax across both pools: structure label wins when its score_weighted_chemo
  # >= prediction score, otherwise the predicted label wins.
  # Combine all joins into single operation to avoid multiple table scans
  joined <- purrr::reduce(
    .x = list(df_str_cla, df_str_npc, df_pred_cla, df_pred_npc),
    .init = df_has_ik,
    .f = function(acc, tbl) tidytable::left_join(x = acc, y = tbl)
  )

  joined |>
    tidytable::mutate(
      label_classyfire = tidytable::case_when(
        !is.na(score_cla_str) &
          !is.na(score_classyfire) &
          score_cla_str >= score_classyfire ~ label_classyfire_str,
        !is.na(score_classyfire) ~ label_classyfire_predicted,
        !is.na(label_classyfire_str) ~ label_classyfire_str,
        TRUE ~ NA_character_
      ),
      # No uncertainty score when label comes from a confirmed InChIKey structure
      score_classyfire = tidytable::case_when(
        !is.na(score_cla_str) &
          !is.na(score_classyfire) &
          score_cla_str >= score_classyfire ~ NA_real_,
        !is.na(score_classyfire) ~ score_classyfire,
        TRUE ~ NA_real_
      ),
      label_npclassifier = tidytable::case_when(
        !is.na(score_npc_str) &
          !is.na(score_npclassifier) &
          score_npc_str >= score_npclassifier ~ label_npclassifier_str,
        !is.na(score_npclassifier) ~ label_npclassifier_predicted,
        !is.na(label_npclassifier_str) ~ label_npclassifier_str,
        TRUE ~ NA_character_
      ),
      score_npclassifier = tidytable::case_when(
        !is.na(score_npc_str) &
          !is.na(score_npclassifier) &
          score_npc_str >= score_npclassifier ~ NA_real_,
        !is.na(score_npclassifier) ~ score_npclassifier,
        TRUE ~ NA_real_
      )
    ) |>
    tidytable::select(
      feature_id,
      has_inchikey,
      label_classyfire,
      label_npclassifier,
      score_classyfire,
      score_npclassifier
    ) |>
    tidytable::distinct()
}

#' Build mini-tier result table
#'
#' @param features_table Feature metadata table
#' @param df_classes_mini Mini taxonomy table keyed by feature_id
#' @param results_filtered Filtered-tier summarized results
#' @param df_filtered Filtered candidate table prior to summarization
#' @param xrefs_table Optional cross-reference table
#'
#' @return Mini-tier output table
#' @keywords internal
build_mini_results_table <- function(
  features_table,
  df_classes_mini,
  results_filtered,
  df_filtered,
  xrefs_table = NULL
) {
  results_mini <- purrr::reduce(
    .x = list(
      df_classes_mini,
      results_filtered,
      df_filtered |>
        tidytable::select(
          tidyselect::any_of(c(
            "feature_id",
            "candidate_structure_name",
            "candidate_adduct",
            "candidate_structure_smiles_no_stereo",
            "candidate_structure_inchikey_connectivity_layer",
            "candidate_library",
            "candidate_structure_error_mz",
            "candidate_structure_error_rt",
            "candidate_structure_organism_occurrence_closest",
            "candidate_structure_tag",
            "score_weighted_chemo"
          ))
        )
    ),
    .init = features_table,
    .f = function(acc, tbl) tidytable::left_join(x = acc, y = tbl)
  ) |>
    tidytable::rename(
      label_compound = candidate_structure_name,
      adduct = candidate_adduct,
      smiles_no_stereo = candidate_structure_smiles_no_stereo,
      inchikey_connectivity_layer = candidate_structure_inchikey_connectivity_layer,
      library = candidate_library,
      error_mz = candidate_structure_error_mz,
      error_rt = candidate_structure_error_rt,
      organism_closest = candidate_structure_organism_occurrence_closest,
      score = score_weighted_chemo
    ) |>
    tidytable::mutate(
      candidates_evaluated = tidytable::if_else(
        is.na(inchikey_connectivity_layer),
        NA_integer_,
        candidates_evaluated
      ),
      candidates_best = tidytable::if_else(
        is.na(inchikey_connectivity_layer),
        NA_integer_,
        candidates_best
      ),
      .pred_denom = as.numeric(!is.na(score_classyfire)) +
        as.numeric(!is.na(score_npclassifier)),
      .pred_sum = tidytable::coalesce(score_classyfire, 0) +
        tidytable::coalesce(score_npclassifier, 0),
      .pred_avg = tidytable::if_else(
        .pred_denom > 0,
        .pred_sum / .pred_denom,
        NA_real_
      ),
      score = tidytable::if_else(
        is.na(inchikey_connectivity_layer),
        .pred_avg,
        score
      )
    ) |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          "feature_id",
          "rt",
          "mz",
          "label_classyfire",
          "label_npclassifier",
          "label_compound",
          "adduct",
          "smiles_no_stereo",
          "inchikey_connectivity_layer",
          "library",
          "error_mz",
          "error_rt",
          "organism_closest",
          "tag" = "candidate_structure_tag",
          "score",
          "candidates_evaluated",
          "candidates_best",
          "note" = "annotation_note"
        )
      )
    ) |>
    tidytable::distinct() |>
    tidytable::mutate(
      label_classyfire = tidytable::if_else(
        condition = !is.na(score),
        true = label_classyfire,
        false = NA_character_
      ),
      label_npclassifier = tidytable::if_else(
        condition = !is.na(score),
        true = label_npclassifier,
        false = NA_character_
      )
    )

  if (!is.null(xrefs_table) && nrow(xrefs_table) > 0L) {
    results_mini <- results_mini |>
      tidytable::rename(
        candidate_structure_inchikey_connectivity_layer = inchikey_connectivity_layer
      ) |>
      .add_xrefs_to_df(xrefs = xrefs_table) |>
      tidytable::rename(
        inchikey_connectivity_layer = candidate_structure_inchikey_connectivity_layer
      )

    mini_id_cols <- grep(
      pattern = "^candidate_structure_id_",
      x = names(results_mini),
      value = TRUE
    )

    if (length(mini_id_cols) > 0L) {
      results_mini <- results_mini |>
        tidytable::rename_with(
          ~ sub("^candidate_structure_", "", .x),
          .cols = tidyselect::all_of(mini_id_cols)
        )
    }
  }

  results_mini
}

#' Enforce cluster-level entity consensus
#'
#' @description Ensures that all features within the same MS1 component/cluster
#'     with the same neutral mass M must agree on their best InChIKey candidate.
#'     This is a fundamental design constraint: if multiple features form an
#'     MS1-based cluster via adduct/loss edges, they should all resolve to the
#'     same chemical entity.
#'
#'     For each (component_id, neutral_M) group:
#'     - Identify the best-ranked feature by score_weighted_chemo
#'     - Extract its best InChIKey from rank_final == 1
#'     - All other features in the group reorder their rank=1 candidates
#'       such that the anchoring InChIKey becomes rank=1 (if present)
#'
#' @param df_ranked Data frame with ranked candidates
#' @param components_table Data frame with feature_id and component_id columns
#'
#' @return df_ranked with cluster-consensus-enforced rankings
#' @keywords internal
enforce_cluster_entity_consensus <- function(df_ranked, components_table) {
  if (nrow(df_ranked) == 0L) {
    return(df_ranked)
  }

  # Check if required columns exist
  has_component_col <- "component_id" %in% names(components_table)
  has_rank_col <- "rank_final" %in% names(df_ranked)
  has_ik_col <- "candidate_structure_inchikey_connectivity_layer" %in%
    names(df_ranked)
  has_score_col <- "score_weighted_chemo" %in% names(df_ranked)
  has_exact_mass_col <- "candidate_structure_exact_mass" %in% names(df_ranked)
  # mz/adduct fallback retained for legacy data that lacks exact_mass
  has_mz_col <- "mz" %in% names(df_ranked)
  has_adduct_col <- "candidate_adduct" %in% names(df_ranked)
  has_M_source <- has_exact_mass_col || (has_mz_col && has_adduct_col)

  if (
    !all(c(
      has_component_col,
      has_rank_col,
      has_ik_col,
      has_score_col,
      has_M_source
    ))
  ) {
    log_debug(
      "Skipping cluster entity consensus (missing required columns: %s)",
      paste(
        c(
          "component_id",
          "rank_final",
          "InChIKey",
          "score",
          "candidate_structure_exact_mass (or mz + adduct)"
        )[
          !c(
            has_component_col,
            has_rank_col,
            has_ik_col,
            has_score_col,
            has_M_source
          )
        ],
        collapse = ", "
      )
    )
    return(df_ranked)
  }

  # Add component_id and neutral mass to ranked candidates
  df_with_comp <- df_ranked |>
    tidytable::left_join(
      y = components_table |> tidytable::distinct(feature_id, component_id),
      by = "feature_id"
    )

  has_similarity_col <- "candidate_score_similarity" %in% names(df_with_comp)
  has_sirius_csi_col <- "candidate_score_sirius_csi" %in% names(df_with_comp)
  has_sirius_conf_col <- "candidate_score_sirius_confidence" %in%
    names(df_with_comp)

  # Prefer anchors with direct MS2 evidence when available so consensus does
  # not systematically promote MS1-only rows to rank 1.
  df_with_comp <- df_with_comp |>
    tidytable::mutate(
      .has_ms2_evidence = (if (has_similarity_col) {
        !is.na(candidate_score_similarity)
      } else {
        FALSE
      }) |
        (if (has_sirius_csi_col) {
          !is.na(candidate_score_sirius_csi)
        } else {
          FALSE
        }) |
        (if (has_sirius_conf_col) {
          !is.na(candidate_score_sirius_confidence) &
            as.numeric(candidate_score_sirius_confidence) > 0
        } else {
          FALSE
        })
    )

  # Compute neutral mass for grouping.
  # Primary key: candidate_structure_exact_mass (formula-derived, adduct
  # independent). Row-level fallback: invert mz+adduct when exact mass is
  # missing, so partially populated exact-mass columns still propagate
  # consensus.
  has_mz_adduct_fallback <- has_mz_col && has_adduct_col
  if (!has_exact_mass_col) {
    log_warn(paste0(
      "candidate_structure_exact_mass not found; ",
      "falling back to mz/adduct inversion for neutral-mass grouping. ",
      "Results may be less reliable for cross-adduct consensus."
    ))
  }

  df_with_comp <- df_with_comp |>
    tidytable::mutate(
      .candidate_M_exact = if (has_exact_mass_col) {
        as.numeric(candidate_structure_exact_mass)
      } else {
        NA_real_
      },
      .candidate_M_mz = if (has_mz_adduct_fallback) {
        compute_candidate_M(mz, candidate_adduct)
      } else {
        NA_real_
      },
      .candidate_M = tidytable::coalesce(.candidate_M_exact, .candidate_M_mz),
      .candidate_M_key = round(.candidate_M, NEUTRAL_MASS_GROUP_DECIMALS)
    )

  if (has_exact_mass_col && has_mz_adduct_fallback) {
    n_fallback_rows <- df_with_comp |>
      tidytable::filter(is.na(.candidate_M_exact), !is.na(.candidate_M_mz)) |>
      nrow()
    if (n_fallback_rows > 0L) {
      log_info(
        "Cluster-consensus mass key used mz/adduct fallback for %d row(s) with missing exact mass",
        n_fallback_rows
      )
    }
  }

  # For each (component_id, .candidate_M_key) group with rank_final == 1
  # identify the anchor InChIKey from the best-scored feature
  anchor_lookup <- df_with_comp |>
    tidytable::filter(
      rank_final == 1L,
      !is.na(component_id),
      !is.na(.candidate_M_key),
      !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::arrange(
      component_id,
      .candidate_M_key,
      tidytable::desc(.has_ms2_evidence),
      tidytable::desc(score_weighted_chemo)
    ) |>
    tidytable::distinct(component_id, .candidate_M_key, .keep_all = TRUE) |>
    tidytable::select(
      component_id,
      .candidate_M_key,
      .anchor_feature_id = feature_id,
      .anchor_ik = candidate_structure_inchikey_connectivity_layer
    ) |>
    tidytable::mutate(
      cluster_consensus_group_id = paste(
        component_id,
        sprintf("%.*f", NEUTRAL_MASS_GROUP_DECIMALS, .candidate_M_key),
        sep = "::"
      )
    )

  n_anchor_groups <- nrow(anchor_lookup)

  if (n_anchor_groups == 0L) {
    log_debug("No cluster entity consensus groups to enforce")
    return(df_ranked |> tidytable::select(-tidyselect::any_of(".candidate_M")))
  }

  # For each feature in the consensus group, carry forward the anchor metadata.
  # Features whose current rank-1 candidate differs from the consensus anchor
  # need their rows reordered so the anchor candidate becomes rank 1. Features
  # that already have the anchor at rank 1 need provenance only.
  df_with_anchor <- df_with_comp |>
    tidytable::left_join(
      y = anchor_lookup,
      by = c("component_id", ".candidate_M_key")
    ) |>
    tidytable::mutate(
      .has_anchor_ik = candidate_structure_inchikey_connectivity_layer ==
        .anchor_ik,
      .is_anchor_feature = feature_id == .anchor_feature_id
    )

  features_with_anchor <- df_with_anchor |>
    tidytable::filter(.has_anchor_ik) |>
    tidytable::distinct(feature_id, component_id, .candidate_M_key)

  if (nrow(features_with_anchor) > 0L) {
    has_annotation_note <- "annotation_note" %in% names(df_with_anchor)
    df_part_reorder <- df_with_anchor |>
      tidytable::inner_join(
        y = features_with_anchor,
        by = c("feature_id", "component_id", ".candidate_M_key")
      ) |>
      tidytable::mutate(
        cluster_consensus_group_id = tidytable::coalesce(
          cluster_consensus_group_id,
          paste(
            component_id,
            sprintf(
              "%.*f",
              NEUTRAL_MASS_GROUP_DECIMALS,
              .candidate_M_key
            ),
            sep = "::"
          )
        ),
        cluster_consensus_anchor_feature_id = .anchor_feature_id,
        cluster_consensus_anchor_inchikey = .anchor_ik,
        cluster_consensus_applied = TRUE,
        .existing_note = if (has_annotation_note) {
          annotation_note
        } else {
          NA_character_
        }
      )

    reorder_fids <- unique(
      df_part_reorder$feature_id[
        df_part_reorder$rank_final == 1L &
          !df_part_reorder$.has_ms2_evidence &
          df_part_reorder$candidate_structure_inchikey_connectivity_layer !=
            df_part_reorder$.anchor_ik
      ]
    )

    if (length(reorder_fids) > 0L) {
      df_reordered <- df_part_reorder |>
        tidytable::filter(feature_id %in% reorder_fids) |>
        tidytable::arrange(
          feature_id,
          .candidate_M_key,
          tidytable::desc(.has_anchor_ik),
          rank_final
        ) |>
        tidytable::mutate(
          rank_final = tidytable::row_number(),
          .by = c(feature_id, component_id, .candidate_M_key)
        )

      df_not_reordered <- df_part_reorder |>
        tidytable::filter(!feature_id %in% reorder_fids)

      df_part_reorder <- tidytable::bind_rows(df_reordered, df_not_reordered) |>
        tidytable::arrange(feature_id, rank_final)
    }

    df_part_reorder <- df_part_reorder |>
      tidytable::mutate(
        cluster_consensus_promoted_from_anchor = .has_anchor_ik &
          rank_final == 1L &
          !.is_anchor_feature,
        annotation_note = tidytable::if_else(
          condition = cluster_consensus_promoted_from_anchor,
          true = "Promoted to rank 1: cluster entity consensus (same M across component)",
          false = .existing_note
        )
      )

    # Combine anchored rows with unchanged rows.
    df_unchanged <- df_with_comp |>
      tidytable::anti_join(
        y = features_with_anchor,
        by = c("feature_id", "component_id", ".candidate_M_key")
      ) |>
      tidytable::mutate(
        cluster_consensus_applied = FALSE,
        cluster_consensus_promoted_from_anchor = FALSE
      ) |>
      tidytable::select(
        -tidyselect::all_of(c(
          ".candidate_M",
          ".candidate_M_exact",
          ".candidate_M_mz",
          ".candidate_M_key",
          "component_id",
          ".has_ms2_evidence"
        ))
      )

    df_part_reorder <- df_part_reorder |>
      tidytable::select(
        -tidyselect::all_of(c(
          ".candidate_M",
          ".candidate_M_exact",
          ".candidate_M_mz",
          ".candidate_M_key",
          ".has_anchor_ik",
          ".is_anchor_feature",
          ".anchor_ik",
          ".anchor_feature_id",
          ".existing_note",
          ".has_ms2_evidence"
        ))
      )

    affected_fids <- unique(df_part_reorder$feature_id)

    df_result <- tidytable::bind_rows(df_part_reorder, df_unchanged)

    if (length(affected_fids) > 0L) {
      df_affected <- df_result |>
        tidytable::filter(feature_id %in% affected_fids) |>
        tidytable::arrange(
          feature_id,
          tidytable::desc(cluster_consensus_promoted_from_anchor),
          rank_final
        ) |>
        tidytable::mutate(
          rank_final = tidytable::row_number(),
          .by = feature_id
        )
      df_unaffected <- df_result |>
        tidytable::filter(!feature_id %in% affected_fids)
      df_result <- tidytable::bind_rows(df_affected, df_unaffected) |>
        tidytable::arrange(feature_id, rank_final)
    } else {
      df_result <- df_result |>
        tidytable::arrange(feature_id, rank_final)
    }

    log_info(
      "Enforced cluster entity consensus for %d features (anchor InChIKey promoted to rank 1)",
      tidytable::n_distinct(features_with_anchor$feature_id)
    )

    return(df_result)
  }

  # No reordering needed; clean up temporary columns
  df_with_comp |>
    tidytable::mutate(
      cluster_consensus_applied = FALSE,
      cluster_consensus_promoted_from_anchor = FALSE
    ) |>
    tidytable::select(
      -tidyselect::any_of(c(
        ".candidate_M",
        ".candidate_M_exact",
        ".candidate_M_mz",
        ".candidate_M_key",
        "component_id",
        ".has_ms2_evidence"
      ))
    )
}
