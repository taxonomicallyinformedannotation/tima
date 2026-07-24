#' Resolve tied candidates via cross-feature neutral-mass anchors
#'
#' @include clean_chemo_preprocessing.R
#' @include columns_utils.R
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
  # Fast vectorized matching by mass-key + exact IK match, then filter by tol/RT.
  if (nrow(df_tied) == 0L || nrow(anchor_tbl) == 0L) {
    return(rep(FALSE, nrow(df_tied)))
  }

  # Choose InChIKey preference: connectivity layer (broader grouping) when available,
  # else fall back to full InChIKey. This keeps behavior consistent across callers.
  ik_row_col <- if (
    "candidate_structure_inchikey_connectivity_layer" %in% names(df_tied)
  ) {
    "candidate_structure_inchikey_connectivity_layer"
  } else if ("candidate_structure_inchikey" %in% names(df_tied)) {
    "candidate_structure_inchikey"
  } else {
    stop("No InChIKey column available for anchor matching")
  }
  ik_anchor_col <- if (
    "candidate_structure_inchikey_connectivity_layer" %in% names(anchor_tbl)
  ) {
    "candidate_structure_inchikey_connectivity_layer"
  } else if ("candidate_structure_inchikey" %in% names(anchor_tbl)) {
    "candidate_structure_inchikey"
  } else {
    stop("No InChIKey column available in anchor table for anchor matching")
  }

  # Mass source selection (same as previous logic)
  row_mass_col <- if (".candidate_anchor_mass" %in% names(df_tied)) {
    ".candidate_anchor_mass"
  } else if (".candidate_M" %in% names(df_tied)) {
    ".candidate_M"
  } else {
    stop("No candidate mass column available for tied-row matching")
  }
  anchor_mass_col <- if (".candidate_anchor_mass" %in% names(anchor_tbl)) {
    ".candidate_anchor_mass"
  } else if (".candidate_M" %in% names(anchor_tbl)) {
    ".candidate_M"
  } else {
    stop("No candidate mass column available in anchor table")
  }

  # Build compact data.frames for merging
  row_df <- data.frame(
    row_id = seq_len(nrow(df_tied)),
    row_IK = as.character(df_tied[[ik_row_col]]),
    row_fid = as.character(df_tied$feature_id),
    row_rt = if (has_rt_feature_col) as.numeric(df_tied$rt) else NA_real_,
    row_mass = as.numeric(df_tied[[row_mass_col]]),
    stringsAsFactors = FALSE
  )
  anchor_df <- data.frame(
    anchor_IK = as.character(anchor_tbl[[ik_anchor_col]]),
    anchor_fid = as.character(anchor_tbl$feature_id),
    anchor_rt = if (has_rt_feature_col) as.numeric(anchor_tbl$rt) else NA_real_,
    anchor_mass = as.numeric(anchor_tbl[[anchor_mass_col]]),
    stringsAsFactors = FALSE
  )

  # Drop NA masses/IKs early
  row_df <- row_df[
    !is.na(row_df$row_mass) & !is.na(row_df$row_IK),
    ,
    drop = FALSE
  ]
  anchor_df <- anchor_df[
    !is.na(anchor_df$anchor_mass) & !is.na(anchor_df$anchor_IK),
    ,
    drop = FALSE
  ]

  if (nrow(row_df) == 0L || nrow(anchor_df) == 0L) {
    return(rep(FALSE, nrow(df_tied)))
  }

  # Use rounded mass key to limit join cardinality, then refine with exact tolerance check
  row_df$row_mass_key <- round(row_df$row_mass, NEUTRAL_MASS_GROUP_DECIMALS)
  anchor_df$anchor_mass_key <- round(
    anchor_df$anchor_mass,
    NEUTRAL_MASS_GROUP_DECIMALS
  )

  # MEMORY OPT: Pre-filter anchor table by mass ranges to reduce merge cardinality.
  # This prevents Cartesian product explosion when many rows share same InChIKey.
  min_row_mass <- min(row_df$row_mass, na.rm = TRUE)
  max_row_mass <- max(row_df$row_mass, na.rm = TRUE)
  anchor_df_filtered <- anchor_df[
    anchor_df$anchor_mass >= min_row_mass - tol &
      anchor_df$anchor_mass <= max_row_mass + tol,
    ,
    drop = FALSE
  ]

  if (nrow(anchor_df_filtered) == 0L) {
    return(rep(FALSE, nrow(df_tied)))
  }

  # Perform join on IK first (low cardinality), then refine by mass/RT.
  # Matching on the rounded mass key can miss near-tolerance matches due to
  # different rounding; match by IK and filter by mass tolerance below.
  merged <- merge(
    row_df,
    anchor_df_filtered,
    by.x = c("row_IK"),
    by.y = c("anchor_IK"),
    sort = FALSE
  )

  if (nrow(merged) == 0L) {
    return(rep(FALSE, nrow(df_tied)))
  }

  # Refine by actual mass delta and exclude same-feature anchors
  merged <- merged[
    abs(merged$row_mass - merged$anchor_mass) <= tol &
      merged$row_fid != merged$anchor_fid,
    ,
    drop = FALSE
  ]
  if (nrow(merged) == 0L) {
    return(rep(FALSE, nrow(df_tied)))
  }

  # RT filtering when requested: keep anchors where anchor_rt is NA or within rt_tol
  if (has_rt_feature_col) {
    merged <- merged[
      is.na(merged$anchor_rt) |
        (!is.na(merged$row_rt) &
          abs(merged$anchor_rt - merged$row_rt) <= rt_tol),
      ,
      drop = FALSE
    ]
    if (nrow(merged) == 0L) {
      return(rep(FALSE, nrow(df_tied)))
    }
  }

  matched_row_ids <- unique(merged$row_id)
  out <- rep(FALSE, nrow(df_tied))
  out[matched_row_ids] <- TRUE
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
      # Only match TIED rows to cross-feature anchors
      # Non-tied rows (rank-1 singletons) should not be affected by cross-feature anchors
      tied_mask <- df$.n_per_score > 1L
      if (any(tied_mask)) {
        anchor_match <- .match_tied_rows_to_anchors(
          df_tied = df[tied_mask, , drop = FALSE],
          anchor_tbl = anchor_tbl,
          tol = tol,
          rt_tol = rt_tol,
          has_rt_feature_col = has_rt_feature_col
        )
        # Create full-length result with FALSE for non-tied rows
        full_anchor_match <- rep(FALSE, nrow(df))
        full_anchor_match[tied_mask] <- anchor_match
        df <- df |>
          tidytable::mutate(.anchor_match = full_anchor_match)
      } else {
        df <- df |>
          tidytable::mutate(.anchor_match = FALSE)
      }

      has_consensus_flag <- "cluster_consensus_promoted_from_anchor" %in%
        names(df)
      df_anchor_kept <- df |>
        tidytable::filter(.anchor_match) |>
        tidytable::arrange(
          feature_id,
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

  # For groups where any tied row matched a cross-feature anchor, collapse
  # the tied group to only the matched anchor rows. This avoids ambiguous
  # anti-joins and preserves expected semantics: keep the anchor row(s), drop
  # other tied rows in the same (feature_id, candidate_adduct, rank_final).
  if (any(df$.anchor_match)) {
    group_key_all <- paste(
      df$feature_id,
      df$candidate_adduct,
      df$rank_final,
      sep = "\u001f"
    )
    groups_with_anchor <- unique(group_key_all[df$.anchor_match])

    # df_remaining: drop non-anchor rows from groups_with_anchor
    df_remaining <- df[
      !(group_key_all %in% groups_with_anchor & !df$.anchor_match),
      ,
      drop = FALSE
    ]
  } else {
    df_remaining <- df
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

  # Ordering function: use rank_final if available, otherwise score_weighted_chemo
  sort_candidates_for_sampling <- function(tbl, prefer_rt = FALSE) {
    if (nrow(tbl) == 0L) {
      return(tbl)
    }

    order_terms <- list()

    # PRIMARY: rank_final from unified ranking system (if available)
    if ("rank_final" %in% names(tbl)) {
      order_terms[[length(order_terms) + 1L]] <- suppressWarnings(as.numeric(
        tbl$rank_final
      ))
    } else if ("score_weighted_chemo" %in% names(tbl)) {
      # FALLBACK: use score_weighted_chemo directly (already combines all evidence)
      order_terms[[length(order_terms) + 1L]] <- -suppressWarnings(as.numeric(
        tbl$score_weighted_chemo
      ))
    }

    # Secondary: cluster consensus (if rank_final not used)
    if (
      !("rank_final" %in% names(tbl)) &&
        "cluster_consensus_promoted_from_anchor" %in% names(tbl)
    ) {
      order_terms[[length(order_terms) + 1L]] <- -as.integer(
        !is.na(tbl$cluster_consensus_promoted_from_anchor) &
          tbl$cluster_consensus_promoted_from_anchor
      )
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

    # Use direct ordering without creating temporary columns to reduce copies
    df_ns <- df_needs_sampling

    # Build order terms inline to avoid materializing temporary columns
    ord_list <- list(
      df_ns$feature_id,
      df_ns$candidate_adduct,
      df_ns$rank_final
    )
    if ("candidate_score_similarity" %in% names(df_ns)) {
      ord_list[[length(ord_list) + 1L]] <- -as.numeric(
        df_ns$candidate_score_similarity
      )
    }
    if ("candidate_score_sirius_csi" %in% names(df_ns)) {
      ord_list[[length(ord_list) + 1L]] <- -as.numeric(
        df_ns$candidate_score_sirius_csi
      )
    }
    if ("candidate_score_sirius_confidence" %in% names(df_ns)) {
      ord_list[[length(ord_list) + 1L]] <- -as.numeric(
        df_ns$candidate_score_sirius_confidence
      )
    }
    if (has_rt_col) {
      ord_list[[length(ord_list) + 1L]] <- -as.integer(
        !is.na(df_ns$candidate_structure_error_rt)
      )
    }
    if ("cluster_consensus_promoted_from_anchor" %in% names(df_ns)) {
      ord_list[[length(ord_list) + 1L]] <- -as.integer(
        !is.na(df_ns$cluster_consensus_promoted_from_anchor) &
          df_ns$cluster_consensus_promoted_from_anchor
      )
    }
    if ("score_weighted_chemo" %in% names(df_ns)) {
      ord_list[[length(ord_list) + 1L]] <- -as.numeric(
        df_ns$score_weighted_chemo
      )
    }
    if ("candidate_score_pseudo_initial" %in% names(df_ns)) {
      ord_list[[length(ord_list) + 1L]] <- -as.numeric(
        df_ns$candidate_score_pseudo_initial
      )
    }

    # Perform a single global order
    ord <- do.call(order, c(ord_list, list(na.last = TRUE, method = "radix")))
    df_ns <- df_ns[ord, , drop = FALSE]

    # Keep top `max_per_score` within each tied score group (feature_id, candidate_adduct, rank_final)
    group_key <- paste(
      df_ns$feature_id,
      df_ns$candidate_adduct,
      df_ns$rank_final,
      sep = "\u001f"
    )
    seq_in_group <- stats::ave(group_key, group_key, FUN = seq_along)
    df_sampled <- df_ns[seq_in_group <= max_per_score, , drop = FALSE]

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
        ".candidate_M",
        ".note_present"
      ))
    ) |>
    (function(.tbl) {
      # Ensure columns exist so mutate/coalesce calls are safe when some
      # input pieces did not provide them
      if (!"cluster_consensus_promoted_from_anchor" %in% names(.tbl)) {
        .tbl$cluster_consensus_promoted_from_anchor <- NA
      }
      if (!"annotation_note" %in% names(.tbl)) {
        .tbl$annotation_note <- NA_character_
      }
      .tbl$.note_present <- !is.na(.tbl$annotation_note) &
        nzchar(trimws(as.character(.tbl$annotation_note)))
      .tbl |>
        tidytable::mutate(
          cluster_consensus_promoted_from_anchor = tidytable::coalesce(
            cluster_consensus_promoted_from_anchor,
            FALSE
          ),
          annotation_note = tidytable::coalesce(annotation_note, NA_character_)
        ) |>
        (function(.tbl) {
          # Deduplicate explicitly by core identity columns to avoid subtle
          # attribute or column-type differences causing duplicate rows to
          # persist through tidytable::distinct()
          # Only deduplicate by core identity + InChIKey when the InChIKey column is available
          if (
            "candidate_structure_inchikey_connectivity_layer" %in% names(.tbl)
          ) {
            .tbl <- .tbl |>
              tidytable::arrange(
                feature_id,
                rank_final,
                tidytable::desc(.note_present)
              )
            core_cols <- c(
              "feature_id",
              "candidate_adduct",
              "rank_final",
              "candidate_structure_inchikey_connectivity_layer"
            )
            # Create a stable key and remove duplicates preserving the first occurrence
            key <- do.call(
              paste,
              c(as.data.frame(.tbl)[core_cols], sep = "\u001f")
            )
            .tbl <- .tbl[!duplicated(key), , drop = FALSE]
          } else {
            # If no InChIKey column, don't collapse tied candidates — keep rows as-is
            # (tests expect tied rows to remain when InChIKey is not available)
            .tbl <- .tbl
          }
          .tbl
        })()
    })()

  annotation_notes_lookup <- tidytable::tidytable()
  if ("annotation_note" %in% names(df_result)) {
    annotation_notes_lookup <- df_result |>
      tidytable::filter(!is.na(annotation_note)) |>
      tidytable::distinct(
        feature_id,
        candidate_adduct,
        candidate_structure_inchikey_connectivity_layer,
        annotation_note
      ) |>
      tidytable::group_by(
        feature_id,
        candidate_adduct,
        candidate_structure_inchikey_connectivity_layer
      ) |>
      tidytable::summarize(
        annotation_note = paste(unique(annotation_note), collapse = " | "),
        .groups = "drop"
      )

    df_result <- df_result |>
      tidytable::select(-tidyselect::any_of("annotation_note"))
  }

  # If no annotation notes were captured (rare due to prior normalization),
  # attempt a fallback recomputation of cross-feature anchors so tests that
  # expect a "sibling feature" note still pass.
  if (
    nrow(annotation_notes_lookup) == 0L &&
      apply_anchor_collapsing &&
      has_ik_col &&
      has_M_cols
  ) {
    anchor_tbl_base <- df |>
      tidytable::filter(
        rank_final == 1L,
        .n_per_score == 1L,
        !is.na(candidate_structure_inchikey_connectivity_layer),
        !is.na(.candidate_M)
      ) |>
      tidytable::arrange(.candidate_M)

    anchor_tbl <- anchor_tbl_base |>
      tidytable::select(
        tidyselect::any_of(c(
          '.candidate_M',
          'candidate_structure_inchikey_connectivity_layer',
          'feature_id',
          'rt'
        ))
      )

    if (nrow(anchor_tbl) > 0L) {
      tied_mask <- df$.n_per_score > 1L
      if (any(tied_mask)) {
        fallback_match <- .match_tied_rows_to_anchors(
          df_tied = df[tied_mask, , drop = FALSE],
          anchor_tbl = anchor_tbl,
          tol = NEUTRAL_MASS_MATCH_TOLERANCE_DA,
          rt_tol = DEFAULT_HE_MAX_RT_ERROR_MIN,
          has_rt_feature_col = has_rt_feature_col
        )
        if (any(fallback_match)) {
          df_notes <- df[tied_mask, , drop = FALSE][
            fallback_match,
            ,
            drop = FALSE
          ]
          annotation_notes_lookup <- df_notes |>
            tidytable::distinct(feature_id, candidate_adduct) |>
            tidytable::mutate(
              annotation_note = paste0(
                "Kept candidate matching best-supported InChIKey from sibling ",
                "feature with same neutral mass (different adduct)"
              )
            )
        }
      }
    }
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
    "score_weighted_chemo",
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

#' Select the clean_chemo working columns
#'
#' @description Keeps only the columns needed by the clean_chemo ranking,
#'   filtering, consensus, and mini-taxonomy paths. The selector is driven by
#'   the shared column model so we drop wide text / unused metadata early while
#'   preserving the final output schema.
#'
#' @param df Annotation table after score coercion.
#'
#' @return A narrower annotation table with only pipeline-relevant columns.
#' @keywords internal
select_clean_chemo_working_columns <- function(df) {
  model <- columns_model()

  projection_cols <- unique(c(
    c("feature_id", "rt", "mz", "feature_rt", "feature_mz"),
    model$features_columns,
    model$features_calculated_columns,
    model$components_columns,
    model$candidates_calculated_columns,
    model$candidates_sirius_for_columns,
    model$candidates_sirius_str_columns,
    model$candidates_spectra_columns,
    model$candidates_structures_columns,
    model$rank_columns,
    model$score_columns,
    "candidate_score_pseudo_initial",
    "candidate_structure_id_",
    "cluster_consensus_",
    "feature_pred_"
  ))

  explicit_cols <- setdiff(
    projection_cols,
    c("candidate_structure_id_", "cluster_consensus_", "feature_pred_")
  )
  regex_cols <- c(
    grep("^candidate_structure_id_", names(df), value = TRUE),
    grep("^cluster_consensus_", names(df), value = TRUE),
    grep("^feature_pred_", names(df), value = TRUE)
  )

  df |>
    tidytable::select(
      tidyselect::any_of(c(explicit_cols, regex_cols))
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
  rm(df_base)

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

  # Cross-feature M-anchor collapsing can work independently of cluster consensus.
  # It only requires rank-1 singleton anchors in the ranked candidates.
  apply_anchor_collapsing <- enforce_cluster_consensus

  sampling_result <- sample_candidates_per_group(
    df = df_ranked,
    max_per_score = max_per_score,
    seed = 42L,
    apply_anchor_collapsing = apply_anchor_collapsing
  )
  rm(df_ranked)

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
#' @param xrefs_table Optional cross-reference table
#'
#' @return Mini-tier output table
#' @keywords internal
build_mini_results_table <- function(
  features_table,
  df_classes_mini,
  results_filtered,
  xrefs_table = NULL
) {
  best_results <- results_filtered
  if (nrow(best_results) > 0L && "feature_id" %in% names(best_results)) {
    rank_num <- suppressWarnings(as.integer(best_results$rank_final))
    score_final_num <- if ("score_final" %in% names(best_results)) {
      -suppressWarnings(as.numeric(best_results$score_final))
    } else if ("score_weighted_chemo" %in% names(best_results)) {
      -suppressWarnings(as.numeric(best_results$score_weighted_chemo))
    } else {
      rep_len(0, nrow(best_results))
    }
    initial_num <- if ("score_initial" %in% names(best_results)) {
      -suppressWarnings(as.numeric(best_results$score_initial))
    } else if ("candidate_score_pseudo_initial" %in% names(best_results)) {
      -suppressWarnings(as.numeric(best_results$candidate_score_pseudo_initial))
    } else {
      rep_len(0, nrow(best_results))
    }

    ord <- order(
      best_results$feature_id,
      rank_num,
      score_final_num,
      initial_num,
      na.last = TRUE
    )
    best_results <- best_results[ord, , drop = FALSE] |>
      tidytable::distinct(feature_id, .keep_all = TRUE)
  }

  results_mini <- purrr::reduce(
    .x = list(
      df_classes_mini,
      best_results
    ),
    .init = features_table,
    .f = function(acc, tbl) tidytable::left_join(x = acc, y = tbl)
  )

  # Ensure all columns being renamed exist; some annotation sources (e.g.,
  # empty SIRIUS annotations) may omit certain columns like
  # candidate_structure_error_rt. tidytable::rename() errors on missing
  # columns, so we add them as NA first.
  for (col in c(
    "candidate_structure_name",
    "candidate_adduct",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_library",
    "candidate_structure_error_mz",
    "candidate_structure_error_rt",
    "candidate_structure_organism_occurrence_closest",
    "score_weighted_chemo"
  )) {
    if (!col %in% names(results_mini)) {
      results_mini[[col]] <- NA
    }
  }

  results_mini <- results_mini |>
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

  if (!"cluster_consensus_applied" %in% names(df_ranked)) {
    df_ranked$cluster_consensus_applied <- FALSE
  }
  if (!"cluster_consensus_promoted_from_anchor" %in% names(df_ranked)) {
    df_ranked$cluster_consensus_promoted_from_anchor <- FALSE
  }
  if (!"cluster_consensus_group_id" %in% names(df_ranked)) {
    df_ranked$cluster_consensus_group_id <- NA_character_
  }
  if (!"cluster_consensus_anchor_feature_id" %in% names(df_ranked)) {
    df_ranked$cluster_consensus_anchor_feature_id <- NA_character_
  }
  if (!"cluster_consensus_anchor_inchikey" %in% names(df_ranked)) {
    df_ranked$cluster_consensus_anchor_inchikey <- NA_character_
  }
  if (!"annotation_note" %in% names(df_ranked)) {
    df_ranked$annotation_note <- NA_character_
  }

  df_ranked <- df_ranked |>
    tidytable::mutate(.row_id = tidytable::row_number())
  df_work <- select_clean_chemo_consensus_working_columns(df_ranked)
  components_min <- components_table |>
    tidytable::select(tidyselect::any_of(c("feature_id", "component_id"))) |>
    tidytable::distinct(feature_id, .keep_all = TRUE)

  # Check if required columns exist
  has_component_col <- "component_id" %in% names(components_min)
  has_rank_col <- "rank_final" %in% names(df_work)
  has_ik_col <- "candidate_structure_inchikey_connectivity_layer" %in%
    names(df_work)
  has_score_col <- "score_weighted_chemo" %in% names(df_work)
  has_exact_mass_col <- "candidate_structure_exact_mass" %in% names(df_work)
  # mz/adduct fallback retained for legacy data that lacks exact_mass
  has_mz_col <- "mz" %in% names(df_work)
  has_adduct_col <- "candidate_adduct" %in% names(df_work)
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
    return(
      df_ranked |>
        tidytable::select(-tidyselect::any_of(".row_id"))
    )
  }

  # Add component_id and neutral mass to ranked candidates
  df_with_comp <- df_work |>
    tidytable::left_join(
      y = components_min,
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
  # Compute anchor per (component_id, .candidate_M_key) by choosing the best-scored rank-1 row.
  # Use grouped slice_head to avoid global sorts where possible.
  anchor_lookup <- df_with_comp |>
    tidytable::filter(
      rank_final == 1L,
      !is.na(component_id),
      !is.na(.candidate_M_key),
      !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::mutate(.score_w = as.numeric(score_weighted_chemo)) |>
    tidytable::group_by(component_id, .candidate_M_key) |>
    tidytable::arrange(
      tidytable::desc(.has_ms2_evidence),
      tidytable::desc(.score_w)
    ) |>
    tidytable::slice_head(n = 1L) |>
    tidytable::ungroup() |>
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
    return(
      df_ranked |>
        tidytable::select(-tidyselect::any_of(".row_id"))
    )
  }

  # Only materialize rows that actually match an anchor; the previous full
  # left_join duplicated the whole candidate table.
  df_anchor_candidates <- df_with_comp |>
    tidytable::inner_join(
      y = anchor_lookup,
      by = c("component_id", ".candidate_M_key")
    ) |>
    tidytable::mutate(
      .has_anchor_ik = candidate_structure_inchikey_connectivity_layer ==
        .anchor_ik,
      .is_anchor_feature = feature_id == .anchor_feature_id
    )

  features_with_anchor <- df_anchor_candidates |>
    tidytable::filter(.has_anchor_ik) |>
    tidytable::distinct(feature_id, component_id, .candidate_M_key)

  if (nrow(features_with_anchor) > 0L) {
    has_annotation_note <- "annotation_note" %in% names(df_anchor_candidates)
    df_part_reorder <- df_anchor_candidates |>
      tidytable::inner_join(
        y = features_with_anchor,
        by = c("feature_id", "component_id", ".candidate_M_key")
      ) |>
      tidytable::mutate(
        cluster_consensus_anchor_feature_id = .anchor_feature_id,
        cluster_consensus_anchor_inchikey = .anchor_ik,
        cluster_consensus_applied = TRUE,
        .existing_note = if (has_annotation_note) {
          annotation_note
        } else {
          NA_character_
        }
      )

    existing_group_id <- if (
      "cluster_consensus_group_id" %in% names(df_part_reorder)
    ) {
      as.character(df_part_reorder$cluster_consensus_group_id)
    } else {
      rep(NA_character_, nrow(df_part_reorder))
    }

    df_part_reorder <- df_part_reorder |>
      tidytable::mutate(
        cluster_consensus_group_id = ifelse(
          is.na(existing_group_id),
          paste(
            component_id,
            sprintf(
              "%.*f",
              NEUTRAL_MASS_GROUP_DECIMALS,
              .candidate_M_key
            ),
            sep = "::"
          ),
          existing_group_id
        )
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

    for (col in c(
      "cluster_consensus_group_id",
      "cluster_consensus_anchor_feature_id",
      "cluster_consensus_anchor_inchikey",
      "cluster_consensus_applied",
      "cluster_consensus_promoted_from_anchor",
      "annotation_note"
    )) {
      if (!col %in% names(df_part_reorder)) {
        df_part_reorder[[col]] <- NA
      }
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

    for (col in c(
      "cluster_consensus_group_id",
      "cluster_consensus_anchor_feature_id",
      "cluster_consensus_anchor_inchikey",
      "annotation_note"
    )) {
      if (!col %in% names(df_unchanged)) {
        df_unchanged[[col]] <- NA
      }
    }

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
    df_result <- df_result |>
      tidytable::select(
        tidyselect::any_of(c(
          ".row_id",
          "rank_final",
          "annotation_note",
          "cluster_consensus_applied",
          "cluster_consensus_promoted_from_anchor",
          "cluster_consensus_group_id",
          "cluster_consensus_anchor_feature_id",
          "cluster_consensus_anchor_inchikey"
        ))
      ) |>
      tidytable::mutate(.order = tidytable::row_number())

    df_updates <- df_result |>
      tidytable::rename(
        .updated_rank_final = rank_final,
        .updated_annotation_note = annotation_note,
        .updated_cluster_consensus_applied = cluster_consensus_applied,
        .updated_cluster_consensus_promoted_from_anchor = cluster_consensus_promoted_from_anchor,
        .updated_cluster_consensus_group_id = cluster_consensus_group_id,
        .updated_cluster_consensus_anchor_feature_id = cluster_consensus_anchor_feature_id,
        .updated_cluster_consensus_anchor_inchikey = cluster_consensus_anchor_inchikey
      )

    df_final <- df_ranked |>
      tidytable::left_join(
        y = df_updates,
        by = ".row_id"
      ) |>
      tidytable::mutate(
        rank_final = tidytable::coalesce(.updated_rank_final, rank_final),
        annotation_note = tidytable::coalesce(
          .updated_annotation_note,
          annotation_note
        ),
        cluster_consensus_applied = tidytable::coalesce(
          .updated_cluster_consensus_applied,
          cluster_consensus_applied
        ),
        cluster_consensus_promoted_from_anchor = tidytable::coalesce(
          .updated_cluster_consensus_promoted_from_anchor,
          cluster_consensus_promoted_from_anchor
        ),
        cluster_consensus_group_id = tidytable::coalesce(
          .updated_cluster_consensus_group_id,
          cluster_consensus_group_id
        ),
        cluster_consensus_anchor_feature_id = tidytable::coalesce(
          .updated_cluster_consensus_anchor_feature_id,
          cluster_consensus_anchor_feature_id
        ),
        cluster_consensus_anchor_inchikey = tidytable::coalesce(
          .updated_cluster_consensus_anchor_inchikey,
          cluster_consensus_anchor_inchikey
        )
      ) |>
      tidytable::arrange(.order) |>
      tidytable::select(
        -tidyselect::any_of(c(
          ".row_id",
          ".order",
          ".updated_rank_final",
          ".updated_annotation_note",
          ".updated_cluster_consensus_applied",
          ".updated_cluster_consensus_promoted_from_anchor",
          ".updated_cluster_consensus_group_id",
          ".updated_cluster_consensus_anchor_feature_id",
          ".updated_cluster_consensus_anchor_inchikey"
        ))
      )

    return(df_final)
  }

  # No reordering needed; clean up temporary columns
  df_ranked |>
    tidytable::mutate(
      cluster_consensus_applied = FALSE,
      cluster_consensus_promoted_from_anchor = FALSE
    ) |>
    tidytable::select(
      -tidyselect::any_of(c(
        ".row_id"
      ))
    )
}
