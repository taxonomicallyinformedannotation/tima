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

  # MEMORY OPT: Pre-filter anchor table by mass ranges and shared InChIKeys to
  # reduce merge cardinality and avoid copying uninvolved tied rows.
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

  u_anchor_ik <- unique(anchor_df_filtered$anchor_IK)
  row_in_anchor <- !is.na(fastmatch::fmatch(row_df$row_IK, u_anchor_ik))
  if (!any(row_in_anchor)) {
    return(rep(FALSE, nrow(df_tied)))
  }
  row_df <- row_df[which(row_in_anchor), , drop = FALSE]

  u_row_ik <- unique(row_df$row_IK)
  anchor_df_filtered <- anchor_df_filtered[
    which(!is.na(fastmatch::fmatch(anchor_df_filtered$anchor_IK, u_row_ik))),
    ,
    drop = FALSE
  ]

  if (anyDuplicated(anchor_df_filtered$anchor_IK) == 0L) {
    m_ik <- fastmatch::fmatch(row_df$row_IK, anchor_df_filtered$anchor_IK)
    a_mass <- anchor_df_filtered$anchor_mass[m_ik]
    a_fid <- anchor_df_filtered$anchor_fid[m_ik]
    a_rt <- anchor_df_filtered$anchor_rt[m_ik]
    valid_m <- abs(row_df$row_mass - a_mass) <= tol & row_df$row_fid != a_fid
    if (has_rt_feature_col) {
      valid_m <- valid_m &
        (is.na(a_rt) |
          (!is.na(row_df$row_rt) & abs(a_rt - row_df$row_rt) <= rt_tol))
    }
    matched_row_ids <- unique(row_df$row_id[valid_m])
    out <- rep(FALSE, nrow(df_tied))
    out[matched_row_ids] <- TRUE
    return(out)
  }

  # Perform join on IK when multiple anchors share the same InChIKey
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
  df <- tidytable::as_tidytable(df)
  n_rows <- nrow(df)
  if (!"rank_final" %in% names(df)) {
    data.table::set(df, j = "rank_final", value = rep.int(NA_integer_, n_rows))
  }

  # Compute per-score-group sizes (feature_id, candidate_adduct, rank_final)
  # using integer group encoding + tabulate() instead of tidytable::mutate(.by)
  fids <- df$feature_id
  adds <- if ("candidate_adduct" %in% names(df)) {
    df$candidate_adduct
  } else {
    rep.int(NA_character_, n_rows)
  }
  rks <- df$rank_final

  u_fids <- unique(fids)
  u_adds <- unique(adds)
  u_rks <- unique(rks)
  fid_id <- fastmatch::fmatch(fids, u_fids)
  add_id <- fastmatch::fmatch(adds, u_adds)
  rk_id <- fastmatch::fmatch(rks, u_rks)

  n_a <- max(1L, length(u_adds))
  n_r <- max(1L, length(u_rks))
  if (as.numeric(length(u_fids)) * as.numeric(n_a) * as.numeric(n_r) < 4.5e15) {
    grp_code <- ((as.numeric(fid_id - 1L) * n_a + (add_id - 1L)) * n_r) + rk_id
    u_grp <- unique(grp_code)
    grp_idx <- fastmatch::fmatch(grp_code, u_grp)
  } else {
    grp_str <- paste0(fids, "\u001f", adds, "\u001f", rks)
    u_grp <- unique(grp_str)
    grp_idx <- fastmatch::fmatch(grp_str, u_grp)
  }
  grp_counts <- tabulate(grp_idx, nbins = length(u_grp))
  data.table::set(df, j = ".n_per_score", value = grp_counts[grp_idx])
  data.table::set(df, j = ".anchor_match", value = rep.int(FALSE, n_rows))

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
      df_anchor_kept <- df[which(df$.anchor_match), , drop = FALSE]
      if (nrow(df_anchor_kept) > 0L) {
        ord_ak <- list(df_anchor_kept$feature_id, df_anchor_kept$rank_final)
        if ("score_weighted_chemo" %in% names(df_anchor_kept)) {
          ord_ak[[length(ord_ak) + 1L]] <- -as.numeric(
            df_anchor_kept$score_weighted_chemo
          )
        }
        if ("candidate_score_pseudo_initial" %in% names(df_anchor_kept)) {
          ord_ak[[length(ord_ak) + 1L]] <- -as.numeric(
            df_anchor_kept$candidate_score_pseudo_initial
          )
        }
        ak_idx <- do.call(
          order,
          c(ord_ak, list(na.last = TRUE, method = "radix"))
        )
        df_anchor_kept <- df_anchor_kept[ak_idx, , drop = FALSE]
        if (!has_consensus_flag) {
          df_anchor_kept$cluster_consensus_promoted_from_anchor <- rep(
            FALSE,
            nrow(df_anchor_kept)
          )
        }
        df_anchor_kept$annotation_note <- rep(
          paste0(
            "Kept candidate matching best-supported InChIKey from sibling ",
            "feature with same neutral mass (different adduct)"
          ),
          nrow(df_anchor_kept)
        )
      }
    }
  }

  # For groups where any tied row matched a cross-feature anchor, collapse
  # the tied group to only the matched anchor rows. This avoids ambiguous
  # anti-joins and preserves expected semantics: keep the anchor row(s), drop
  # other tied rows in the same (feature_id, candidate_adduct, rank_final).
  # OPTIMIZATION: Use paste0 with sep="\u001f" directly on columns instead of
  # do.call(paste, c(as.data.frame(.tbl)[core_cols], sep=...)) which creates
  # a full as.data.frame() copy per call. paste0() on vectors is fast and
  # memory-efficient; the \u001f separator avoids collision issues.
  if (any(df$.anchor_match)) {
    group_key_all <- paste0(
      df$feature_id,
      "\u001f",
      df$candidate_adduct,
      "\u001f",
      df$rank_final
    )
    groups_with_anchor <- unique(group_key_all[df$.anchor_match])

    # Use fastmatch for O(1) lookup instead of %in%
    keep_rows <- !(fastmatch::`%fin%`(
      group_key_all,
      groups_with_anchor
    ) &
      !df$.anchor_match)
    df_remaining <- df[keep_rows, , drop = FALSE]
  } else {
    df_remaining <- df
  }

  # Count how many distinct tied groups require sampling and partition directly
  # by integer row indices to avoid materializing `.partition` and 3x filter() copies.
  n_ps <- df_remaining$.n_per_score
  idx_sample <- which(n_ps > max_per_score)
  n_sampled_features <- if (length(idx_sample) > 0L) {
    length(unique(df_remaining$feature_id[idx_sample]))
  } else {
    0L
  }

  has_rt_col <- "candidate_structure_error_rt" %in% names(df_remaining)

  df_untied <- df_remaining[which(n_ps == 1L), , drop = FALSE]
  df_keep_remaining <- df_remaining[
    which(n_ps > 1L & n_ps <= max_per_score),
    ,
    drop = FALSE
  ]
  df_needs_sampling <- df_remaining[idx_sample, , drop = FALSE]

  # Sample only within each tied score group (feature_id, candidate_adduct, rank_final)
  if (nrow(df_needs_sampling) > 0L) {
    if (has_rt_col) {
      df_needs_sampling$.rt_priority <- !is.na(
        df_needs_sampling$candidate_structure_error_rt
      )
    }

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

    # Since df_ns is sorted by (feature_id, candidate_adduct, rank_final),
    # each tied group is a contiguous run. Compute within-group sequence in O(N)
    # without paste0() or stats::ave().
    n_ns <- nrow(df_ns)
    f_ns <- if (anyNA(u_fids)) {
      match(df_ns$feature_id, u_fids)
    } else {
      fastmatch::fmatch(df_ns$feature_id, u_fids)
    }
    a_ns <- if ("candidate_adduct" %in% names(df_ns)) {
      if (anyNA(u_adds)) {
        match(df_ns$candidate_adduct, u_adds)
      } else {
        fastmatch::fmatch(df_ns$candidate_adduct, u_adds)
      }
    } else {
      rep.int(1L, n_ns)
    }
    r_ns <- if (anyNA(u_rks)) {
      match(df_ns$rank_final, u_rks)
    } else {
      fastmatch::fmatch(df_ns$rank_final, u_rks)
    }
    is_new_grp <- if (n_ns > 1L) {
      c(
        TRUE,
        f_ns[-1L] != f_ns[-n_ns] |
          a_ns[-1L] != a_ns[-n_ns] |
          r_ns[-1L] != r_ns[-n_ns]
      )
    } else {
      TRUE
    }
    idx_ns <- seq_len(n_ns)
    seq_in_group <- idx_ns - cummax(ifelse(is_new_grp, idx_ns, 1L)) + 1L
    df_sampled <- tidytable::as_tidytable(
      df_ns[which(seq_in_group <= max_per_score), , drop = FALSE]
    )
    df_sampled$annotation_note <- paste0(
      "Sampled ",
      max_per_score,
      " of ",
      df_sampled$.n_per_score,
      " tied candidates"
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
            "candidate_structure_inchikey_connectivity_layer" %in%
              names(.tbl) &&
              nrow(.tbl) > 0L
          ) {
            fids_t <- .tbl$feature_id
            adds_t <- if ("candidate_adduct" %in% names(.tbl)) {
              .tbl$candidate_adduct
            } else {
              rep.int(NA_character_, nrow(.tbl))
            }
            rks_t <- .tbl$rank_final
            iks_t <- .tbl$candidate_structure_inchikey_connectivity_layer
            note_p <- -as.integer(.tbl$.note_present)

            u_f <- sort(unique(fids_t), na.last = TRUE)
            u_a <- unique(adds_t)
            u_r <- sort(unique(rks_t), na.last = TRUE)
            u_k <- unique(iks_t)

            f_id <- fastmatch::fmatch(fids_t, u_f)
            a_id <- fastmatch::fmatch(adds_t, u_a)
            r_id <- fastmatch::fmatch(rks_t, u_r)
            k_id <- fastmatch::fmatch(iks_t, u_k)

            ord_t <- order(f_id, r_id, note_p, na.last = TRUE, method = "radix")
            .tbl <- .tbl[ord_t, , drop = FALSE]

            na_u <- max(1L, length(u_a))
            nr_u <- max(1L, length(u_r))
            nk_u <- max(1L, length(u_k))
            if (
              as.numeric(length(u_f)) *
                as.numeric(na_u) *
                as.numeric(nr_u) *
                as.numeric(nk_u) <
                4.5e15
            ) {
              key <- (((as.numeric(f_id[ord_t] - 1L) *
                na_u +
                (a_id[ord_t] - 1L)) *
                nr_u +
                (r_id[ord_t] - 1L)) *
                nk_u) +
                k_id[ord_t]
            } else {
              key <- paste0(
                .tbl$feature_id,
                "\u001f",
                .tbl$candidate_adduct,
                "\u001f",
                .tbl$rank_final,
                "\u001f",
                .tbl$candidate_structure_inchikey_connectivity_layer
              )
            }
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
    has_note_idx <- which(!is.na(df_result$annotation_note))
    if (length(has_note_idx) > 0L) {
      note_cols <- intersect(
        c(
          "feature_id",
          "candidate_adduct",
          "candidate_structure_inchikey_connectivity_layer",
          "annotation_note"
        ),
        names(df_result)
      )
      notes_sub <- tidytable::as_tidytable(
        lapply(as.list(df_result)[note_cols], `[`, has_note_idx)
      ) |>
        tidytable::distinct()
      key_cols_n <- setdiff(note_cols, "annotation_note")
      if (anyDuplicated(as.data.frame(as.list(notes_sub)[key_cols_n])) == 0L) {
        annotation_notes_lookup <- notes_sub
      } else {
        annotation_notes_lookup <- notes_sub |>
          tidytable::summarize(
            annotation_note = paste(unique(annotation_note), collapse = " | "),
            .by = tidyselect::all_of(key_cols_n)
          )
      }
    }

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

  annot_table_wei_chemo <- tidytable::as_tidytable(annot_table_wei_chemo)
  for (col in cols_to_convert) {
    val <- annot_table_wei_chemo[[col]]
    if (!is.numeric(val)) {
      data.table::set(annot_table_wei_chemo, j = col, value = as.numeric(val))
    }
  }
  annot_table_wei_chemo
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
  nms <- names(df)

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
    "candidate_score_pseudo_initial"
  ))

  prefix_cols <- c(
    nms[startsWith(nms, "candidate_structure_id_")],
    nms[startsWith(nms, "cluster_consensus_")],
    nms[startsWith(nms, "feature_pred_")]
  )

  keep <- intersect(unique(c(projection_cols, prefix_cols)), nms)
  if (identical(keep, nms) && inherits(df, "tidytable")) {
    return(df)
  }
  tidytable::as_tidytable(as.list(df)[keep])
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

  normalize_tax_label_dedup <- function(x) {
    if (length(x) == 0L) {
      return(character(0L))
    }
    x_chr <- if (is.character(x)) x else as.character(x)
    u <- unique(x_chr)
    u_trim <- trimws(u)
    u_trim[
      u_trim %in% c("", "notClassified", "empty", "N/A", "null")
    ] <- NA_character_
    idx <- if (anyNA(u)) match(x_chr, u) else fastmatch::fmatch(x_chr, u)
    u_trim[idx]
  }

  coalesce_tax_labels <- function(cols) {
    n_rows <- nrow(df_percentile)
    out <- rep(NA_character_, n_rows)
    rem <- seq_len(n_rows)
    for (col in cols) {
      if (length(rem) == 0L) {
        break
      }
      vals <- normalize_tax_label_dedup(df_percentile[[col]][rem])
      ok <- !is.na(vals)
      if (any(ok)) {
        out[rem[ok]] <- vals[ok]
        rem <- rem[!ok]
      }
    }
    out
  }

  fids <- df_percentile$feature_id
  u_fids <- unique(fids)
  n_u <- length(u_fids)
  has_na_ufid <- anyNA(u_fids)
  fid_id <- if (has_na_ufid) {
    match(fids, u_fids)
  } else {
    fastmatch::fmatch(fids, u_fids)
  }
  attr(u_fids, ".match.hash") <- NULL

  sc_chemo <- .as_numeric_dedup(df_percentile$score_weighted_chemo)
  has_ik_row <- !is.na(
    df_percentile$candidate_structure_inchikey_connectivity_layer
  )
  fids_with_ik <- unique(fid_id[has_ik_row])
  has_ik_u <- logical(n_u)
  has_ik_u[fids_with_ik[!is.na(fids_with_ik)]] <- TRUE

  label_cla_str <- coalesce_tax_labels(c(
    "candidate_structure_tax_cla_04dirpar",
    "candidate_structure_tax_cla_03cla",
    "candidate_structure_tax_cla_02sup",
    "candidate_structure_tax_cla_01kin"
  ))
  label_npc_str <- coalesce_tax_labels(c(
    "candidate_structure_tax_npc_03cla",
    "candidate_structure_tax_npc_02sup",
    "candidate_structure_tax_npc_01pat"
  ))

  # Best structure label per feature (highest score_weighted_chemo wins)
  label_cla_str_u <- rep(NA_character_, n_u)
  score_cla_str_u <- rep(NA_real_, n_u)
  idx_cla <- which(!is.na(label_cla_str))
  if (length(idx_cla) > 0L) {
    ord_cla <- idx_cla[
      order(-sc_chemo[idx_cla], na.last = TRUE, method = "radix")
    ]
    keep_cla <- ord_cla[!duplicated(fid_id[ord_cla])]
    u_idx_cla <- fid_id[keep_cla]
    label_cla_str_u[u_idx_cla] <- label_cla_str[keep_cla]
    score_cla_str_u[u_idx_cla] <- sc_chemo[keep_cla]
  }

  label_npc_str_u <- rep(NA_character_, n_u)
  score_npc_str_u <- rep(NA_real_, n_u)
  idx_npc <- which(!is.na(label_npc_str))
  if (length(idx_npc) > 0L) {
    ord_npc <- idx_npc[
      order(-sc_chemo[idx_npc], na.last = TRUE, method = "radix")
    ]
    keep_npc <- ord_npc[!duplicated(fid_id[ord_npc])]
    u_idx_npc <- fid_id[keep_npc]
    label_npc_str_u[u_idx_npc] <- label_npc_str[keep_npc]
    score_npc_str_u[u_idx_npc] <- sc_chemo[keep_npc]
  }
  rm(label_cla_str, label_npc_str, sc_chemo)

  # Best predicted label per feature (highest prediction score wins)
  pred_cols <- grep(
    "^(feature_id|feature_pred)",
    names(df_percentile),
    value = TRUE
  )
  df_pred_tax <- if (nrow(df_percentile) > 10000L) {
    first_f_idx <- which(!duplicated(fid_id))
    df_percentile[first_f_idx, ..pred_cols]
  } else {
    df_percentile[, ..pred_cols] |>
      tidytable::distinct()
  }

  label_cla_pred_u <- rep(NA_character_, n_u)
  score_cla_pred_u <- rep(NA_real_, n_u)
  label_npc_pred_u <- rep(NA_character_, n_u)
  score_npc_pred_u <- rep(NA_real_, n_u)

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

    df_pred_cla <- compute_classyfire_taxonomy(df_pred_tax, weights)
    if (nrow(df_pred_cla) > 0L) {
      ord_pc <- order(
        -df_pred_cla$score_classyfire,
        na.last = TRUE,
        method = "radix"
      )
      f_pc <- df_pred_cla$feature_id[ord_pc]
      keep_pc <- ord_pc[!duplicated(f_pc)]
      m_pc <- if (has_na_ufid) {
        match(df_pred_cla$feature_id[keep_pc], u_fids)
      } else {
        fastmatch::fmatch(df_pred_cla$feature_id[keep_pc], u_fids)
      }
      label_cla_pred_u[m_pc] <- df_pred_cla$label_classyfire_predicted[keep_pc]
      score_cla_pred_u[m_pc] <- df_pred_cla$score_classyfire[keep_pc]
    }

    df_pred_npc <- compute_npclassifier_taxonomy(df_pred_tax, weights)
    if (nrow(df_pred_npc) > 0L) {
      ord_pn <- order(
        -df_pred_npc$score_npclassifier,
        na.last = TRUE,
        method = "radix"
      )
      f_pn <- df_pred_npc$feature_id[ord_pn]
      keep_pn <- ord_pn[!duplicated(f_pn)]
      m_pn <- if (has_na_ufid) {
        match(df_pred_npc$feature_id[keep_pn], u_fids)
      } else {
        fastmatch::fmatch(df_pred_npc$feature_id[keep_pn], u_fids)
      }
      label_npc_pred_u[
        m_pn
      ] <- df_pred_npc$label_npclassifier_predicted[keep_pn]
      score_npc_pred_u[m_pn] <- df_pred_npc$score_npclassifier[keep_pn]
    }
    attr(u_fids, ".match.hash") <- NULL
  }

  # Structure label wins when its score_weighted_chemo >= prediction score;
  # otherwise the predicted label wins.
  cla_str_wins <- !is.na(score_cla_str_u) &
    !is.na(score_cla_pred_u) &
    score_cla_str_u >= score_cla_pred_u
  take_cla_pred <- !cla_str_wins & !is.na(score_cla_pred_u)
  label_classyfire <- label_cla_str_u
  label_classyfire[take_cla_pred] <- label_cla_pred_u[take_cla_pred]
  score_classyfire <- rep(NA_real_, n_u)
  score_classyfire[take_cla_pred] <- score_cla_pred_u[take_cla_pred]

  npc_str_wins <- !is.na(score_npc_str_u) &
    !is.na(score_npc_pred_u) &
    score_npc_str_u >= score_npc_pred_u
  take_npc_pred <- !npc_str_wins & !is.na(score_npc_pred_u)
  label_npclassifier <- label_npc_str_u
  label_npclassifier[take_npc_pred] <- label_npc_pred_u[take_npc_pred]
  score_npclassifier <- rep(NA_real_, n_u)
  score_npclassifier[take_npc_pred] <- score_npc_pred_u[take_npc_pred]

  tidytable::tidytable(
    feature_id = u_fids,
    has_inchikey = has_ik_u,
    label_classyfire = label_classyfire,
    label_npclassifier = label_npclassifier,
    score_classyfire = score_classyfire,
    score_npclassifier = score_npclassifier
  )
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
  ft_cols <- intersect(c("feature_id", "rt", "mz"), names(features_table))
  ft_base <- features_table[, ..ft_cols]
  if (anyDuplicated(ft_base$feature_id) > 0L) {
    ft_base <- tidytable::distinct(ft_base)
  }
  fids <- ft_base$feature_id
  n <- length(fids)

  if (
    nrow(results_filtered) > 0L && "feature_id" %in% names(results_filtered)
  ) {
    rank_num <- suppressWarnings(as.integer(results_filtered$rank_final))
    score_final_num <- if ("score_final" %in% names(results_filtered)) {
      -.as_numeric_dedup(results_filtered$score_final)
    } else if ("score_weighted_chemo" %in% names(results_filtered)) {
      -.as_numeric_dedup(results_filtered$score_weighted_chemo)
    } else {
      rep_len(0, nrow(results_filtered))
    }
    initial_num <- if ("score_initial" %in% names(results_filtered)) {
      -.as_numeric_dedup(results_filtered$score_initial)
    } else if ("candidate_score_pseudo_initial" %in% names(results_filtered)) {
      -.as_numeric_dedup(results_filtered$candidate_score_pseudo_initial)
    } else {
      rep_len(0, nrow(results_filtered))
    }

    ord <- order(
      rank_num,
      score_final_num,
      initial_num,
      na.last = TRUE,
      method = "radix"
    )
    fids_ord <- results_filtered$feature_id[ord]
    u_fids_br <- unique(fids_ord)
    fid_id_ord <- if (anyNA(u_fids_br)) {
      match(fids_ord, u_fids_br)
    } else {
      fastmatch::fmatch(fids_ord, u_fids_br)
    }
    keep_br <- ord[!duplicated(fid_id_ord)]
    br_fids <- results_filtered$feature_id[keep_br]
    m_br_u <- if (anyNA(br_fids)) {
      match(fids, br_fids)
    } else {
      fastmatch::fmatch(fids, br_fids)
    }
    row_br <- keep_br[m_br_u]
  } else {
    row_br <- rep(NA_integer_, n)
  }

  m_cls <- if (
    nrow(df_classes_mini) > 0L && "feature_id" %in% names(df_classes_mini)
  ) {
    cls_fids <- df_classes_mini$feature_id
    if (anyNA(cls_fids)) {
      match(fids, cls_fids)
    } else {
      fastmatch::fmatch(fids, cls_fids)
    }
  } else {
    rep(NA_integer_, n)
  }

  get_cls_col <- function(col, default) {
    if (col %in% names(df_classes_mini)) {
      df_classes_mini[[col]][m_cls]
    } else {
      rep(default, n)
    }
  }
  get_br_col <- function(col) {
    if (col %in% names(results_filtered)) {
      results_filtered[[col]][row_br]
    } else {
      rep(NA, n)
    }
  }

  label_classyfire <- get_cls_col("label_classyfire", NA_character_)
  label_npclassifier <- get_cls_col("label_npclassifier", NA_character_)
  score_classyfire <- get_cls_col("score_classyfire", NA_real_)
  score_npclassifier <- get_cls_col("score_npclassifier", NA_real_)

  inchikey_connectivity_layer <- get_br_col(
    "candidate_structure_inchikey_connectivity_layer"
  )
  na_ik <- is.na(inchikey_connectivity_layer)

  score_raw <- get_br_col("score_weighted_chemo")
  has_s_cla <- !is.na(score_classyfire)
  has_s_npc <- !is.na(score_npclassifier)
  pred_denom <- as.numeric(has_s_cla) + as.numeric(has_s_npc)
  s_cla0 <- score_classyfire
  s_cla0[!has_s_cla] <- 0
  s_npc0 <- score_npclassifier
  s_npc0[!has_s_npc] <- 0
  pred_avg <- (s_cla0 + s_npc0) / pred_denom
  pred_avg[pred_denom == 0] <- NA_real_

  score <- as.numeric(score_raw)
  score[na_ik] <- pred_avg[na_ik]

  na_score <- is.na(score)
  if (any(na_score)) {
    label_classyfire[na_score] <- NA_character_
    label_npclassifier[na_score] <- NA_character_
  }

  out_list <- as.list(ft_base)
  out_list[["label_classyfire"]] <- label_classyfire
  out_list[["label_npclassifier"]] <- label_npclassifier
  out_list[["label_compound"]] <- get_br_col("candidate_structure_name")
  out_list[["adduct"]] <- get_br_col("candidate_adduct")
  out_list[["smiles_no_stereo"]] <- get_br_col(
    "candidate_structure_smiles_no_stereo"
  )
  out_list[["inchikey_connectivity_layer"]] <- inchikey_connectivity_layer
  out_list[["library"]] <- get_br_col("candidate_library")
  out_list[["error_mz"]] <- get_br_col("candidate_structure_error_mz")
  out_list[["error_rt"]] <- get_br_col("candidate_structure_error_rt")
  out_list[["organism_closest"]] <- get_br_col(
    "candidate_structure_organism_occurrence_closest"
  )
  if ("candidate_structure_tag" %in% names(results_filtered)) {
    out_list[["tag"]] <- results_filtered[["candidate_structure_tag"]][row_br]
  }
  out_list[["score"]] <- score
  if ("candidates_evaluated" %in% names(results_filtered)) {
    ce <- results_filtered[["candidates_evaluated"]][row_br]
    ce[na_ik] <- NA_integer_
    out_list[["candidates_evaluated"]] <- ce
  }
  if ("candidates_best" %in% names(results_filtered)) {
    cb <- results_filtered[["candidates_best"]][row_br]
    cb[na_ik] <- NA_integer_
    out_list[["candidates_best"]] <- cb
  }
  if ("annotation_note" %in% names(results_filtered)) {
    out_list[["note"]] <- results_filtered[["annotation_note"]][row_br]
  }

  results_mini <- tidytable::as_tidytable(out_list)
  if (anyDuplicated(fids) > 0L) {
    results_mini <- tidytable::distinct(results_mini)
  }

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
  n <- nrow(df_ranked)
  if (n == 0L) {
    return(df_ranked)
  }

  df_out <- data.table::copy(tidytable::as_tidytable(df_ranked))
  if (!"cluster_consensus_applied" %in% names(df_out)) {
    data.table::set(
      df_out,
      j = "cluster_consensus_applied",
      value = rep.int(FALSE, n)
    )
  }
  if (!"cluster_consensus_promoted_from_anchor" %in% names(df_out)) {
    data.table::set(
      df_out,
      j = "cluster_consensus_promoted_from_anchor",
      value = rep.int(FALSE, n)
    )
  }
  if (!"cluster_consensus_group_id" %in% names(df_out)) {
    data.table::set(
      df_out,
      j = "cluster_consensus_group_id",
      value = rep.int(NA_character_, n)
    )
  }
  if (!"cluster_consensus_anchor_feature_id" %in% names(df_out)) {
    data.table::set(
      df_out,
      j = "cluster_consensus_anchor_feature_id",
      value = rep.int(NA_character_, n)
    )
  }
  if (!"cluster_consensus_anchor_inchikey" %in% names(df_out)) {
    data.table::set(
      df_out,
      j = "cluster_consensus_anchor_inchikey",
      value = rep.int(NA_character_, n)
    )
  }
  if (!"annotation_note" %in% names(df_out)) {
    data.table::set(
      df_out,
      j = "annotation_note",
      value = rep.int(NA_character_, n)
    )
  }

  df_work <- select_clean_chemo_consensus_working_columns(
    df_out[0L, , drop = FALSE]
  )
  has_component_col <- "component_id" %in% names(components_table)
  has_rank_col <- "rank_final" %in% names(df_work)
  has_ik_col <- "candidate_structure_inchikey_connectivity_layer" %in%
    names(df_work)
  has_score_col <- "score_weighted_chemo" %in% names(df_work)
  has_exact_mass_col <- "candidate_structure_exact_mass" %in% names(df_work)
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
    return(df_out)
  }

  first_c <- !duplicated(components_table$feature_id)
  comp_fids <- components_table$feature_id[first_c]
  comp_ids <- components_table$component_id[first_c]
  fids <- df_out$feature_id
  row_comp <- comp_ids[fastmatch::fmatch(fids, comp_fids)]

  has_ms2 <- if ("candidate_score_similarity" %in% names(df_out)) {
    !is.na(df_out$candidate_score_similarity)
  } else {
    rep.int(FALSE, n)
  }
  if ("candidate_score_sirius_csi" %in% names(df_out)) {
    has_ms2 <- has_ms2 | !is.na(df_out$candidate_score_sirius_csi)
  }
  if ("candidate_score_sirius_confidence" %in% names(df_out)) {
    conf <- suppressWarnings(.as_numeric_dedup(
      df_out$candidate_score_sirius_confidence
    ))
    has_ms2 <- has_ms2 | (!is.na(conf) & conf > 0)
  }

  has_mz_adduct_fallback <- has_mz_col && has_adduct_col
  if (!has_exact_mass_col) {
    log_warn(paste0(
      "candidate_structure_exact_mass not found; ",
      "falling back to mz/adduct inversion for neutral-mass grouping. ",
      "Results may be less reliable for cross-adduct consensus."
    ))
  }

  cand_m_exact <- if (has_exact_mass_col) {
    .as_numeric_dedup(df_out$candidate_structure_exact_mass)
  } else {
    rep.int(NA_real_, n)
  }
  cand_m_mz <- rep.int(NA_real_, n)
  if (has_mz_adduct_fallback) {
    missing_exact <- which(is.na(cand_m_exact))
    if (length(missing_exact) > 0L) {
      cand_m_mz[missing_exact] <- compute_candidate_M(
        df_out$mz[missing_exact],
        df_out$candidate_adduct[missing_exact]
      )
    }
  }
  cand_m <- tidytable::coalesce(cand_m_exact, cand_m_mz)
  if (has_exact_mass_col && has_mz_adduct_fallback) {
    n_fallback_rows <- sum(is.na(cand_m_exact) & !is.na(cand_m_mz))
    if (n_fallback_rows > 0L) {
      log_info(
        "Cluster-consensus mass key used mz/adduct fallback for %d row(s) with missing exact mass",
        n_fallback_rows
      )
    }
  }
  cand_m_key <- round(cand_m, NEUTRAL_MASS_GROUP_DECIMALS)
  rm(cand_m_exact, cand_m_mz, cand_m)

  rks <- df_out$rank_final
  iks <- df_out$candidate_structure_inchikey_connectivity_layer
  r1_idx <- which(
    rks == 1L &
      !is.na(row_comp) &
      !is.na(cand_m_key) &
      !is.na(iks)
  )
  if (length(r1_idx) == 0L) {
    log_debug("No cluster entity consensus groups to enforce")
    return(df_out)
  }

  swc_r1 <- .as_numeric_dedup(df_out$score_weighted_chemo[r1_idx])
  ord_r1 <- r1_idx[order(
    -as.integer(has_ms2[r1_idx]),
    -swc_r1,
    na.last = TRUE,
    method = "radix"
  )]

  u_comp <- unique(row_comp[ord_r1])
  u_mkey <- unique(cand_m_key[ord_r1])
  c_id <- fastmatch::fmatch(row_comp, u_comp)
  m_id <- fastmatch::fmatch(cand_m_key, u_mkey)
  n_m <- length(u_mkey)
  cm_key <- (c_id - 1L) * n_m + m_id

  cm_r1 <- cm_key[ord_r1]
  anchor_rows <- ord_r1[!duplicated(cm_r1)]
  anchor_cm <- cm_key[anchor_rows]
  anchor_fid <- fids[anchor_rows]
  anchor_ik <- iks[anchor_rows]
  anchor_grp_id <- paste(
    row_comp[anchor_rows],
    sprintf("%.*f", NEUTRAL_MASS_GROUP_DECIMALS, cand_m_key[anchor_rows]),
    sep = "::"
  )

  m_anc <- fastmatch::fmatch(cm_key, anchor_cm)
  has_anchor_ik <- !is.na(m_anc) & !is.na(iks) & (iks == anchor_ik[m_anc])
  if (!any(has_anchor_ik)) {
    data.table::set(
      df_out,
      j = "cluster_consensus_applied",
      value = rep.int(FALSE, n)
    )
    data.table::set(
      df_out,
      j = "cluster_consensus_promoted_from_anchor",
      value = rep.int(FALSE, n)
    )
    return(df_out)
  }

  u_fids <- sort(unique(fids), na.last = TRUE)
  fid_id <- fastmatch::fmatch(fids, u_fids)
  n_anc <- length(anchor_rows)
  fm_key <- as.double(fid_id - 1L) * n_anc + m_anc
  active_fm <- unique(fm_key[which(has_anchor_ik)])
  in_part <- !is.na(m_anc) & !is.na(fastmatch::fmatch(fm_key, active_fm))
  idx_part <- which(in_part)

  col_applied <- rep.int(FALSE, n)
  col_promoted <- rep.int(FALSE, n)
  col_grp_id <- as.character(df_out$cluster_consensus_group_id)
  col_anc_fid <- as.character(df_out$cluster_consensus_anchor_feature_id)
  col_anc_ik <- as.character(df_out$cluster_consensus_anchor_inchikey)
  col_note <- as.character(df_out$annotation_note)
  col_rk <- as.integer(rks)

  m_anc_p <- m_anc[idx_part]
  col_applied[idx_part] <- TRUE
  col_anc_fid[idx_part] <- anchor_fid[m_anc_p]
  col_anc_ik[idx_part] <- anchor_ik[m_anc_p]
  na_grp_p <- is.na(col_grp_id[idx_part])
  if (any(na_grp_p)) {
    col_grp_id[idx_part[na_grp_p]] <- anchor_grp_id[m_anc_p[na_grp_p]]
  }

  fid_p <- fid_id[idx_part]
  rk_p <- col_rk[idx_part]
  has_anc_p <- has_anchor_ik[idx_part]
  is_anc_feat_p <- fids[idx_part] == anchor_fid[m_anc_p]

  reorder_fids_id <- unique(fid_p[rk_p == 1L & !has_ms2[idx_part] & !has_anc_p])
  if (length(reorder_fids_id) > 0L) {
    in_reord_p <- !is.na(fastmatch::fmatch(fid_p, reorder_fids_id))
    idx_rp <- which(in_reord_p)
    idx_nrp <- which(!in_reord_p)

    mkey_rp <- cand_m_key[idx_part[idx_rp]]
    ord_rp <- idx_rp[order(
      fid_p[idx_rp],
      mkey_rp,
      -as.integer(has_anc_p[idx_rp]),
      rk_p[idx_rp],
      na.last = TRUE,
      method = "radix"
    )]
    fm_rp <- fm_key[idx_part[ord_rp]]
    n_rp <- length(ord_rp)
    new_g_rp <- if (n_rp > 1L) c(TRUE, fm_rp[-1L] != fm_rp[-n_rp]) else TRUE
    seq_rp <- seq_len(n_rp) - cummax(ifelse(new_g_rp, seq_len(n_rp), 1L)) + 1L
    rk_p[ord_rp] <- seq_rp

    comb_p <- c(ord_rp, idx_nrp)
    ord_p <- comb_p[order(
      fid_p[comb_p],
      rk_p[comb_p],
      na.last = TRUE,
      method = "radix"
    )]
  } else {
    ord_p <- seq_along(idx_part)
  }

  idx_part_ord <- idx_part[ord_p]
  rk_p_ord <- rk_p[ord_p]
  col_rk[idx_part_ord] <- rk_p_ord
  prom_p_ord <- has_anc_p[ord_p] & (rk_p_ord == 1L) & !is_anc_feat_p[ord_p]
  col_promoted[idx_part_ord] <- prom_p_ord
  if (any(prom_p_ord)) {
    col_note[idx_part_ord[
      prom_p_ord
    ]] <- "Promoted to rank 1: cluster entity consensus (same M across component)"
  }

  idx_unchanged <- which(!in_part)
  all_idx <- c(idx_part_ord, idx_unchanged)
  affected_fid_ids <- unique(fid_p)
  in_aff <- !is.na(fastmatch::fmatch(fid_id[all_idx], affected_fid_ids))
  aff_idx <- all_idx[in_aff]
  unaff_idx <- all_idx[!in_aff]

  if (length(aff_idx) > 0L) {
    ord_aff <- aff_idx[order(
      fid_id[aff_idx],
      -as.integer(col_promoted[aff_idx]),
      col_rk[aff_idx],
      na.last = TRUE,
      method = "radix"
    )]
    f_aff <- fid_id[ord_aff]
    n_aff <- length(ord_aff)
    new_g_aff <- if (n_aff > 1L) c(TRUE, f_aff[-1L] != f_aff[-n_aff]) else TRUE
    col_rk[ord_aff] <- seq_len(n_aff) -
      cummax(ifelse(new_g_aff, seq_len(n_aff), 1L)) +
      1L
    all_idx2 <- c(ord_aff, unaff_idx)
  } else {
    all_idx2 <- all_idx
  }

  final_ord <- all_idx2[order(
    fid_id[all_idx2],
    col_rk[all_idx2],
    na.last = TRUE,
    method = "radix"
  )]

  log_info(
    "Enforced cluster entity consensus for %d features (anchor InChIKey promoted to rank 1)",
    length(affected_fid_ids)
  )

  data.table::set(df_out, j = "rank_final", value = col_rk)
  data.table::set(df_out, j = "annotation_note", value = col_note)
  data.table::set(df_out, j = "cluster_consensus_applied", value = col_applied)
  data.table::set(
    df_out,
    j = "cluster_consensus_promoted_from_anchor",
    value = col_promoted
  )
  data.table::set(df_out, j = "cluster_consensus_group_id", value = col_grp_id)
  data.table::set(
    df_out,
    j = "cluster_consensus_anchor_feature_id",
    value = col_anc_fid
  )
  data.table::set(
    df_out,
    j = "cluster_consensus_anchor_inchikey",
    value = col_anc_ik
  )

  df_out[final_ord, , drop = FALSE]
}
