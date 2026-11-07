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
  has_adduct_col <- "candidate_adduct" %in% names(df)
  adds <- if (has_adduct_col) {
    df$candidate_adduct
  } else {
    rep.int(NA_character_, n_rows)
  }
  rks <- df$rank_final

  u_fids <- unique(fids)
  u_adds <- unique(adds)
  u_rks <- unique(rks)
  fid_id <- if (anyNA(u_fids)) {
    match(fids, u_fids)
  } else {
    fastmatch::fmatch(fids, u_fids)
  }
  add_id <- if (anyNA(u_adds)) {
    match(adds, u_adds)
  } else {
    fastmatch::fmatch(adds, u_adds)
  }
  rk_id <- if (anyNA(u_rks)) {
    match(rks, u_rks)
  } else {
    fastmatch::fmatch(rks, u_rks)
  }

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
  n_per_score <- grp_counts[grp_idx]
  full_anchor_match <- rep.int(FALSE, n_rows)

  has_ik_col <- "candidate_structure_inchikey_connectivity_layer" %in% names(df)
  has_M_cols <- "mz" %in% names(df) && has_adduct_col
  has_rt_feature_col <- "rt" %in% names(df)

  idx_anchor_kept <- integer(0L)
  if (apply_anchor_collapsing && has_ik_col && has_M_cols) {
    cand_M <- compute_candidate_M(df$mz, adds)
    iks <- df$candidate_structure_inchikey_connectivity_layer

    # anchors must come from strong rank-1 singletons (best-supported)
    anc_idx <- which(
      !is.na(rks) &
        rks == 1L &
        n_per_score == 1L &
        !is.na(iks) &
        !is.na(cand_M)
    )

    if (length(anc_idx) > 0L) {
      anc_idx <- anc_idx[order(cand_M[anc_idx], method = "radix")]
      anchor_tbl <- if (has_rt_feature_col) {
        tidytable::tidytable(
          .candidate_M = cand_M[anc_idx],
          candidate_structure_inchikey_connectivity_layer = iks[anc_idx],
          feature_id = fids[anc_idx],
          rt = df$rt[anc_idx]
        )
      } else {
        tidytable::tidytable(
          .candidate_M = cand_M[anc_idx],
          candidate_structure_inchikey_connectivity_layer = iks[anc_idx],
          feature_id = fids[anc_idx]
        )
      }

      tol <- NEUTRAL_MASS_MATCH_TOLERANCE_DA
      rt_tol <- DEFAULT_HE_MAX_RT_ERROR_MIN
      # Only match TIED rows to cross-feature anchors
      # Non-tied rows (rank-1 singletons) should not be affected by cross-feature anchors
      tied_idx <- which(n_per_score > 1L)
      if (length(tied_idx) > 0L) {
        df_tied_min <- if (has_rt_feature_col) {
          tidytable::tidytable(
            .candidate_M = cand_M[tied_idx],
            candidate_structure_inchikey_connectivity_layer = iks[tied_idx],
            feature_id = fids[tied_idx],
            rt = df$rt[tied_idx]
          )
        } else {
          tidytable::tidytable(
            .candidate_M = cand_M[tied_idx],
            candidate_structure_inchikey_connectivity_layer = iks[tied_idx],
            feature_id = fids[tied_idx]
          )
        }
        anchor_match <- .match_tied_rows_to_anchors(
          df_tied = df_tied_min,
          anchor_tbl = anchor_tbl,
          tol = tol,
          rt_tol = rt_tol,
          has_rt_feature_col = has_rt_feature_col
        )
        full_anchor_match[tied_idx] <- anchor_match
      }

      idx_anchor_kept <- which(full_anchor_match)
      if (length(idx_anchor_kept) > 0L) {
        ord_ak <- list(fids[idx_anchor_kept], rks[idx_anchor_kept])
        if ("score_weighted_chemo" %in% names(df)) {
          ord_ak[[length(ord_ak) + 1L]] <- -.as_numeric_dedup(
            df$score_weighted_chemo[idx_anchor_kept]
          )
        }
        if ("candidate_score_pseudo_initial" %in% names(df)) {
          ord_ak[[length(ord_ak) + 1L]] <- -.as_numeric_dedup(
            df$candidate_score_pseudo_initial[idx_anchor_kept]
          )
        }
        ak_idx <- do.call(
          order,
          c(ord_ak, list(na.last = TRUE, method = "radix"))
        )
        idx_anchor_kept <- idx_anchor_kept[ak_idx]
      }
    }
  }

  # For groups where any tied row matched a cross-feature anchor, collapse
  # the tied group to only the matched anchor rows.
  if (length(idx_anchor_kept) > 0L) {
    in_anchor_grp <- logical(length(u_grp))
    in_anchor_grp[unique(grp_idx[idx_anchor_kept])] <- TRUE
    rem_idx <- which(!(in_anchor_grp[grp_idx] & !full_anchor_match))
  } else {
    rem_idx <- seq_len(n_rows)
  }

  # Count how many distinct tied groups require sampling and partition directly
  # by integer row indices to avoid materializing intermediate table slices.
  n_ps_rem <- n_per_score[rem_idx]
  idx_sample <- rem_idx[n_ps_rem > max_per_score]
  n_sampled_features <- if (length(idx_sample) > 0L) {
    length(unique(fids[idx_sample]))
  } else {
    0L
  }

  has_rt_col <- "candidate_structure_error_rt" %in% names(df)

  idx_untied <- rem_idx[n_ps_rem == 1L]
  idx_keep_remaining <- rem_idx[n_ps_rem > 1L & n_ps_rem <= max_per_score]

  # Sample only within each tied score group (feature_id, candidate_adduct, rank_final)
  if (length(idx_sample) > 0L) {
    ord_list <- list(fids[idx_sample])
    if (has_adduct_col) {
      ord_list[[length(ord_list) + 1L]] <- adds[idx_sample]
    }
    ord_list[[length(ord_list) + 1L]] <- rks[idx_sample]

    if ("candidate_score_similarity" %in% names(df)) {
      ord_list[[length(ord_list) + 1L]] <- -.as_numeric_dedup(
        df$candidate_score_similarity[idx_sample]
      )
    }
    if ("candidate_score_sirius_csi" %in% names(df)) {
      ord_list[[length(ord_list) + 1L]] <- -.as_numeric_dedup(
        df$candidate_score_sirius_csi[idx_sample]
      )
    }
    if ("candidate_score_sirius_confidence" %in% names(df)) {
      ord_list[[length(ord_list) + 1L]] <- -.as_numeric_dedup(
        df$candidate_score_sirius_confidence[idx_sample]
      )
    }
    if (has_rt_col) {
      ord_list[[length(ord_list) + 1L]] <- -as.integer(
        !is.na(df$candidate_structure_error_rt[idx_sample])
      )
    }
    if ("cluster_consensus_promoted_from_anchor" %in% names(df)) {
      cc_sub <- df$cluster_consensus_promoted_from_anchor[idx_sample]
      ord_list[[length(ord_list) + 1L]] <- -as.integer(
        !is.na(cc_sub) & cc_sub
      )
    }
    if ("score_weighted_chemo" %in% names(df)) {
      ord_list[[length(ord_list) + 1L]] <- -.as_numeric_dedup(
        df$score_weighted_chemo[idx_sample]
      )
    }
    if ("candidate_score_pseudo_initial" %in% names(df)) {
      ord_list[[length(ord_list) + 1L]] <- -.as_numeric_dedup(
        df$candidate_score_pseudo_initial[idx_sample]
      )
    }

    # Perform a single global order on the sampled row indices
    ord <- do.call(order, c(ord_list, list(na.last = TRUE, method = "radix")))
    idx_ns_ord <- idx_sample[ord]

    # Since idx_ns_ord is sorted by (feature_id, candidate_adduct, rank_final),
    # each tied group is a contiguous run. Compute within-group sequence in O(N).
    n_ns <- length(idx_ns_ord)
    f_ns <- fid_id[idx_ns_ord]
    a_ns <- add_id[idx_ns_ord]
    r_ns <- rk_id[idx_ns_ord]
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
    idx_seq <- seq_len(n_ns)
    seq_in_group <- idx_seq - cummax(ifelse(is_new_grp, idx_seq, 1L)) + 1L
    idx_sampled <- idx_ns_ord[which(seq_in_group <= max_per_score)]
  } else {
    idx_sampled <- integer(0L)
  }

  idx_uk <- c(idx_untied, idx_keep_remaining)
  n_uk <- length(idx_uk)
  n_ak <- length(idx_anchor_kept)
  n_sm <- length(idx_sampled)
  comb_idx <- c(idx_uk, idx_anchor_kept, idx_sampled)

  notes_uk <- if ("annotation_note" %in% names(df) && n_uk > 0L) {
    as.character(df$annotation_note[idx_uk])
  } else {
    rep.int(NA_character_, n_uk)
  }
  merge_existing_note <- function(idx_vec, new_notes) {
    if (!"annotation_note" %in% names(df) || length(idx_vec) == 0L) {
      return(new_notes)
    }
    prev <- as.character(df$annotation_note[idx_vec])
    has_prev <- !is.na(prev) & nzchar(trimws(prev))
    if (any(has_prev)) {
      new_notes[has_prev] <- paste(
        prev[has_prev],
        new_notes[has_prev],
        sep = " | "
      )
    }
    new_notes
  }
  notes_ak <- if (n_ak > 0L) {
    merge_existing_note(
      idx_anchor_kept,
      rep.int(
        paste0(
          "Kept candidate matching best-supported InChIKey from sibling ",
          "feature with same neutral mass (different adduct)"
        ),
        n_ak
      )
    )
  } else {
    character(0L)
  }
  notes_sm <- if (n_sm > 0L) {
    merge_existing_note(
      idx_sampled,
      paste0(
        "Sampled ",
        max_per_score,
        " of ",
        n_per_score[idx_sampled],
        " tied candidates"
      )
    )
  } else {
    character(0L)
  }
  comb_note <- c(notes_uk, notes_ak, notes_sm)

  uk_present <- !is.na(notes_uk)
  if (any(uk_present)) {
    uk_non_na <- which(uk_present)
    uk_present[uk_non_na] <- nzchar(trimws(notes_uk[uk_non_na]))
  }
  comb_note_present <- c(uk_present, rep.int(TRUE, n_ak + n_sm))

  # Deduplicate explicitly by core identity columns + InChIKey when available
  if (has_ik_col && length(comb_idx) > 0L) {
    iks_all <- df$candidate_structure_inchikey_connectivity_layer
    u_f <- sort(u_fids, na.last = TRUE)
    u_a <- u_adds
    u_r <- sort(u_rks, na.last = TRUE)
    u_k <- unique(iks_all)

    f_id_all <- if (anyNA(u_f)) {
      match(fids, u_f)
    } else {
      fastmatch::fmatch(fids, u_f)
    }
    r_id_all <- if (anyNA(u_r)) match(rks, u_r) else fastmatch::fmatch(rks, u_r)
    k_id_all <- if (anyNA(u_k)) {
      match(iks_all, u_k)
    } else {
      fastmatch::fmatch(iks_all, u_k)
    }

    f_id_c <- f_id_all[comb_idx]
    a_id_c <- add_id[comb_idx]
    r_id_c <- r_id_all[comb_idx]
    k_id_c <- k_id_all[comb_idx]
    note_p <- -as.integer(comb_note_present)

    ord_t <- order(f_id_c, r_id_c, note_p, na.last = TRUE, method = "radix")

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
      key <- (((as.numeric(f_id_c[ord_t] - 1L) *
        na_u +
        (a_id_c[ord_t] - 1L)) *
        nr_u +
        (r_id_c[ord_t] - 1L)) *
        nk_u) +
        k_id_c[ord_t]
    } else {
      ord_rows <- comb_idx[ord_t]
      key <- paste0(
        fids[ord_rows],
        "\u001f",
        adds[ord_rows],
        "\u001f",
        rks[ord_rows],
        "\u001f",
        iks_all[ord_rows]
      )
    }
    keep_ord <- ord_t[!duplicated(key)]
    final_idx <- comb_idx[keep_ord]
    final_note <- comb_note[keep_ord]
  } else {
    final_idx <- comb_idx
    final_note <- comb_note
  }

  keep_cols_df <- setdiff(
    names(df),
    c(
      ".n_per_score",
      "n_per_score",
      ".anchor_match",
      ".candidate_M",
      ".note_present",
      "annotation_note"
    )
  )
  df_result <- tidytable::as_tidytable(
    lapply(as.list(df)[keep_cols_df], `[`, final_idx)
  )
  if ("cluster_consensus_promoted_from_anchor" %in% names(df_result)) {
    cc_col <- df_result$cluster_consensus_promoted_from_anchor
    if (anyNA(cc_col)) {
      cc_col[is.na(cc_col)] <- FALSE
      data.table::set(
        df_result,
        j = "cluster_consensus_promoted_from_anchor",
        value = cc_col
      )
    }
  } else {
    data.table::set(
      df_result,
      j = "cluster_consensus_promoted_from_anchor",
      value = rep.int(FALSE, length(final_idx))
    )
  }

  annotation_notes_lookup <- tidytable::tidytable()
  has_note_idx <- which(!is.na(final_note))
  if (length(has_note_idx) > 0L) {
    note_key_cols <- intersect(
      c(
        "feature_id",
        "candidate_adduct",
        "candidate_structure_inchikey_connectivity_layer"
      ),
      names(df_result)
    )
    notes_list <- lapply(as.list(df_result)[note_key_cols], `[`, has_note_idx)
    notes_list[["annotation_note"]] <- final_note[has_note_idx]
    notes_sub <- tidytable::distinct(tidytable::as_tidytable(notes_list))
    if (anyDuplicated(as.data.frame(as.list(notes_sub)[note_key_cols])) == 0L) {
      annotation_notes_lookup <- notes_sub
    } else {
      annotation_notes_lookup <- notes_sub |>
        tidytable::summarize(
          annotation_note = paste(unique(annotation_note), collapse = " | "),
          .by = tidyselect::all_of(note_key_cols)
        )
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
    drop_name_col <- function(tbl) {
      if (is.null(tbl) || !"candidate_structure_name" %in% names(tbl)) {
        return(tbl)
      }
      keep <- setdiff(names(tbl), "candidate_structure_name")
      if (inherits(tbl, "tidytable")) {
        tidytable::as_tidytable(as.list(tbl)[keep])
      } else {
        tbl[, keep, drop = FALSE]
      }
    }
    results_list$mini <- drop_name_col(results_list$mini)
    results_list$filtered <- drop_name_col(results_list$filtered)
    results_list$full <- drop_name_col(results_list$full)
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

  needs_convert <- cols_to_convert[
    !vapply(
      as.list(annot_table_wei_chemo)[cols_to_convert],
      is.numeric,
      logical(1L)
    )
  ]
  if (
    length(needs_convert) == 0L && inherits(annot_table_wei_chemo, "tidytable")
  ) {
    return(annot_table_wei_chemo)
  }

  annot_table_wei_chemo <- tidytable::as_tidytable(
    as.list(annot_table_wei_chemo)
  )
  for (col in needs_convert) {
    val <- annot_table_wei_chemo[[col]]
    u <- unique(val)
    u_num <- as.numeric(u)
    idx <- if (anyNA(u)) match(val, u) else fastmatch::fmatch(val, u)
    data.table::set(annot_table_wei_chemo, j = col, value = u_num[idx])
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

  if (enforce_cluster_consensus) {
    df_ranked <- resolve_cross_rt_duplicate_candidates(df_ranked)
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
  df_pred_sub <- tidytable::as_tidytable(as.list(df_percentile)[pred_cols])
  df_pred_tax <- if (nrow(df_percentile) > 10000L) {
    first_f_idx <- which(!duplicated(fid_id))
    df_pred_sub[first_f_idx, ]
  } else {
    tidytable::distinct(df_pred_sub)
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
  ft_base <- tidytable::as_tidytable(as.list(features_table)[ft_cols])
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

  df_out <- tidytable::as_tidytable(as.list(df_ranked))
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
    sim <- suppressWarnings(
      .as_numeric_dedup(df_out$candidate_score_similarity)
    )
    !is.na(sim) & sim > 0
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
  base_cm_key <- (c_id - 1L) * n_m + m_id

  row_rt <- if ("rt" %in% names(df_out)) {
    suppressWarnings(.as_numeric_dedup(df_out$rt))
  } else if ("feature_rt" %in% names(df_out)) {
    suppressWarnings(.as_numeric_dedup(df_out$feature_rt))
  } else {
    rep.int(NA_real_, n)
  }
  rt_sub_id <- integer(n)
  fin_rt_idx <- which(!is.na(base_cm_key) & is.finite(row_rt))
  if (length(fin_rt_idx) > 0L) {
    ord_rt <- fin_rt_idx[order(
      base_cm_key[fin_rt_idx],
      row_rt[fin_rt_idx],
      method = "radix"
    )]
    bcm_s <- base_cm_key[ord_rt]
    rt_s <- row_rt[ord_rt]
    n_s <- length(ord_rt)
    new_rt_sub <- if (n_s > 1L) {
      c(
        TRUE,
        bcm_s[-1L] != bcm_s[-n_s] |
          (rt_s[-1L] - rt_s[-n_s]) > DEFAULT_HE_MAX_RT_ERROR_MIN
      )
    } else {
      TRUE
    }
    rt_sub_id[ord_rt] <- cumsum(new_rt_sub)
  }
  n_rt_sub <- max(1L, max(rt_sub_id, na.rm = TRUE) + 1L)
  cm_key <- as.double(base_cm_key - 1L) * n_rt_sub + (rt_sub_id + 1L)

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

  if (is.unsorted(final_ord)) {
    tidytable::as_tidytable(lapply(as.list(df_out), `[`, final_ord))
  } else {
    df_out
  }
}

#' Compute single-linkage RT cluster IDs per feature
#' @keywords internal
.compute_feature_rt_clusters <- function(
  fids,
  row_rt,
  rt_tolerance = DEFAULT_HE_MAX_RT_ERROR_MIN
) {
  n <- length(fids)
  if (n == 0L || !any(is.finite(row_rt))) {
    return(rep.int(NA_integer_, n))
  }
  fin <- which(!is.na(fids) & is.finite(row_rt))
  if (length(fin) == 0L) {
    return(rep.int(NA_integer_, n))
  }
  first_f <- fin[!duplicated(fids[fin])]
  if (length(first_f) < 2L) {
    return(rep.int(NA_integer_, n))
  }
  u_fids <- fids[first_f]
  u_rt <- row_rt[first_f]
  ord <- order(u_rt, u_fids, method = "radix")
  u_fids_s <- u_fids[ord]
  u_rt_s <- u_rt[ord]
  n_u <- length(u_rt_s)
  new_c <- c(TRUE, (u_rt_s[-1L] - u_rt_s[-n_u]) > rt_tolerance)
  u_cid <- cumsum(new_c)
  if (u_cid[[n_u]] < 2L) {
    return(rep.int(NA_integer_, n))
  }
  u_cid[fastmatch::fmatch(fids, u_fids_s)]
}

#' Compute per-row evidence priority and weight vectors for cross-RT disambiguation
#' @keywords internal
.cross_rt_row_priority_and_weights <- function(df) {
  n <- nrow(df)
  sim <- if ("candidate_score_similarity" %in% names(df)) {
    suppressWarnings(.as_numeric_dedup(df$candidate_score_similarity))
  } else {
    rep.int(NA_real_, n)
  }
  sim_w <- sim
  sim_w[!is.finite(sim_w) | sim_w < 0] <- 0

  has_ms2 <- !is.na(sim) & sim > 0
  if ("candidate_score_sirius_csi" %in% names(df)) {
    has_ms2 <- has_ms2 | !is.na(df$candidate_score_sirius_csi)
  }
  if ("candidate_score_sirius_confidence" %in% names(df)) {
    conf <- suppressWarnings(
      .as_numeric_dedup(df$candidate_score_sirius_confidence)
    )
    has_ms2 <- has_ms2 | (!is.na(conf) & conf > 0)
  }
  swc <- if ("score_weighted_chemo" %in% names(df)) {
    suppressWarnings(.as_numeric_dedup(df$score_weighted_chemo))
  } else {
    rep.int(0, n)
  }
  swc_sort <- swc
  swc_sort[!is.finite(swc_sort)] <- -Inf
  swc_w <- swc
  swc_w[!is.finite(swc_w)] <- 0

  spi <- if ("candidate_score_pseudo_initial" %in% names(df)) {
    suppressWarnings(.as_numeric_dedup(df$candidate_score_pseudo_initial))
  } else {
    rep.int(0, n)
  }
  spi_sort <- spi
  spi_sort[!is.finite(spi_sort)] <- -Inf
  spi_w <- spi
  spi_w[!is.finite(spi_w)] <- 0

  err_rt <- if ("candidate_structure_error_rt" %in% names(df)) {
    abs(suppressWarnings(.as_numeric_dedup(df$candidate_structure_error_rt)))
  } else {
    rep.int(Inf, n)
  }
  err_rt[!is.finite(err_rt)] <- Inf
  q_rt <- ifelse(is.finite(err_rt), 1 / (1 + err_rt), 0)

  err_mz <- if ("candidate_structure_error_mz" %in% names(df)) {
    abs(suppressWarnings(.as_numeric_dedup(df$candidate_structure_error_mz)))
  } else {
    rep.int(Inf, n)
  }
  err_mz[!is.finite(err_mz)] <- Inf
  q_mz <- ifelse(is.finite(err_mz), 1 / (1 + err_mz), 0)

  fids <- as.character(df$feature_id)
  iks <- if ("candidate_structure_inchikey_connectivity_layer" %in% names(df)) {
    as.character(df$candidate_structure_inchikey_connectivity_layer)
  } else {
    rep.int("", n)
  }
  adds <- if ("candidate_adduct" %in% names(df)) {
    as.character(df$candidate_adduct)
  } else {
    rep.int("", n)
  }

  ord <- order(
    -as.integer(has_ms2),
    -swc_sort,
    -sim_w,
    -spi_sort,
    err_rt,
    err_mz,
    fids,
    iks,
    adds,
    na.last = TRUE,
    method = "radix"
  )
  prio_rank <- integer(n)
  prio_rank[ord] <- seq_len(n)

  # Pure, order-independent hierarchical composite weight:
  # 1. Assigned candidate cardinality (100)
  # 2. MS2-supported assignments (10)
  # 3. Sum of score_weighted_chemo (1.0), similarity (1e-2), pseudo_initial (1e-4)
  # 4. RT and m/z accuracy coupled supermodularly with (1 + s_eff) so that when
  #    two features share isomer candidates with s_A > s_B, the feature with
  #    smaller |error_rt| or |error_mz| strictly wins the higher-scoring isomer,
  #    while truly identical evidence produces an exact tie (zero row-order bias).
  s_eff <- swc_w + 1e-2 * sim_w + 1e-4 * spi_w
  weights <- 100 +
    10 * as.double(has_ms2) +
    s_eff +
    (1 + s_eff) * (1e-5 * q_rt + 1e-6 * q_mz)

  list(prio_rank = prio_rank, weights = weights)
}

#' Solve exact maximum-weight bipartite assignment via Hungarian shortest augmenting paths
#'
#' @param weight_mat Numeric matrix (`K x M`) with `-Inf` for absent edges and
#'   positive weights for valid `(RT_cluster, InChIKey)` edges.
#' @return Integer vector of length `K` with matched column index in `1..M`, or
#'   `NA_integer_` when a row is best left unmatched.
#' @keywords internal
.solve_bipartite_max_weight <- function(weight_mat) {
  k <- nrow(weight_mat)
  m <- ncol(weight_mat)
  if (k == 0L || m == 0L) {
    return(rep.int(NA_integer_, k))
  }
  n_cols <- m + k
  big_cost <- 1e15
  cost <- matrix(big_cost, nrow = k, ncol = n_cols)
  fin <- is.finite(weight_mat)
  cost[, seq_len(m)][fin] <- -weight_mat[fin]
  for (i in seq_len(k)) {
    cost[i, m + i] <- 0
  }

  u <- numeric(k + 1L)
  v <- numeric(n_cols + 1L)
  p <- integer(n_cols + 1L)
  way <- integer(n_cols + 1L)

  for (i in seq_len(k)) {
    p[1L] <- i
    j0 <- 1L
    minv <- rep.int(Inf, n_cols + 1L)
    used <- logical(n_cols + 1L)
    repeat {
      used[j0] <- TRUE
      i0 <- p[j0]
      delta <- Inf
      j1 <- 1L
      for (j in 2L:(n_cols + 1L)) {
        if (!used[j]) {
          cur <- cost[i0, j - 1L] - u[i0 + 1L] - v[j]
          if (cur < minv[j]) {
            minv[j] <- cur
            way[j] <- j0
          }
          if (minv[j] < delta) {
            delta <- minv[j]
            j1 <- j
          }
        }
      }
      for (j in seq_len(n_cols + 1L)) {
        if (used[j]) {
          u[p[j] + 1L] <- u[p[j] + 1L] + delta
          v[j] <- v[j] - delta
        } else {
          minv[j] <- minv[j] - delta
        }
      }
      j0 <- j1
      if (p[j0] == 0L) {
        break
      }
    }
    repeat {
      j1 <- way[j0]
      p[j0] <- p[j1]
      j0 <- j1
      if (j0 == 1L) {
        break
      }
    }
  }

  match_col <- rep.int(NA_integer_, k)
  for (j in 2L:(m + 1L)) {
    r_idx <- p[j]
    if (r_idx > 0L && is.finite(weight_mat[r_idx, j - 1L])) {
      match_col[r_idx] <- j - 1L
    }
  }
  match_col
}

#' Compute total weight of a bipartite matching vector
#' @keywords internal
.bipartite_matching_weight <- function(w_mat, match_vec) {
  total <- 0
  for (ii in seq_along(match_vec)) {
    mj <- match_vec[[ii]]
    if (!is.na(mj)) {
      total <- total + w_mat[ii, mj]
    }
  }
  total
}

#' Compute globally optimal RT-cluster to InChIKey assignment for conflicting clusters
#' @keywords internal
.solve_optimal_rt_cluster_assignment <- function(
  fids,
  rt_cid,
  iks,
  valid_ik,
  col_rk,
  row_prio_rank,
  row_weights
) {
  r1_rows <- which(col_rk == 1L & valid_ik & !is.na(rt_cid))
  if (length(r1_rows) < 2L) {
    return(NULL)
  }
  r1_iks <- iks[r1_rows]
  if (anyDuplicated(r1_iks) == 0L) {
    return(NULL)
  }

  # Identify InChIKeys that appear at rank 1 in >= 2 distinct RT clusters
  dup_iks_cand <- sort(unique(r1_iks[duplicated(r1_iks)]))
  conflict_iks <- character(0L)
  for (ik in dup_iks_cand) {
    cids_ik <- unique(rt_cid[r1_rows[r1_iks == ik]])
    if (length(cids_ik) >= 2L) {
      conflict_iks <- c(conflict_iks, ik)
    }
  }
  if (length(conflict_iks) == 0L) {
    return(NULL)
  }

  # Expand conflict closure so promoting an alternative candidate that is rank 1
  # in another RT cluster jointly optimizes all coupled RT clusters together.
  active_cids <- sort(unique(rt_cid[r1_rows[r1_iks %in% conflict_iks]]))
  valid_rows <- which(valid_ik & !is.na(rt_cid))
  repeat {
    in_act <- valid_rows[rt_cid[valid_rows] %in% active_cids]
    pool_iks <- unique(iks[in_act])
    new_cids <- unique(rt_cid[r1_rows[r1_iks %in% pool_iks]])
    merged_cids <- sort(unique(c(active_cids, new_cids)))
    if (length(merged_cids) == length(active_cids)) {
      break
    }
    active_cids <- merged_cids
  }

  act_rows <- valid_rows[rt_cid[valid_rows] %in% active_cids]
  act_iks <- sort(unique(iks[act_rows]))
  k_all <- length(active_cids)
  m_all <- length(act_iks)

  cid_pos <- fastmatch::fmatch(rt_cid[act_rows], active_cids)
  ik_pos <- fastmatch::fmatch(iks[act_rows], act_iks)

  weight_mat <- matrix(-Inf, nrow = k_all, ncol = m_all)
  best_row_mat <- matrix(NA_integer_, nrow = k_all, ncol = m_all)
  for (idx in seq_along(act_rows)) {
    r <- act_rows[[idx]]
    ci <- cid_pos[[idx]]
    ij <- ik_pos[[idx]]
    w <- row_weights[[r]]
    prev_r <- best_row_mat[ci, ij]
    if (
      w > weight_mat[ci, ij] ||
        (w == weight_mat[ci, ij] &&
          !is.na(prev_r) &&
          row_prio_rank[[r]] < row_prio_rank[[prev_r]])
    ) {
      weight_mat[ci, ij] <- w
      best_row_mat[ci, ij] <- r
    }
  }

  # Solve connected components of the bipartite conflict graph independently
  comp_of_cid <- integer(k_all)
  comp_id <- 0L
  for (start_ci in seq_len(k_all)) {
    if (comp_of_cid[[start_ci]] != 0L) {
      next
    }
    comp_id <- comp_id + 1L
    queue <- start_ci
    comp_of_cid[[start_ci]] <- comp_id
    head_idx <- 1L
    while (head_idx <= length(queue)) {
      cur_ci <- queue[[head_idx]]
      head_idx <- head_idx + 1L
      js <- which(is.finite(weight_mat[cur_ci, ]))
      if (length(js) > 0L) {
        nbr_cis <- which(
          rowSums(is.finite(weight_mat[, js, drop = FALSE])) > 0L
        )
        new_nbrs <- nbr_cis[comp_of_cid[nbr_cis] == 0L]
        if (length(new_nbrs) > 0L) {
          comp_of_cid[new_nbrs] <- comp_id
          queue <- c(queue, new_nbrs)
        }
      }
    }
  }

  target_iks_by_cid <- stats::setNames(
    vector("list", k_all),
    as.character(active_cids)
  )
  optimal_cids_by_ik <- stats::setNames(vector("list", m_all), act_iks)
  holder_fids_by_ik <- stats::setNames(vector("list", m_all), act_iks)

  for (cc in seq_len(comp_id)) {
    sub_cis <- which(comp_of_cid == cc)
    sub_js <- which(
      colSums(is.finite(weight_mat[sub_cis, , drop = FALSE])) > 0L
    )
    k_sub <- length(sub_cis)
    m_sub <- length(sub_js)
    if (k_sub == 0L || m_sub == 0L) {
      next
    }
    sub_w <- weight_mat[sub_cis, sub_js, drop = FALSE]
    sub_match <- .solve_bipartite_max_weight(sub_w)
    w_star <- .bipartite_matching_weight(sub_w, sub_match)
    tol <- 1e-13 * max(1, abs(w_star))

    opt_edge <- matrix(FALSE, nrow = k_sub, ncol = m_sub)
    for (ii in seq_len(k_sub)) {
      mj <- sub_match[[ii]]
      if (!is.na(mj)) {
        opt_edge[ii, mj] <- TRUE
      }
    }
    for (ii in seq_len(k_sub)) {
      for (jj in seq_len(m_sub)) {
        if (!opt_edge[ii, jj] && is.finite(sub_w[ii, jj])) {
          w_rem <- if (k_sub == 1L || m_sub == 1L) {
            0
          } else {
            sub_w_rem <- sub_w[-ii, -jj, drop = FALSE]
            rem_match <- .solve_bipartite_max_weight(sub_w_rem)
            .bipartite_matching_weight(sub_w_rem, rem_match)
          }
          if ((w_star - (sub_w[ii, jj] + w_rem)) <= tol) {
            opt_edge[ii, jj] <- TRUE
          }
        }
      }
    }

    for (ii in seq_len(k_sub)) {
      ci <- sub_cis[[ii]]
      cid_chr <- as.character(active_cids[[ci]])
      opt_jj <- which(opt_edge[ii, ])
      target_iks_by_cid[[cid_chr]] <- act_iks[sub_js[opt_jj]]
    }
    for (jj in seq_len(m_sub)) {
      ij <- sub_js[[jj]]
      ik_val <- act_iks[[ij]]
      opt_ii <- which(opt_edge[, jj])
      if (length(opt_ii) > 0L) {
        cids_opt <- active_cids[sub_cis[opt_ii]]
        optimal_cids_by_ik[[ik_val]] <- cids_opt
        holders <- vapply(
          opt_ii,
          function(ii_idx) {
            ci <- sub_cis[[ii_idx]]
            fids[[best_row_mat[ci, ij]]]
          },
          character(1L)
        )
        names(holders) <- as.character(cids_opt)
        holder_fids_by_ik[[ik_val]] <- holders
      } else {
        optimal_cids_by_ik[[ik_val]] <- integer(0L)
        holder_fids_by_ik[[ik_val]] <- character(0L)
      }
    }
  }

  list(
    active_cids = active_cids,
    target_iks_by_cid = target_iks_by_cid,
    optimal_cids_by_ik = optimal_cids_by_ik,
    holder_fids_by_ik = holder_fids_by_ik
  )
}

#' Resolve duplicate rank-1 InChIKeys across distinct retention-time clusters
#'
#' @description Ensures two peaks eluting at different retention times
#'   (`> rt_tolerance` min apart) do not keep the same planar structure
#'   (`candidate_structure_inchikey_connectivity_layer`) at `rank_final = 1`.
#'
#'   Co-eluting ions within `rt_tolerance` (default `0.05` min, e.g. `[M+H]+`,
#'   `[M+Na]+`, and in-source fragments) are grouped into the same RT cluster
#'   and can share the same top candidate. Across distinct RT clusters, TIMA
#'   finds the best global assignment of candidates to peaks (via the Hungarian
#'   algorithm), prioritizing:
#'   1. Assigning a distinct candidate to as many RT clusters as possible,
#'   2. MS2-supported matches over MS1-only matches,
#'   3. Higher `score_weighted_chemo`, spectral similarity, and
#'      `candidate_score_pseudo_initial`,
#'   4. Smaller `|error_rt|` and `|error_mz|` (weighted by candidate score so
#'      the peak with smaller error gets the higher-scoring isomer).
#'
#'   Exact ties are never broken at random:
#'   - Tied runner-up candidates on the losing peak are promoted together to
#'     `rank_final = 1`.
#'   - If two RT clusters have 100% identical scores and errors across all
#'     metrics, all tied optimal candidates are kept at `rank_final = 1`.
#'
#'   Every promoted candidate receives an `annotation_note` (and `note` in
#'   `mini`):
#'   `"Promoted to rank 1: cross-RT duplicate InChIKey <IK> assigned to feature <id>"`
#'   (or `"tied with feature <id>"` on an exact cross-RT tie).
#'
#' @param df_ranked Ranked candidate table.
#' @param rt_tolerance Maximum RT difference (minutes) for co-eluting features.
#' @return Updated `df_ranked` with cross-RT rank-1 duplicates disambiguated.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Example 1: Asymmetric isomer scores across two retention times
#' # Feature 1 (RT = 2.0 min) has IK_A (0.90) and IK_B (0.89).
#' # Feature 2 (RT = 8.0 min) has IK_A (0.88) and IK_B (0.20).
#' # Greedy selection would give IK_A to Feature 1 and force Feature 2 onto IK_B
#' # (total score = 1.10). Optimal bipartite matching assigns IK_B (0.89) to
#' # Feature 1 and IK_A (0.88) to Feature 2 (total score = 1.77), and records a
#' # promotion note on Feature 1.
#' df <- tidytable::tidytable(
#'   feature_id = c("1", "1", "2", "2"),
#'   rt = c(2.0, 2.0, 8.0, 8.0),
#'   rank_final = c(1L, 2L, 1L, 2L),
#'   candidate_structure_inchikey_connectivity_layer = c(
#'     "IK_A", "IK_B", "IK_A", "IK_B"
#'   ),
#'   score_weighted_chemo = c(0.90, 0.89, 0.88, 0.20)
#' )
#' resolve_cross_rt_duplicate_candidates(df)
#' }
resolve_cross_rt_duplicate_candidates <- function(
  df_ranked,
  rt_tolerance = DEFAULT_HE_MAX_RT_ERROR_MIN
) {
  n <- nrow(df_ranked)
  if (n == 0L) {
    return(df_ranked)
  }
  req_cols <- c(
    "feature_id",
    "rank_final",
    "candidate_structure_inchikey_connectivity_layer"
  )
  if (!all(req_cols %in% names(df_ranked))) {
    return(df_ranked)
  }
  row_rt <- if ("rt" %in% names(df_ranked)) {
    suppressWarnings(.as_numeric_dedup(df_ranked$rt))
  } else if ("feature_rt" %in% names(df_ranked)) {
    suppressWarnings(.as_numeric_dedup(df_ranked$feature_rt))
  } else {
    return(df_ranked)
  }

  fids <- as.character(df_ranked$feature_id)
  rt_cid <- .compute_feature_rt_clusters(
    fids = fids,
    row_rt = row_rt,
    rt_tolerance = rt_tolerance
  )
  if (all(is.na(rt_cid))) {
    return(df_ranked)
  }

  iks <- as.character(
    df_ranked$candidate_structure_inchikey_connectivity_layer
  )
  valid_ik <- !is.na(iks) & nzchar(iks)
  col_rk <- as.integer(df_ranked$rank_final)

  r1_check <- which(col_rk == 1L & valid_ik & !is.na(rt_cid))
  if (length(r1_check) < 2L || anyDuplicated(iks[r1_check]) == 0L) {
    return(df_ranked)
  }

  pw <- .cross_rt_row_priority_and_weights(df_ranked)
  row_prio_rank <- pw$prio_rank
  row_weights <- pw$weights

  opt <- .solve_optimal_rt_cluster_assignment(
    fids = fids,
    rt_cid = rt_cid,
    iks = iks,
    valid_ik = valid_ik,
    col_rk = col_rk,
    row_prio_rank = row_prio_rank,
    row_weights = row_weights
  )
  if (is.null(opt)) {
    return(df_ranked)
  }

  df_out <- tidytable::as_tidytable(as.list(df_ranked))
  if (!"cluster_consensus_promoted_from_anchor" %in% names(df_out)) {
    data.table::set(
      df_out,
      j = "cluster_consensus_promoted_from_anchor",
      value = rep.int(FALSE, n)
    )
  }
  if (!"annotation_note" %in% names(df_out)) {
    data.table::set(
      df_out,
      j = "annotation_note",
      value = rep.int(NA_character_, n)
    )
  }

  col_prom <- as.logical(df_out$cluster_consensus_promoted_from_anchor)
  col_prom[is.na(col_prom)] <- FALSE
  col_note <- as.character(df_out$annotation_note)

  rows_by_fid <- split(seq_len(n), fids)
  reordered_fids <- character(0L)

  target_iks_by_cid <- opt$target_iks_by_cid
  optimal_cids_by_ik <- opt$optimal_cids_by_ik
  holder_fids_by_ik <- opt$holder_fids_by_ik

  act_fids <- unique(fids[!is.na(rt_cid) & rt_cid %in% opt$active_cids])
  for (lfid in act_fids) {
    f_rows <- rows_by_fid[[lfid]]
    cid <- rt_cid[f_rows[[1L]]]
    cid_chr <- as.character(cid)
    target_iks <- target_iks_by_cid[[cid_chr]]

    r1_f <- f_rows[col_rk[f_rows] == 1L & valid_ik[f_rows]]
    if (length(r1_f) == 0L) {
      next
    }
    r1_f_ord <- r1_f[order(row_prio_rank[r1_f])]
    orig_iks <- unique(iks[r1_f_ord])

    # Determine which candidates on this feature belong exclusively to other RT clusters
    f_iks <- iks[f_rows]
    taken_by_other <- vapply(
      seq_along(f_rows),
      function(idx) {
        if (!valid_ik[f_rows[[idx]]]) {
          return(FALSE)
        }
        cids_opt <- optimal_cids_by_ik[[f_iks[[idx]]]]
        length(cids_opt) > 0L && !(cid %in% cids_opt)
      },
      logical(1L)
    )

    is_target_row <- valid_ik[f_rows] &
      (f_iks %in% target_iks) &
      !taken_by_other
    if (!any(is_target_row) && any(valid_ik[f_rows] & !taken_by_other)) {
      avail_mask <- valid_ik[f_rows] & !taken_by_other
      min_avail_rk <- min(col_rk[f_rows[avail_mask]], na.rm = TRUE)
      is_target_row <- avail_mask & (col_rk[f_rows] == min_avail_rk)
    }

    if (!any(is_target_row)) {
      next
    }

    f_target_iks <- unique(f_iks[is_target_row])
    if (
      length(orig_iks) == length(f_target_iks) &&
        all(orig_iks %in% f_target_iks) &&
        !any(taken_by_other)
    ) {
      next
    }

    tier <- ifelse(is_target_row, 0L, ifelse(taken_by_other, 2L, 1L))
    ord_f <- order(
      tier,
      col_rk[f_rows],
      row_prio_rank[f_rows],
      method = "radix"
    )
    f_rows_s <- f_rows[ord_f]
    tier_s <- tier[ord_f]
    rk_key_part <- ifelse(tier_s == 0L, 1L, col_rk[f_rows_s])
    old_key <- paste(tier_s, rk_key_part, sep = ":")
    new_rk_f <- match(old_key, unique(old_key))
    col_rk[f_rows_s] <- new_rk_f

    r1_new <- f_rows_s[new_rk_f == 1L]
    new_top_iks <- unique(iks[r1_new])
    if (
      length(orig_iks) == length(new_top_iks) &&
        all(orig_iks %in% new_top_iks)
    ) {
      next
    }

    displaced_iks <- orig_iks[!(orig_iks %in% new_top_iks)]
    if (length(displaced_iks) > 0L) {
      displaced_ik <- displaced_iks[[1L]]
      holders <- holder_fids_by_ik[[displaced_ik]]
      other_holders <- holders[names(holders) != cid_chr]
      holder_fid <- if (length(other_holders) > 0L) {
        unname(other_holders[[1L]])
      } else if (length(holders) > 0L) {
        unname(holders[[1L]])
      } else {
        lfid
      }
      col_prom[f_rows] <- FALSE
      col_prom[r1_new] <- TRUE
      col_note[r1_new] <- paste0(
        "Promoted to rank 1: cross-RT duplicate InChIKey ",
        displaced_ik,
        " assigned to feature ",
        holder_fid
      )
      reordered_fids <- c(reordered_fids, lfid)
    } else {
      tied_ik <- orig_iks[[1L]]
      holders <- holder_fids_by_ik[[tied_ik]]
      other_holders <- holders[names(holders) != cid_chr]
      tied_fid <- if (length(other_holders) > 0L) {
        unname(other_holders[[1L]])
      } else {
        lfid
      }
      r1_promoted <- r1_new[!(iks[r1_new] %in% orig_iks)]
      if (length(r1_promoted) > 0L) {
        col_prom[f_rows] <- FALSE
        col_prom[r1_promoted] <- TRUE
        col_note[r1_promoted] <- paste0(
          "Promoted to rank 1: cross-RT duplicate InChIKey ",
          tied_ik,
          " tied with feature ",
          tied_fid
        )
        reordered_fids <- c(reordered_fids, lfid)
      }
    }
  }

  if (length(reordered_fids) == 0L) {
    return(df_ranked)
  }

  u_fids <- unique(fids)
  fid_id <- fastmatch::fmatch(fids, u_fids)
  final_ord <- order(fid_id, col_rk, row_prio_rank, method = "radix")

  data.table::set(df_out, j = "rank_final", value = col_rk)
  data.table::set(
    df_out,
    j = "cluster_consensus_promoted_from_anchor",
    value = col_prom
  )
  data.table::set(df_out, j = "annotation_note", value = col_note)

  log_info(
    "Disambiguated cross-RT duplicate rank-1 candidates across %d feature(s)",
    length(unique(reordered_fids))
  )

  if (is.unsorted(final_ord)) {
    tidytable::as_tidytable(lapply(as.list(df_out), `[`, final_ord))
  } else {
    df_out
  }
}

#' Drop cross-RT duplicate rank-1 candidates from filtered-tier results
#'
#' @description Applies globally optimal RT-cluster assignment on `df_filtered`
#'   so that when a losing feature at a different retention time had no
#'   alternative candidate in `resolve_cross_rt_duplicate_candidates` (or when
#'   `filter_high_evidence_only` removed its promoted alternative), any
#'   candidate assigned exclusively to another RT cluster is removed from the
#'   losing RT cluster in `df_filtered`. Exact ties are preserved without
#'   randomness. If a remaining candidate in `df_filtered` is promoted to rank 1
#'   as a result, its promotion comment is recorded on `annotation_note`.
#'
#' @param df_filtered Filtered-tier candidate table.
#' @param rt_tolerance Maximum RT difference (minutes) for co-eluting features.
#' @return `df_filtered` with cross-RT duplicate rank-1 candidates removed and
#'   any resulting rank-1 promotions annotated in `annotation_note`.
#' @keywords internal
drop_cross_rt_duplicate_final_candidates <- function(
  df_filtered,
  rt_tolerance = DEFAULT_HE_MAX_RT_ERROR_MIN
) {
  n <- nrow(df_filtered)
  if (n < 2L) {
    return(df_filtered)
  }
  req_cols <- c(
    "feature_id",
    "rank_final",
    "candidate_structure_inchikey_connectivity_layer"
  )
  if (!all(req_cols %in% names(df_filtered))) {
    return(df_filtered)
  }
  row_rt <- if ("rt" %in% names(df_filtered)) {
    suppressWarnings(.as_numeric_dedup(df_filtered$rt))
  } else if ("feature_rt" %in% names(df_filtered)) {
    suppressWarnings(.as_numeric_dedup(df_filtered$feature_rt))
  } else {
    return(df_filtered)
  }

  fids <- as.character(df_filtered$feature_id)
  rt_cid <- .compute_feature_rt_clusters(
    fids = fids,
    row_rt = row_rt,
    rt_tolerance = rt_tolerance
  )
  if (all(is.na(rt_cid))) {
    return(df_filtered)
  }

  iks <- as.character(
    df_filtered$candidate_structure_inchikey_connectivity_layer
  )
  valid_ik <- !is.na(iks) & nzchar(iks)
  col_rk <- as.integer(df_filtered$rank_final)

  pw <- .cross_rt_row_priority_and_weights(df_filtered)
  row_prio_rank <- pw$prio_rank
  row_weights <- pw$weights

  opt <- .solve_optimal_rt_cluster_assignment(
    fids = fids,
    rt_cid = rt_cid,
    iks = iks,
    valid_ik = valid_ik,
    col_rk = col_rk,
    row_prio_rank = row_prio_rank,
    row_weights = row_weights
  )
  if (is.null(opt)) {
    return(df_filtered)
  }

  target_iks_by_cid <- opt$target_iks_by_cid
  optimal_cids_by_ik <- opt$optimal_cids_by_ik
  holder_fids_by_ik <- opt$holder_fids_by_ik

  col_note <- if ("annotation_note" %in% names(df_filtered)) {
    as.character(df_filtered$annotation_note)
  } else {
    rep.int(NA_character_, n)
  }
  added_note <- FALSE

  keep_row <- !vapply(
    seq_len(n),
    function(r) {
      if (!valid_ik[[r]] || is.na(rt_cid[[r]])) {
        return(FALSE)
      }
      cids_opt <- optimal_cids_by_ik[[iks[[r]]]]
      length(cids_opt) > 0L && !(rt_cid[[r]] %in% cids_opt)
    },
    logical(1L)
  )

  act_fids <- unique(fids[!is.na(rt_cid) & rt_cid %in% opt$active_cids])
  for (lfid in act_fids) {
    f_all <- which(fids == lfid)
    r1_orig <- f_all[col_rk[f_all] == 1L & valid_ik[f_all]]
    orig_iks <- unique(iks[r1_orig])
    rem_f <- which(keep_row & fids == lfid)
    if (length(rem_f) == 0L) {
      next
    }
    cid_chr <- as.character(rt_cid[rem_f[[1L]]])
    target_iks <- target_iks_by_cid[[cid_chr]]
    is_target_rem <- valid_ik[rem_f] & (iks[rem_f] %in% target_iks)
    if (!any(is_target_rem) && any(valid_ik[rem_f])) {
      min_rem_rk <- min(col_rk[rem_f[valid_ik[rem_f]]], na.rm = TRUE)
      is_target_rem <- valid_ik[rem_f] & (col_rk[rem_f] == min_rem_rk)
    }
    tier <- ifelse(is_target_rem, 0L, 1L)
    ord_idx <- order(
      tier,
      col_rk[rem_f],
      row_prio_rank[rem_f],
      method = "radix"
    )
    ord_rem <- rem_f[ord_idx]
    tier_s <- tier[ord_idx]
    rk_key_part <- ifelse(tier_s == 0L, 1L, col_rk[ord_rem])
    old_key <- paste(tier_s, rk_key_part, sep = ":")
    new_rk <- match(old_key, unique(old_key))
    col_rk[ord_rem] <- new_rk

    r1_new <- ord_rem[new_rk == 1L]
    new_top_iks <- unique(iks[r1_new])
    if (
      length(orig_iks) > 0L &&
        !(length(orig_iks) == length(new_top_iks) &&
          all(orig_iks %in% new_top_iks))
    ) {
      displaced_iks <- orig_iks[!(orig_iks %in% new_top_iks)]
      if (length(displaced_iks) > 0L) {
        displaced_ik <- displaced_iks[[1L]]
        holders <- holder_fids_by_ik[[displaced_ik]]
        other_holders <- holders[names(holders) != cid_chr]
        holder_fid <- if (length(other_holders) > 0L) {
          unname(other_holders[[1L]])
        } else {
          NA_character_
        }
        if (!is.na(holder_fid) && nzchar(holder_fid)) {
          note_str <- paste0(
            "Promoted to rank 1: cross-RT duplicate InChIKey ",
            displaced_ik,
            " assigned to feature ",
            holder_fid
          )
          na_note <- is.na(col_note[r1_new]) | !nzchar(trimws(col_note[r1_new]))
          if (any(na_note)) {
            col_note[r1_new[na_note]] <- note_str
            added_note <- TRUE
          }
        }
      } else {
        tied_ik <- orig_iks[[1L]]
        holders <- holder_fids_by_ik[[tied_ik]]
        other_holders <- holders[names(holders) != cid_chr]
        tied_fid <- if (length(other_holders) > 0L) {
          unname(other_holders[[1L]])
        } else {
          NA_character_
        }
        r1_promoted <- r1_new[!(iks[r1_new] %in% orig_iks)]
        if (length(r1_promoted) > 0L && !is.na(tied_fid) && nzchar(tied_fid)) {
          note_str <- paste0(
            "Promoted to rank 1: cross-RT duplicate InChIKey ",
            tied_ik,
            " tied with feature ",
            tied_fid
          )
          na_note <- is.na(col_note[r1_promoted]) |
            !nzchar(trimws(col_note[r1_promoted]))
          if (any(na_note)) {
            col_note[r1_promoted[na_note]] <- note_str
            added_note <- TRUE
          }
        }
      }
    }
  }

  if (all(keep_row) && !added_note) {
    return(df_filtered)
  }

  keep_idx <- which(keep_row)
  out <- tidytable::as_tidytable(lapply(as.list(df_filtered), `[`, keep_idx))
  data.table::set(out, j = "rank_final", value = col_rk[keep_idx])
  if (added_note || "annotation_note" %in% names(out)) {
    data.table::set(out, j = "annotation_note", value = col_note[keep_idx])
  }
  if (!all(keep_row)) {
    log_info(
      "Filtered-tier cross-RT duplicate gate removed %d duplicate rank-1 row(s)",
      n - length(keep_idx)
    )
  }
  out
}
