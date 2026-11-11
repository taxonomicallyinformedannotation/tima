#' Compute intensity co-variance edges within RT windows
#'
#' For each feature, identify all m/z pairs within RT tolerance and compute
#' Pearson correlation of their intensities across samples. This discovers
#' edges based purely on intensity co-variance, independent of known adduct
#' rules. These edges enter the network alongside rule-based edges (adducts,
#' clusters, losses) and are validated by network consensus.
#'
#' @include logs_utils.R
#' @param features_table Long-format table with columns:
#'   feature_id, sample, mz, rt, and intensity columns
#' @param tolerance_rt RT tolerance in minutes for grouping features
#' @param correlation_p_threshold False discovery rate threshold (default
#'   0.05). P-values of all pairs tested are Benjamini-Hochberg adjusted; only
#'   pairs with correlation > 0 and adjusted p < threshold are retained.
#'
#' @return Data frame with columns:
#'   - feature_id: anchor feature
#'   - feature_id_dest: target feature
#'   - correlation: Pearson correlation coefficient (positive only)
#'   - p_value: Benjamini-Hochberg adjusted p-value
#'   - p_value_raw: Unadjusted p-value from the correlation test
#'   - n_samples: Number of sample pairs used
#'   - source: "intensity_covariance" (for consistency with other edge types)
#'
#' @keywords internal
#' @noRd
compute_intensity_covariance_edges <- function(
  features_table,
  tolerance_rt,
  correlation_p_threshold = 0.05,
  log_transform = TRUE,
  alternative = c("greater", "two.sided")
) {
  alternative <- match.arg(alternative)
  if (nrow(features_table) == 0L) {
    return(.empty_covariance_edges())
  }

  # Identify intensity columns: look for 'intensity_all' (unfiltered) or 'intensity' (filtered)
  has_intensity_all <- "intensity_all" %in% names(features_table)
  intensity_col <- if (has_intensity_all) "intensity_all" else "intensity"

  if (!(intensity_col %in% names(features_table))) {
    log_debug(
      "No %s column found in features_table. Skipping intensity co-variance edges.",
      intensity_col
    )
    return(.empty_covariance_edges())
  }

  log_debug(
    "Computing intensity co-variance edges across %d features using %s (RT tolerance: %.4f, p threshold: %.3f)",
    length(unique(features_table$feature_id)),
    intensity_col,
    tolerance_rt,
    correlation_p_threshold
  )

  # Get unique RT per feature and sort by RT for efficient windowing
  feature_rts <- features_table |>
    tidytable::select(feature_id, rt) |>
    tidytable::distinct() |>
    tidytable::filter(!is.na(rt)) |>
    tidytable::arrange(rt)

  if (nrow(feature_rts) < 2L) {
    log_info(
      "Intensity co-variance: tested 0 pair(s), 0 significant edge(s) retained (p < %.2f)",
      correlation_p_threshold
    )
    return(.empty_covariance_edges())
  }

  # Generate pairs efficiently using vectorized sorted-RT window lookup
  n_rts <- nrow(feature_rts)
  rts <- as.numeric(feature_rts$rt)
  feat_ids_rt <- feature_rts$feature_id
  i_seq <- seq_len(n_rts - 1L)
  end_idx <- findInterval(rts[i_seq] + tolerance_rt + 1e-12, rts)
  counts <- end_idx - i_seq
  active <- which(counts > 0L)

  if (length(active) == 0L) {
    log_info(
      "Intensity co-variance: tested 0 pair(s), 0 significant edge(s) retained (p < %.2f)",
      correlation_p_threshold
    )
    return(.empty_covariance_edges())
  }

  pair_r_i <- rep.int(active, counts[active])
  pair_r_j <- sequence(counts[active], from = active + 1L)
  keep_rt <- abs(rts[pair_r_j] - rts[pair_r_i]) <= tolerance_rt
  if (!all(keep_rt)) {
    pair_r_i <- pair_r_i[keep_rt]
    pair_r_j <- pair_r_j[keep_rt]
  }

  n_pairs_tested <- length(pair_r_i)
  if (n_pairs_tested == 0L) {
    log_info(
      "Intensity co-variance: tested 0 pair(s), 0 significant edge(s) retained (p < %.2f)",
      correlation_p_threshold
    )
    return(.empty_covariance_edges())
  }

  pair_feat_i <- feat_ids_rt[pair_r_i]
  pair_feat_j <- feat_ids_rt[pair_r_j]

  # Extract intensity_all lists - one per feature (they're duplicated per sample row)
  intensity_by_feature <- features_table |>
    tidytable::select(feature_id, !!as.name(intensity_col)) |>
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::filter(!is.na(.data[[intensity_col]]))

  if (nrow(intensity_by_feature) == 0L) {
    log_info(
      "Intensity co-variance: no features with intensity data. Skipping covariance edges."
    )
    return(.empty_covariance_edges())
  }

  # Convert to numeric vectors per feature; vectorize pipe-separated string parsing
  raw_intensities <- intensity_by_feature[[intensity_col]]
  intensity_vectors <- if (is.character(raw_intensities)) {
    lapply(
      strsplit(raw_intensities, "|", fixed = TRUE),
      function(s) suppressWarnings(as.numeric(s))
    )
  } else if (
    is.list(raw_intensities) &&
      all(vapply(raw_intensities, is.double, logical(1L)))
  ) {
    raw_intensities
  } else {
    lapply(
      raw_intensities,
      function(x) {
        if (is.character(x)) {
          suppressWarnings(as.numeric(strsplit(x, "|", fixed = TRUE)[[1L]]))
        } else if (is.list(x)) {
          as.numeric(unlist(x, use.names = FALSE))
        } else {
          as.numeric(x)
        }
      }
    )
  }

  log_debug(
    "Extracted intensity vectors for %d features",
    length(intensity_vectors)
  )

  # Map pair endpoints to feature indices and classify drops vectorially
  feat_ids_int <- intensity_by_feature$feature_id
  idx_i <- fastmatch::fmatch(pair_feat_i, feat_ids_int)
  idx_j <- fastmatch::fmatch(pair_feat_j, feat_ids_int)

  in_mat <- !is.na(idx_i) & !is.na(idx_j)
  n_not_in_matrix <- sum(!in_mat)

  vec_lens <- lengths(intensity_vectors)
  idx_in <- which(in_mat)
  same_len <- vec_lens[idx_i[idx_in]] == vec_lens[idx_j[idx_in]]
  n_length_mismatch <- sum(!same_len)
  idx_same <- idx_in[same_len]

  # Any feature with < 3 positive samples cannot form a pair with >= 3 shared samples
  pos_counts <- vapply(
    intensity_vectors,
    function(x) sum(!is.na(x) & x > 0),
    integer(1L)
  )
  has_min_pos <- pos_counts[idx_i[idx_same]] >= 3L &
    pos_counts[idx_j[idx_same]] >= 3L
  n_insufficient_samples <- sum(!has_min_pos)
  idx_cand <- idx_same[has_min_pos]

  n_correlation_failed <- 0L
  valid_test <- logical(n_pairs_tested)
  cor_all <- numeric(n_pairs_tested)
  n_samp_all <- integer(n_pairs_tested)

  if (length(idx_cand) > 0L) {
    cand_lens <- vec_lens[idx_i[idx_cand]]
    u_lens <- unique(cand_lens)
    len_results <- lapply(u_lens, function(s_len) {
      pair_sub <- if (length(u_lens) == 1L) {
        idx_cand
      } else {
        idx_cand[cand_lens == s_len]
      }
      u_feats <- sort(unique(c(idx_i[pair_sub], idx_j[pair_sub])))
      mat <- matrix(
        unlist(intensity_vectors[u_feats], use.names = FALSE),
        nrow = length(u_feats),
        ncol = s_len,
        byrow = TRUE
      )
      mask <- !is.na(mat) & mat > 0
      if (isTRUE(log_transform)) {
        mat[mask] <- log10(mat[mask])
      }
      mat[!mask] <- 0

      ci <- fastmatch::fmatch(idx_i[pair_sub], u_feats)
      cj <- fastmatch::fmatch(idx_j[pair_sub], u_feats)
      n_sub <- length(pair_sub)
      v <- mask[ci, , drop = FALSE] & mask[cj, , drop = FALSE]
      nv <- as.integer(.rowSums(v, n_sub, s_len))
      insuff <- nv < 3L
      n_insuff <- sum(insuff)
      ok <- which(!insuff)
      n_ok <- length(ok)
      n_bad <- 0L
      dest_idx <- integer(0L)
      cor_vals <- numeric(0L)
      samp_vals <- integer(0L)
      if (n_ok > 0L) {
        v_ok <- v[ok, , drop = FALSE]
        nv_ok <- nv[ok]
        x <- mat[ci[ok], , drop = FALSE] * v_ok
        y <- mat[cj[ok], , drop = FALSE] * v_ok
        mx <- .rowSums(x, n_ok, s_len) / nv_ok
        my <- .rowSums(y, n_ok, s_len) / nv_ok
        dx <- (x - mx) * v_ok
        dy <- (y - my) * v_ok
        ssx <- .rowSums(dx * dx, n_ok, s_len)
        ssy <- .rowSums(dy * dy, n_ok, s_len)
        denom <- sqrt(ssx * ssy)
        r_val <- .rowSums(dx * dy, n_ok, s_len) / denom
        bad_r <- is.na(r_val) |
          !is.finite(r_val) |
          !is.finite(denom) |
          denom <= 0
        n_bad <- sum(bad_r)
        good <- which(!bad_r)
        if (length(good) > 0L) {
          dest_idx <- pair_sub[ok[good]]
          cor_vals <- pmax(-1, pmin(1, r_val[good]))
          samp_vals <- nv_ok[good]
        }
      }
      list(
        n_insuff = n_insuff,
        n_bad = n_bad,
        dest_idx = dest_idx,
        cor_vals = cor_vals,
        samp_vals = samp_vals
      )
    })

    n_insufficient_samples <- n_insufficient_samples +
      sum(vapply(len_results, `[[`, integer(1L), "n_insuff"))
    n_correlation_failed <- sum(vapply(len_results, `[[`, integer(1L), "n_bad"))
    all_dest <- unlist(lapply(len_results, `[[`, "dest_idx"), use.names = FALSE)
    if (length(all_dest) > 0L) {
      cor_all[all_dest] <- unlist(
        lapply(len_results, `[[`, "cor_vals"),
        use.names = FALSE
      )
      n_samp_all[all_dest] <- unlist(
        lapply(len_results, `[[`, "samp_vals"),
        use.names = FALSE
      )
      valid_test[all_dest] <- TRUE
    }
  }

  keep <- which(valid_test)
  n_tests <- length(keep)

  result <- if (n_tests == 0L) {
    .empty_covariance_edges()
  } else {
    r_kept <- cor_all[keep]
    n_kept <- n_samp_all[keep]
    df <- n_kept - 2L
    t_stat <- sqrt(df) * r_kept / sqrt(pmax(0, 1 - r_kept * r_kept))
    p_raw_kept <- if (identical(alternative, "greater")) {
      stats::pt(-t_stat, df = df)
    } else {
      2 * stats::pt(-abs(t_stat), df = df)
    }
    tidytable::tidytable(
      feature_id = pair_feat_i[keep],
      feature_id_dest = pair_feat_j[keep],
      correlation = r_kept,
      p_value_raw = p_raw_kept,
      n_samples = n_kept
    ) |>
      # Benjamini-Hochberg over every test performed (positive or not), so the
      # threshold controls the false discovery rate of the retained edges
      tidytable::mutate(
        p_value = stats::p.adjust(.data$p_value_raw, method = "BH")
      )
  }

  n_non_positive <- sum(result$correlation <= 0)
  n_not_significant <- sum(
    result$correlation > 0 & result$p_value >= correlation_p_threshold
  )
  result <- result |>
    tidytable::filter(
      correlation > 0 & p_value < correlation_p_threshold
    ) |>
    tidytable::mutate(source = "intensity_covariance") |>
    tidytable::select(tidyselect::all_of(c(
      "feature_id",
      "feature_id_dest",
      "correlation",
      "p_value",
      "p_value_raw",
      "n_samples",
      "source"
    )))

  r_summary <- if (nrow(result) > 0L) {
    sprintf(
      "; r in [%.2f, %.2f], median r = %.2f",
      min(result$correlation),
      max(result$correlation),
      stats::median(result$correlation)
    )
  } else {
    ""
  }
  log_info(
    "Intensity co-variance: tested %d of %d pair(s), retained %d edge(s) (BH-adjusted p < %.2f)%s. Dropped: %d not in matrix, %d sample-count mismatch, %d insufficient samples, %d correlation failed, %d non-positive, %d adjusted p >= %.2f",
    n_tests,
    n_pairs_tested,
    nrow(result),
    correlation_p_threshold,
    r_summary,
    n_not_in_matrix,
    n_length_mismatch,
    n_insufficient_samples,
    n_correlation_failed,
    n_non_positive,
    n_not_significant,
    correlation_p_threshold
  )

  result
}

#' Empty covariance edges table with the output column types
#' @keywords internal
#' @noRd
.empty_covariance_edges <- function() {
  tidytable::tidytable(
    feature_id = character(),
    feature_id_dest = character(),
    correlation = numeric(),
    p_value = numeric(),
    p_value_raw = numeric(),
    n_samples = integer(),
    source = character()
  )
}
