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
  correlation_p_threshold = 0.05
) {
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

  # Generate pairs efficiently using vectorized window lookup
  feature_pairs_list <- lapply(
    seq_len(nrow(feature_rts) - 1L),
    function(i) {
      feat_id <- feature_rts$feature_id[[i]]
      feat_rt <- feature_rts$rt[[i]]

      # Find all features j > i within RT window (only check features after this one)
      # Features are sorted by RT, so we can stop early
      neighbors <- tidytable::filter(
        feature_rts[(i + 1L):nrow(feature_rts), ], # Only look ahead
        abs(rt - feat_rt) <= tolerance_rt
      )

      if (nrow(neighbors) == 0L) {
        return(tidytable::tidytable(
          feature_id = character(),
          feature_id_dest = character()
        ))
      }

      # Create pairs
      tidytable::tidytable(
        feature_id = feat_id,
        feature_id_dest = neighbors$feature_id
      )
    }
  )

  # Bind all pairs (much smaller than cross_join)
  feature_pairs <- tidytable::bind_rows(feature_pairs_list)

  if (nrow(feature_pairs) == 0L) {
    log_info(
      "Intensity co-variance: tested 0 pair(s), 0 significant edge(s) retained (p < %.2f)",
      correlation_p_threshold
    )
    return(.empty_covariance_edges())
  }

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

  # Convert to a named list for fast lookups
  # If intensities are stored as pipe-separated strings (from CSV), parse them
  intensity_vectors <- lapply(
    intensity_by_feature[[intensity_col]],
    function(x) {
      if (is.character(x)) {
        # Parse pipe-separated string to numeric. Keep NAs so that positions
        # stay aligned with samples across features (filtered pairwise below)
        suppressWarnings(as.numeric(strsplit(x, "\\|")[[1]]))
      } else if (is.list(x)) {
        as.numeric(unlist(x))
      } else {
        as.numeric(x) # Convert to numeric
      }
    }
  )
  intensity_vectors <- stats::setNames(
    intensity_vectors,
    intensity_by_feature$feature_id
  )

  log_debug(
    "Extracted intensity vectors for %d features",
    length(intensity_vectors)
  )

  # Test every pair, then correct the p-values for multiple testing.
  # Counters live in an environment so the per-pair closure can update them
  drops <- new.env(parent = emptyenv())
  drops$n_not_in_matrix <- 0L
  drops$n_length_mismatch <- 0L
  drops$n_insufficient_samples <- 0L
  drops$n_correlation_failed <- 0L

  test_pair <- function(feat_id, other_id) {
    int_x <- intensity_vectors[[feat_id]]
    int_y <- intensity_vectors[[other_id]]
    if (is.null(int_x) || is.null(int_y)) {
      drops$n_not_in_matrix <- drops$n_not_in_matrix + 1L
      return(NULL)
    }

    # Both vectors must have same length (same sample ordering)
    if (length(int_x) != length(int_y)) {
      drops$n_length_mismatch <- drops$n_length_mismatch + 1L
      return(NULL)
    }

    # Filter out NAs and non-positive values
    valid_idx <- !is.na(int_x) & !is.na(int_y) & int_x > 0 & int_y > 0

    # Need at least 3 data points for a meaningful correlation test (df >= 1)
    n_valid <- sum(valid_idx)
    if (n_valid < 3L) {
      drops$n_insufficient_samples <- drops$n_insufficient_samples + 1L
      return(NULL)
    }

    cor_test <- tryCatch(
      suppressWarnings(stats::cor.test(
        int_x[valid_idx],
        int_y[valid_idx],
        method = "pearson"
      )),
      error = function(e) NULL
    )
    if (is.null(cor_test) || is.na(cor_test$p.value)) {
      drops$n_correlation_failed <- drops$n_correlation_failed + 1L
      return(NULL)
    }

    c(
      correlation = as.numeric(cor_test$estimate),
      p_value_raw = as.numeric(cor_test$p.value),
      n_samples = n_valid
    )
  }

  tested <- Map(
    test_pair,
    feature_pairs$feature_id,
    feature_pairs$feature_id_dest
  )
  keep <- !vapply(tested, is.null, logical(1L))
  stats_mat <- do.call(rbind, tested[keep])

  n_pairs_tested <- nrow(feature_pairs)
  n_tests <- sum(keep)
  n_not_in_matrix <- drops$n_not_in_matrix
  n_length_mismatch <- drops$n_length_mismatch
  n_insufficient_samples <- drops$n_insufficient_samples
  n_correlation_failed <- drops$n_correlation_failed
  rm(drops)

  result <- if (n_tests == 0L) {
    .empty_covariance_edges()
  } else {
    tidytable::tidytable(
      feature_id = feature_pairs$feature_id[keep],
      feature_id_dest = feature_pairs$feature_id_dest[keep],
      correlation = stats_mat[, "correlation"],
      p_value_raw = stats_mat[, "p_value_raw"],
      n_samples = as.integer(stats_mat[, "n_samples"])
    ) |>
      # Benjamini-Hochberg over every test performed (positive or not), so the
      # threshold controls the false discovery rate of the retained edges
      tidytable::mutate(
        p_value = stats::p.adjust(p_value_raw, method = "BH")
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
    tidytable::select(
      feature_id,
      feature_id_dest,
      correlation,
      p_value,
      p_value_raw,
      n_samples,
      source
    )

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
