#' Compute intensity co-variance edges within RT windows
#'
#' For each feature, identify all m/z pairs within RT tolerance and compute
#' Pearson correlation of their intensities across samples. This discovers
#' edges based purely on intensity co-variance, independent of known adduct
#' rules. These edges enter the network alongside rule-based edges (adducts,
#' clusters, losses) and are validated by network consensus.
#'
#' @param features_table Long-format table with columns:
#'   feature_id, sample, mz, rt, and intensity columns
#' @param tolerance_rt RT tolerance in minutes for grouping features
#' @param correlation_p_threshold P-value threshold for Pearson correlation
#'   (default 0.05). Only correlations with p < threshold are retained.
#'
#' @return Data frame with columns:
#'   - feature_id: anchor feature
#'   - feature_id_dest: target feature
#'   - correlation: Pearson correlation coefficient
#'   - p_value: P-value from correlation test
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
    return(tidytable::tidytable(
      feature_id = character(),
      feature_id_dest = character(),
      correlation = numeric(),
      p_value = numeric(),
      n_samples = integer(),
      source = character()
    ))
  }

  # Identify intensity columns: look for 'intensity_all' (unfiltered) or 'intensity' (filtered)
  has_intensity_all <- "intensity_all" %in% names(features_table)
  intensity_col <- if (has_intensity_all) "intensity_all" else "intensity"

  if (!(intensity_col %in% names(features_table))) {
    log_debug(
      "No %s column found in features_table. Skipping intensity co-variance edges.",
      intensity_col
    )
    return(tidytable::tidytable(
      feature_id = character(),
      feature_id_dest = character(),
      correlation = numeric(),
      p_value = numeric(),
      n_samples = integer(),
      source = character()
    ))
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
    return(tidytable::tidytable(
      feature_id = character(),
      feature_id_dest = character(),
      correlation = numeric(),
      p_value = numeric(),
      n_samples = integer(),
      source = character()
    ))
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
    return(tidytable::tidytable(
      feature_id = character(),
      feature_id_dest = character(),
      correlation = numeric(),
      p_value = numeric(),
      n_samples = integer(),
      source = character()
    ))
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
    return(tidytable::tidytable(
      feature_id = character(),
      feature_id_dest = character(),
      correlation = numeric(),
      p_value = numeric(),
      n_samples = integer(),
      source = character()
    ))
  }

  # Convert to a named list for fast lookups
  # If intensities are stored as pipe-separated strings (from CSV), parse them
  intensity_vectors <- lapply(
    intensity_by_feature[[intensity_col]],
    function(x) {
      if (is.character(x)) {
        # Parse pipe-separated string to numeric
        result <- as.numeric(strsplit(x, "\\|")[[1]])
        result[!is.na(result)] # Keep NAs but filter NaN
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

  # Apply correlation computation to all pairs with diagnostic tracking
  n_not_in_matrix <- 0L
  n_insufficient_samples <- 0L
  n_correlation_failed <- 0L
  n_not_significant <- 0L

  compute_pair_correlations <- function(pair_row) {
    feat_id <- pair_row$feature_id[[1L]]
    other_id <- pair_row$feature_id_dest[[1L]]

    # Get intensity vectors
    if (
      !(feat_id %in% names(intensity_vectors)) ||
        !(other_id %in% names(intensity_vectors))
    ) {
      n_not_in_matrix <<- n_not_in_matrix + 1L
      return(NULL)
    }

    int_x <- intensity_vectors[[feat_id]]
    int_y <- intensity_vectors[[other_id]]

    # Both vectors must have same length (same sample ordering)
    if (length(int_x) != length(int_y)) {
      n_insufficient_samples <<- n_insufficient_samples + 1L
      return(NULL)
    }

    # Filter out NAs and non-positive values
    valid_idx <- !is.na(int_x) & !is.na(int_y) & int_x > 0 & int_y > 0

    # Need at least 3 data points for a meaningful correlation test (df >= 1)
    n_valid <- sum(valid_idx)
    if (n_valid < 3L) {
      n_insufficient_samples <<- n_insufficient_samples + 1L
      return(NULL)
    }

    x_valid <- int_x[valid_idx]
    y_valid <- int_y[valid_idx]

    # Compute Pearson correlation and p-value
    cor_result <- tryCatch(
      {
        cor_test <- stats::cor.test(x_valid, y_valid, method = "pearson")
        list(
          correlation = as.numeric(cor_test$estimate),
          p_value = as.numeric(cor_test$p.value),
          n_samples = n_valid
        )
      },
      error = function(e) {
        NULL
      }
    )

    if (is.null(cor_result) || is.na(cor_result$p_value)) {
      n_correlation_failed <<- n_correlation_failed + 1L
      return(NULL)
    }

    if (cor_result$p_value < correlation_p_threshold) {
      tidytable::tidytable(
        feature_id = feat_id,
        feature_id_dest = other_id,
        correlation = cor_result$correlation,
        p_value = cor_result$p_value,
        n_samples = cor_result$n_samples
      )
    } else {
      n_not_significant <<- n_not_significant + 1L
      NULL
    }
  }

  # Apply correlation computation to all pairs
  results_list <- lapply(
    seq_len(nrow(feature_pairs)),
    function(i) {
      compute_pair_correlations(feature_pairs[i, ])
    }
  )

  # Filter NULLs and bind results
  results_list <- Filter(Negate(is.null), results_list)
  n_pairs_tested <- nrow(feature_pairs)
  n_significant <- length(results_list)

  if (length(results_list) == 0L) {
    log_info(
      "Intensity co-variance: tested %d pair(s), %d significant edge(s) retained (p < %.2f). Dropped: %d not in matrix, %d insufficient samples, %d correlation failed, %d p >= %.2f",
      n_pairs_tested,
      n_significant,
      correlation_p_threshold,
      n_not_in_matrix,
      n_insufficient_samples,
      n_correlation_failed,
      n_not_significant,
      correlation_p_threshold
    )
    return(tidytable::tidytable(
      feature_id = character(),
      feature_id_dest = character(),
      correlation = numeric(),
      p_value = numeric(),
      n_samples = integer(),
      source = character()
    ))
  }

  result <- tidytable::bind_rows(results_list) |>
    tidytable::mutate(source = "intensity_covariance")

  log_info(
    "Intensity co-variance: tested %d pair(s), retained %d significant edge(s) (p < %.2f); r in [%.2f, %.2f], median r = %.2f. Dropped: %d not in matrix, %d insufficient samples, %d correlation failed, %d p >= %.2f",
    n_pairs_tested,
    nrow(result),
    correlation_p_threshold,
    min(result$correlation, na.rm = TRUE),
    max(result$correlation, na.rm = TRUE),
    stats::median(result$correlation, na.rm = TRUE),
    n_not_in_matrix,
    n_insufficient_samples,
    n_correlation_failed,
    n_not_significant,
    correlation_p_threshold
  )

  result
}
