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

  # Identify intensity columns (all except feature_id, sample, mz, rt)
  exclude_cols <- c("feature_id", "sample", "mz", "rt")
  intensity_cols <- setdiff(names(features_table), exclude_cols)

  if (length(intensity_cols) == 0L) {
    log_debug(
      "No intensity columns found in features_table. Skipping intensity co-variance edges."
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

  # Group by feature and compute RT windows
  covariance_edges <- list()

  for (feat in unique(features_table$feature_id)) {
    feat_data <- tidytable::filter(features_table, feature_id == feat)

    # For each sample, find all features within RT window
    for (samp in unique(feat_data$sample)) {
      samp_data <- tidytable::filter(feat_data, sample == samp)
      if (nrow(samp_data) == 0L) next

      feat_rt <- samp_data$rt[[1L]]
      if (is.na(feat_rt)) next

      # Find all features in same sample within RT window
      all_in_sample <- tidytable::filter(
        features_table,
        sample == samp,
        !is.na(rt),
        abs(rt - feat_rt) <= tolerance_rt
      )

      # For each other feature in the window
      for (other_feat in unique(all_in_sample$feature_id)) {
        if (other_feat <= feat) next # Avoid duplicates and self-loops

        # Collect intensity pairs across all samples for both features
        feat_intensities <- tidytable::filter(features_table, feature_id == feat)
        other_intensities <- tidytable::filter(features_table, feature_id == other_feat)

        # Join on sample
        combined <- tidytable::inner_join(
          feat_intensities,
          other_intensities,
          by = "sample",
          suffix = c("", "_other")
        )

        if (nrow(combined) < 2L) next # Need at least 2 samples for correlation

        # Extract intensity vectors and compute correlation for each intensity col
        for (int_col in intensity_cols) {
          if (!(int_col %in% names(combined))) next

          int_x <- combined[[int_col]]
          int_y <- combined[[paste0(int_col, "_other")]]

          # Filter out NAs and non-positive values
          valid_idx <- !is.na(int_x) & !is.na(int_y) &
            int_x > 0 & int_y > 0

          if (sum(valid_idx) < 2L) next # Need at least 2 valid pairs

          x_valid <- int_x[valid_idx]
          y_valid <- int_y[valid_idx]

          # Compute Pearson correlation and p-value
          cor_result <- tryCatch(
            {
              cor_test <- stats::cor.test(x_valid, y_valid, method = "pearson")
              list(
                correlation = as.numeric(cor_test$estimate),
                p_value = as.numeric(cor_test$p.value)
              )
            },
            error = function(e) {
              list(correlation = NA_real_, p_value = NA_real_)
            }
          )

          # If p-value significant, record edge
          if (!is.na(cor_result$p_value) &&
            cor_result$p_value < correlation_p_threshold) {
            edge_key <- paste(feat, other_feat, sep = "|")
            if (!(edge_key %in% names(covariance_edges))) {
              covariance_edges[[edge_key]] <- list(
                feature_id = feat,
                feature_id_dest = other_feat,
                correlation = cor_result$correlation,
                p_value = cor_result$p_value,
                n_samples = sum(valid_idx)
              )
            }
          }
        }
      }
    }
  }

  # Convert list to data frame
  if (length(covariance_edges) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      feature_id_dest = character(),
      correlation = numeric(),
      p_value = numeric(),
      n_samples = integer(),
      source = character()
    ))
  }

  result <- tidytable::as_tidytable(do.call(rbind, lapply(
    covariance_edges,
    function(edge) {
      tidytable::tidytable(
        feature_id = edge$feature_id,
        feature_id_dest = edge$feature_id_dest,
        correlation = edge$correlation,
        p_value = edge$p_value,
        n_samples = edge$n_samples,
        source = "intensity_covariance"
      )
    }
  )))

  result
}
