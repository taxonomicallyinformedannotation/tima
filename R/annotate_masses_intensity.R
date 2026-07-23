#' Validate adduct edges by intensity co-variance across samples
#'
#' @description For each potential adduct pair, calculates the Pearson correlation
#'     of feature intensities across all samples. Edges where the two features show
#'     poor intensity co-variance are rejected as true adducts of the same neutral mass.
#'
#' @param adduct_edges Data frame with columns: feature_id, adduct, adduct_dest, feature_id_dest
#' @param features_table Data frame with columns: feature_id, sample, and sample intensity columns
#' @param min_correlation Minimum Pearson correlation required to retain an edge.
#'     Default 0.7. Edges with |correlation| < min_correlation are rejected.
#'
#' @return List with two elements:
#'   - `adduct_edges`: Filtered adduct_edges (low-covariance edges removed)
#'   - `rejected_edges`: Data frame of rejected edges with correlation values and rejection reason
#'
#' @keywords internal
validate_adduct_edges_by_intensity_covariance <- function(
  adduct_edges,
  features_table,
  min_correlation = 0.7
) {
  if (nrow(adduct_edges) == 0L || nrow(features_table) == 0L) {
    return(list(
      adduct_edges = adduct_edges,
      rejected_edges = tidytable::tidytable(
        feature_id = character(),
        adduct = character(),
        adduct_dest = character(),
        feature_id_dest = character(),
        intensity_correlation = numeric(),
        rejection_reason = character()
      )
    ))
  }

  # Identify intensity columns (all columns except feature_id, sample, and core metadata)
  core_cols <- c("feature_id", "sample", "mz", "rt", "adduct")
  intensity_cols <- setdiff(colnames(features_table), core_cols)

  if (length(intensity_cols) == 0L) {
    # No intensity data available - keep all edges
    return(list(
      adduct_edges = adduct_edges,
      rejected_edges = tidytable::tidytable(
        feature_id = character(),
        adduct = character(),
        adduct_dest = character(),
        feature_id_dest = character(),
        intensity_correlation = numeric(),
        rejection_reason = character()
      )
    ))
  }

  # Get unique feature pairs to check
  edge_pairs <- adduct_edges |>
    tidytable::distinct(feature_id, feature_id_dest)

  if (nrow(edge_pairs) == 0L) {
    return(list(
      adduct_edges = adduct_edges,
      rejected_edges = tidytable::tidytable(
        feature_id = character(),
        adduct = character(),
        adduct_dest = character(),
        feature_id_dest = character(),
        intensity_correlation = numeric(),
        rejection_reason = character()
      )
    ))
  }

  # Calculate correlations for each pair
  correlations <- lapply(
    seq_len(nrow(edge_pairs)),
    function(i) {
      fid1 <- edge_pairs$feature_id[i]
      fid2 <- edge_pairs$feature_id_dest[i]

      # Get data for both features
      f1_data <- features_table |>
        tidytable::filter(feature_id == fid1) |>
        tidytable::arrange(sample)

      f2_data <- features_table |>
        tidytable::filter(feature_id == fid2) |>
        tidytable::arrange(sample)

      # Extract intensity values as vectors
      int1 <- as.numeric(unlist(
        f1_data |> tidytable::select(tidyselect::all_of(intensity_cols))
      ))
      int2 <- as.numeric(unlist(
        f2_data |> tidytable::select(tidyselect::all_of(intensity_cols))
      ))

      # Calculate correlation
      if (
        length(int1) > 1 && length(int2) > 1 && length(int1) == length(int2)
      ) {
        # Remove NAs and positions where either value is 0 or NA
        valid_idx <- !is.na(int1) & !is.na(int2) & int1 > 0 & int2 > 0
        if (sum(valid_idx) > 1) {
          cor_val <- stats::cor(
            int1[valid_idx],
            int2[valid_idx],
            use = "complete.obs"
          )
        } else {
          cor_val <- NA_real_
        }
      } else {
        cor_val <- NA_real_
      }

      data.frame(
        feature_id = fid1,
        feature_id_dest = fid2,
        intensity_correlation = cor_val,
        stringsAsFactors = FALSE
      )
    }
  )
  correlations_df <- do.call(rbind, correlations)
  correlations_df <- tidytable::as_tidytable(correlations_df)

  # Join correlations back to edges
  adduct_edges_with_cor <- adduct_edges |>
    tidytable::left_join(
      correlations_df,
      by = c("feature_id", "feature_id_dest")
    )

  # Identify low-covariance edges (including NA correlations)
  low_cov_mask <- is.na(adduct_edges_with_cor$intensity_correlation) |
    abs(adduct_edges_with_cor$intensity_correlation) < min_correlation

  rejected_edges <- adduct_edges_with_cor |>
    tidytable::filter(low_cov_mask) |>
    tidytable::mutate(
      rejection_reason = tidytable::if_else(
        is.na(intensity_correlation),
        "Insufficient intensity data for co-variance calculation",
        paste0(
          "Low intensity co-variance (r=",
          round(intensity_correlation, 3),
          ")"
        )
      )
    ) |>
    tidytable::select(
      feature_id,
      adduct,
      adduct_dest,
      feature_id_dest,
      intensity_correlation,
      rejection_reason
    )

  # Keep only high-covariance edges
  adduct_edges_filtered <- adduct_edges_with_cor |>
    tidytable::filter(!low_cov_mask) |>
    tidytable::select(-tidyselect::any_of(c("intensity_correlation")))

  list(
    adduct_edges = adduct_edges_filtered,
    rejected_edges = rejected_edges
  )
}
