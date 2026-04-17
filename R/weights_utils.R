#' Compute confidence-aware weighted sum (row-wise for data frames)
#'
#' @param ... [numeric] Numeric columns/vectors of component values (passed as
#'     separate arguments).
#' @param weights [numeric] Numeric vector of non-negative weights (one per
#'     component).
#' @param coverage_floor [numeric] Minimum coverage multiplier (0-1). Controls
#'     how much missing evidence dimensions penalize the score. Default 0.5
#'     means a single-dimension score is discounted to at most 50-100% of
#'     its value depending on how many dimensions are missing. Set to 1.0
#'     to disable the coverage discount (pure renormalization).
#' @return Numeric vector: row-wise weighted sum with per-row weight
#'     renormalization and evidence coverage discount.
#'     Returns NA for rows where all components are NA.
#' @keywords internal
#'
#' @details
#' Two-step computation:
#' 1. **Renormalized weighted average**: weights are rescaled per row over
#'    non-NA components so missing data doesn't contribute zero.
#' 2. **Coverage discount**: the average is multiplied by a factor reflecting
#'    what fraction of total weight was available. With `coverage_floor = 0.5`,
#'    a row with all components gets factor 1.0, while a row with only 1/3 of
#'    total weight available gets factor ~0.67.
#'
#' Formula: `result = weighted_avg × (floor + (1 - floor) × coverage)`
#' where `coverage = sum(available_weights) / sum(all_weights)`.
compute_weighted_sum <- function(..., weights, coverage_floor = 0.5) {
  values <- list(...)

  # Input validation
  if (length(values) != length(weights)) {
    cli::cli_abort(
      c(
        "number of value vectors must equal number of weights",
        "x" = paste0("values=", length(values), "; weights=", length(weights))
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (
    !all(vapply(
      X = values,
      FUN = is.numeric,
      logical(1L)
    ))
  ) {
    cli::cli_abort(
      "all value vectors must be numeric",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!is.numeric(weights) || any(weights < 0)) {
    cli::cli_abort(
      "weights must be numeric and non-negative",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  weight_sum <- sum(weights)
  if (weight_sum <= 0) {
    return(rep(NA_real_, length(values[[1L]])))
  }

  # Convert list to matrix for efficient row-wise operations
  values_matrix <- do.call(cbind, values)
  weights_vec <- weights

  # Per-row renormalization: only non-NA components contribute
  available <- !is.na(values_matrix)
  # Weight matrix: broadcast weights to each row, zero out NA positions
  weight_matrix <- t(replicate(nrow(values_matrix), weights_vec)) * available
  row_weight_sums <- rowSums(weight_matrix)

  # Compute renormalized weighted average
  values_safe <- values_matrix
  values_safe[is.na(values_safe)] <- 0
  weighted_avg <- rowSums(values_safe * weight_matrix) / row_weight_sums

  # Evidence coverage discount: penalize when fewer dimensions are available
  coverage <- row_weight_sums / weight_sum
  coverage_factor <- coverage_floor + (1 - coverage_floor) * coverage

  result <- weighted_avg * coverage_factor
  # Rows where all components are NA get NA (not NaN from 0/0)
  result[row_weight_sums == 0] <- NA_real_

  as.numeric(result)
}
