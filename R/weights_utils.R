#' Compute normalized weighted sum (row-wise for data frames)
#'
#' @param ... Numeric columns/vectors of component values (passed as separate arguments).
#' @param weights Numeric vector of non-negative weights (one per component).
#' @return Numeric vector: row-wise weighted sum with normalized weights.
#' @keywords internal
#'
#' @details Original formula: (w1*v1 + w2*v2 + ...) / (w1 + w2 + ...)
#' where w are weights and v are value columns. Weights are normalized to sum to 1.
compute_weighted_sum <- function(..., weights) {
  values <- list(...)

  # Input validation
  if (length(values) != length(weights)) {
    stop(
      "Number of value vectors (",
      length(values),
      ") must equal number of weights (",
      length(weights),
      ")",
      call. = FALSE
    )
  }

  if (!all(vapply(values, is.numeric, logical(1L)))) {
    stop("All value vectors must be numeric", call. = FALSE)
  }

  if (!is.numeric(weights) || any(weights < 0)) {
    stop("Weights must be numeric and non-negative", call. = FALSE)
  }

  weight_sum <- sum(weights)
  if (weight_sum <= 0) {
    return(rep(0, length(values[[1L]])))
  }

  # Normalize weights to sum to 1
  norm_weights <- weights / weight_sum

  # Vectorized computation using weighted matrix multiplication
  # Convert list to matrix for efficient row-wise operations
  values_matrix <- do.call(cbind, values)

  # Matrix multiplication: each row multiplied by weights
  as.numeric(values_matrix %*% norm_weights)
}
