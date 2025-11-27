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
  stopifnot(length(values) == length(weights))
  stopifnot(all(sapply(values, is.numeric)))
  stopifnot(is.numeric(weights), all(weights >= 0))

  weight_sum <- sum(weights)
  if (weight_sum <= 0) {
    return(rep(0, length(values[[1]])))
  }

  # Normalize weights to sum to 1
  norm_weights <- weights / weight_sum

  # Compute row-wise weighted sum
  result <- rep(0, length(values[[1]]))
  for (i in seq_along(values)) {
    result <- result + norm_weights[i] * values[[i]]
  }

  as.numeric(result)
}
