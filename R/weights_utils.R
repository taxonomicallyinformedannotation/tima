#' Compute evidence-aware weighted sum (row-wise for data frames)
#'
#' @param ... [numeric] Numeric columns/vectors of component values (passed as
#'     separate arguments).
#' @param weights [numeric] Numeric vector of non-negative weights (one per
#'     component).
#' @return List with `score` (weighted average over all components, treating
#'     missing values as 0) and `coverage` (unused; returned as `NA`).
#'     Rows where all weights are zero return NA.
#' @keywords internal
compute_weighted_components <- function(..., weights, include_coverage = TRUE) {
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

  lens <- lengths(values)
  n <- if (length(lens) > 0L) max(lens) else 0L
  weight_sum <- sum(weights)
  if (weight_sum <= 0 || n == 0L) {
    return(list(
      score = rep(NA_real_, n),
      coverage = if (include_coverage) rep(NA_real_, n) else NULL
    ))
  }

  # Accumulate weighted sum directly without allocating an N x K matrix
  score <- numeric(n)
  for (k in seq_along(values)) {
    w <- weights[[k]]
    if (w != 0) {
      v <- values[[k]]
      if (length(v) != n) {
        v <- rep_len(v, n)
      }
      if (anyNA(v)) {
        v[is.na(v)] <- 0
      }
      score <- score + v * w
    }
  }
  score <- score / weight_sum
  coverage <- if (include_coverage) rep(NA_real_, n) else NULL

  list(score = score, coverage = coverage)
}

#' Compute weighted scores from evidence components
#'
#' @description Convenience wrapper around [compute_weighted_components()] that
#'   returns only the weighted score vector.
#'
#' @param ... [numeric] Numeric columns/vectors of component values (passed as
#'   separate arguments).
#' @param weights [numeric] Numeric vector of non-negative weights (one per
#'   component).
#'
#' @return Numeric vector of weighted scores, or NA for rows with no evidence.
#' @keywords internal
compute_weighted_sum <- function(..., weights) {
  compute_weighted_components(
    ...,
    weights = weights,
    include_coverage = FALSE
  )$score
}
