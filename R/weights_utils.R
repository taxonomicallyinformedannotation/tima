#' Compute evidence-aware weighted sum (row-wise for data frames)
#'
#' @param ... [numeric] Numeric columns/vectors of component values (passed as
#'     separate arguments).
#' @param weights [numeric] Numeric vector of non-negative weights (one per
#'     component).
#' @return List with `score` (weighted average over present evidence) and
#'     `coverage` (fraction of total weight supported by non-missing evidence).
#'     Rows where all components are NA return NA.
#' @keywords internal
compute_weighted_components <- function(..., weights) {
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
    n <- length(values[[1L]])
    return(list(
      score = rep(NA_real_, n),
      coverage = rep(NA_real_, n)
    ))
  }

  values_matrix <- do.call(cbind, values)
  values_matrix <- as.matrix(values_matrix)

  # Convert NA to 0 before calculation
  values_matrix[is.na(values_matrix)] <- 0

  # Simple weighted sum divided by total weight
  score <- as.numeric(values_matrix %*% weights)
  score <- score / weight_sum

  coverage <- NA_real_ # Not used anymore

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
  compute_weighted_components(..., weights = weights)$score
}
