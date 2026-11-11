#' Compute evidence-aware weighted sum (row-wise for data frames)
#'
#' @param ... [numeric] Columns/vectors of component values (passed as
#'     separate arguments).
#' @param weights [numeric] Vector of non-negative weights (one per
#'     component).
#' @param include_coverage [logical] Whether to compute the per-row weight
#'     coverage fraction in `[0, 1]`.
#' @param na_prior [numeric] Prior value for missing (`NA`)
#'     modalities: numeric scalar/vector in `[0, 1]` (default `0`), or `"mean"`
#'     for Empirical Bayes shrinkage toward each component's observed mean.
#' @return List with `score` (weighted average over all components) and
#'     `coverage` (fraction of total weight backed by non-NA evidence in
#'     `[0, 1]`, or `NULL` when `include_coverage = FALSE`).
#'     Rows where all weights are zero return NA.
#' @keywords internal
compute_weighted_components <- function(
  ...,
  weights,
  include_coverage = TRUE,
  na_prior = 0
) {
  values <- list(...)

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

  use_mean_prior <- is.character(na_prior) &&
    length(na_prior) == 1L &&
    identical(na_prior, "mean")
  if (!use_mean_prior) {
    if (
      !is.numeric(na_prior) ||
        !(length(na_prior) %in% c(1L, length(weights))) ||
        anyNA(na_prior) ||
        any(na_prior < 0 | na_prior > 1)
    ) {
      cli::cli_abort(
        "na_prior must be 'mean' or numeric in [0, 1] of length 1 or length(weights)",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }
    na_prior_vec <- rep_len(as.double(na_prior), length(weights))
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

  # Accumulate normalized weighted sum and evidence coverage directly
  w_norm <- weights / weight_sum
  active_k <- which(w_norm != 0)
  acc <- Reduce(
    function(st, k) {
      w <- w_norm[[k]]
      v <- values[[k]]
      if (length(v) != n) {
        v <- rep_len(v, n)
      }
      na_mask <- is.na(v)
      cov_next <- if (include_coverage) {
        st$coverage + (!na_mask) * w
      } else {
        NULL
      }
      if (any(na_mask)) {
        v <- as.double(v)
        prior_k <- if (use_mean_prior) {
          if (all(na_mask)) 0 else mean(v[!na_mask])
        } else {
          na_prior_vec[[k]]
        }
        v[na_mask] <- prior_k
      }
      term <- if (w == 1) as.double(v) else v * w
      list(score = st$score + term, coverage = cov_next)
    },
    active_k,
    init = list(
      score = numeric(n),
      coverage = if (include_coverage) numeric(n) else NULL
    )
  )

  acc
}

#' Compute weighted scores from evidence components
#'
#' @description Convenience wrapper around [compute_weighted_components()] that
#'   returns only the weighted score vector.
#'
#' @param ... [numeric] Columns/vectors of component values (passed as
#'   separate arguments).
#' @param weights [numeric] Vector of non-negative weights (one per
#'   component).
#' @param na_prior [numeric] Prior value for missing (`NA`)
#'   modalities (default `0`).
#'
#' @return Numeric vector of weighted scores, or NA for rows with no evidence.
#' @keywords internal
compute_weighted_sum <- function(..., weights, na_prior = 0) {
  compute_weighted_components(
    ...,
    weights = weights,
    include_coverage = FALSE,
    na_prior = na_prior
  )$score
}

#' Compute continuous Cauchy penalty for RT and m/z errors
#'
#' @description Computes a smooth decay factor in `(0, 1]` from candidate
#'   retention time error and/or m/z error:
#'   `1 / (1 + (error_rt / rt_half_min)^2) * 1 / (1 + (error_mz / mz_half_da)^2)`.
#'   Missing (`NA`) errors receive a neutral factor of `1`.
#'
#' @param error_rt [numeric] Retention time errors (minutes).
#' @param error_mz [numeric] Mass-to-charge errors (Da).
#' @param rt_half_min [numeric] Half-weight RT error scale (> 0, default 0.05).
#' @param mz_half_da [numeric] Half-weight m/z error scale (> 0, default 0.005).
#'
#' @return Numeric vector of multiplicative factors in `(0, 1]`.
#' @keywords internal
compute_rt_mz_penalty <- function(
  error_rt = NULL,
  error_mz = NULL,
  rt_half_min = 0.05,
  mz_half_da = 0.005
) {
  if (
    !is.numeric(rt_half_min) ||
      length(rt_half_min) != 1L ||
      is.na(rt_half_min) ||
      rt_half_min <= 0
  ) {
    cli::cli_abort(
      "rt_half_min must be a positive numeric scalar",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  if (
    !is.numeric(mz_half_da) ||
      length(mz_half_da) != 1L ||
      is.na(mz_half_da) ||
      mz_half_da <= 0
  ) {
    cli::cli_abort(
      "mz_half_da must be a positive numeric scalar",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  n_rt <- if (is.null(error_rt)) 0L else length(error_rt)
  n_mz <- if (is.null(error_mz)) 0L else length(error_mz)
  n <- max(n_rt, n_mz)
  if (n == 0L) {
    return(numeric(0L))
  }
  pen <- rep.int(1, n)
  if (!is.null(error_rt)) {
    rt_v <- rep_len(as.double(error_rt), n)
    ok_rt <- !is.na(rt_v) & is.finite(rt_v)
    if (any(ok_rt)) {
      pen[ok_rt] <- pen[ok_rt] / (1 + (rt_v[ok_rt] / rt_half_min)^2)
    }
  }
  if (!is.null(error_mz)) {
    mz_v <- rep_len(as.double(error_mz), n)
    ok_mz <- !is.na(mz_v) & is.finite(mz_v)
    if (any(ok_mz)) {
      pen[ok_mz] <- pen[ok_mz] / (1 + (mz_v[ok_mz] / mz_half_da)^2)
    }
  }
  pen
}
