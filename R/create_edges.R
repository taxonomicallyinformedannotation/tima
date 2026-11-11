#' @title Create spectral similarity network edges
#'
#' @description Calculates pairwise spectral similarity between all spectra to
#'     create a network edge list.
#'
#' @include calculate_similarity.R
#' @include constants.R
#' @include logs_utils.R
#' @include predicates_utils.R
#' @include sanitize_spectrum_matrix.R
#' @include validations_utils.R
#' @include c_wrappers.R
#'
#' @param frags List of aligned fragment spectra matrices
#' @param nspecs Integer number of spectra
#' @param precs Numeric vector of precursor m/z values
#' @param method Similarity method ("entropy", "gnps", or "cosine")
#' @param ms2_tolerance MS2 tolerance in Daltons
#' @param ppm_tolerance PPM tolerance
#' @param threshold Minimum similarity score threshold (default: \code{0.1}).
#'     Pairs with similarity scores below this threshold are discarded to filter
#'     out random background noise and prevent memory overflow. Set to `NULL`
#'     to disable similarity thresholding and retain all pairwise comparisons.
#' @param matched_peaks Minimum number of matched peaks required. Set to `NULL`
#'     to disable matched-peak filtering.
#' @param assume_sanitized Boolean flag to assume if sanitize or not. FALSE
#'
#' @return Data frame with columns: feature_id, target_id, score, matched_peaks.
#'     Returns empty data frame with NA values if no edges pass thresholds.
#'
#' @family workflow
#'
#' @export
#'
#' @examples
#' \dontrun{
#' edges <- create_edges(
#'   frags = fragment_list,
#'   nspecs = length(fragment_list),
#'   precs = precursor_mz,
#'   method = "gnps",
#'   ms2_tolerance = 0.02,
#'   ppm_tolerance = 10,
#'   threshold = 0.7,
#'   matched_peaks = 6L
#' )
#' }
create_edges <- function(
  frags,
  nspecs,
  precs,
  method,
  ms2_tolerance,
  ppm_tolerance,
  threshold = 0.1,
  matched_peaks = 1L,
  assume_sanitized = FALSE
) {
  empty_result <- tidytable::tidytable(
    feature_id = NA_integer_,
    target_id = NA_integer_,
    score = NA_real_,
    matched_peaks = NA_integer_
  )

  ctx <- log_operation(
    "create_edges",
    n_spectra = nspecs,
    method = method,
    threshold = threshold,
    min_peaks = matched_peaks
  )

  validate_choice(method, VALID_SIMILARITY_METHODS, param_name = "method")

  if (
    !is.null(threshold) &&
      (!is.numeric(threshold) ||
        length(threshold) != 1L ||
        !is.finite(threshold) ||
        threshold < 0 ||
        threshold > 1)
  ) {
    cli::cli_abort(
      "threshold must be NULL or a numeric value between 0 and 1",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (
    !is.null(matched_peaks) &&
      (!is.numeric(matched_peaks) ||
        length(matched_peaks) != 1L ||
        !is.finite(matched_peaks) ||
        matched_peaks < 0)
  ) {
    cli::cli_abort(
      "matched_peaks must be NULL or a non-negative integer",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (nspecs < 2L) {
    log_complete(ctx, n_edges = 0, note = "Less than 2 spectra")
    return(empty_result)
  }

  if (length(frags) != nspecs || length(precs) != nspecs) {
    cli::cli_abort(
      c(
        "length mismatch among frags, precs, and nspecs",
        "x" = paste0(
          "frags=",
          length(frags),
          "; precs=",
          length(precs),
          "; nspecs=",
          nspecs
        )
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  sanitized_indices <- logical(nspecs)
  if (!isTRUE(assume_sanitized) && nspecs >= 2L) {
    needs_san <- vapply(
      frags,
      function(sp) {
        is.matrix(sp) &&
          nrow(sp) >= 2L &&
          !is_spectrum_sanitized(
            sp,
            tolerance = ms2_tolerance,
            ppm = ppm_tolerance
          )
      },
      logical(1L)
    )
    if (any(needs_san)) {
      frags[needs_san] <- lapply(
        frags[needs_san],
        sanitize_spectrum_matrix,
        tolerance = ms2_tolerance,
        ppm = ppm_tolerance
      )
      sanitized_indices[needs_san] <- TRUE
    }
  }

  n_queries <- nspecs - 1L
  n_comparisons <- as.double(nspecs) * (nspecs - 1L) / 2

  log_metadata(ctx, phase = "calculating", n_comparisons = n_comparisons)

  bin_counts <- integer(10L)
  use_gnps <- (method == "gnps")
  precs_num <- as.numeric(precs)
  res_f <- vector("list", n_queries)
  res_t <- vector("list", n_queries)
  res_s <- vector("list", n_queries)
  res_m <- vector("list", n_queries)

  for (i in seq_len(n_queries)) {
    q_sp <- frags[[i]]
    q_pre <- precs[i]
    targets <- (i + 1L):nspecs

    if (use_gnps) {
      res_mat <- gnps_chain_dp_batch_wrapper(
        x = q_sp,
        xPrecursorMz = q_pre,
        y_list = frags[targets],
        yPrecursorMz = precs_num[targets],
        tolerance = ms2_tolerance,
        ppm = ppm_tolerance
      )

      sc_vec <- res_mat[, 1L]
      if (!anyNA(sc_vec) && !any(is.infinite(sc_vec))) {
        bins <- pmin(9L, pmax(0L, as.integer(sc_vec * 10)))
        bin_counts <- bin_counts + tabulate(bins + 1L, nbins = 10L)
      } else {
        finite_sc <- is.finite(sc_vec)
        if (any(finite_sc)) {
          bins <- pmin(9L, pmax(0L, as.integer(sc_vec[finite_sc] * 10)))
          bin_counts <- bin_counts + tabulate(bins + 1L, nbins = 10L)
        }
      }

      kept_idx <- if (!is.null(threshold)) {
        which(!is.na(sc_vec) & sc_vec >= threshold)
      } else {
        seq_along(targets)
      }

      if (length(kept_idx) > 0L) {
        mp_sub <- res_mat[kept_idx, 2L]
        if (!is.null(matched_peaks)) {
          mp_ok <- !is.na(mp_sub) & mp_sub >= matched_peaks
          kept_idx <- kept_idx[mp_ok]
          mp_sub <- mp_sub[mp_ok]
        }
        if (length(kept_idx) > 0L) {
          t_kept <- targets[kept_idx]
          res_f[[i]] <- rep.int(i, length(t_kept))
          res_t[[i]] <- t_kept
          res_s[[i]] <- sc_vec[kept_idx]
          res_m[[i]] <- as.integer(mp_sub)
        }
      }
    } else {
      pair_mat <- vapply(
        targets,
        function(j) {
          res <- calculate_similarity(
            method = method,
            query_spectrum = q_sp,
            target_spectrum = frags[[j]],
            query_precursor = q_pre,
            target_precursor = precs[j],
            dalton = ms2_tolerance,
            ppm = ppm_tolerance,
            return_matched_peaks = TRUE
          )
          if (is.numeric(res) && length(res) >= 2L) {
            c(as.numeric(res[1L]), as.numeric(as.integer(res[2L])))
          } else {
            c(as.numeric(res), 0)
          }
        },
        numeric(2L)
      )
      sc_vec <- pair_mat[1L, ]
      mp_vec <- as.integer(pair_mat[2L, ])

      finite_sc <- is.finite(sc_vec)
      if (any(finite_sc)) {
        bins <- pmin(9L, pmax(0L, as.integer(sc_vec[finite_sc] * 10)))
        bin_counts <- bin_counts + tabulate(bins + 1L, nbins = 10L)
      }

      keep <- rep(TRUE, length(targets))
      if (!is.null(threshold)) {
        keep <- keep & (sc_vec >= threshold)
      }
      if (!is.null(matched_peaks)) {
        keep <- keep & (mp_vec >= matched_peaks)
      }
      keep[is.na(keep)] <- FALSE

      if (any(keep)) {
        t_kept <- targets[keep]
        res_f[[i]] <- rep.int(i, length(t_kept))
        res_t[[i]] <- t_kept
        res_s[[i]] <- sc_vec[keep]
        res_m[[i]] <- mp_vec[keep]
      }
    }
  }

  if (any(sanitized_indices)) {
    log_warn(
      "Sanitized %d/%d spectra on-demand before edge scoring.",
      sum(sanitized_indices),
      nspecs
    )
  }

  bin_labels <- c(
    "[0,0.1]",
    "(0.1,0.2]",
    "(0.2,0.3]",
    "(0.3,0.4]",
    "(0.4,0.5]",
    "(0.5,0.6]",
    "(0.6,0.7]",
    "(0.7,0.8]",
    "(0.8,0.9]",
    "(0.9,1]"
  )
  names(bin_counts) <- bin_labels
  log_similarity_distribution_counts(
    counts = bin_counts,
    title = "Here is the distribution of edge similarity scores (0.1 bins):"
  )

  f_vec <- unlist(res_f, use.names = FALSE)
  if (length(f_vec) > 0L) {
    result <- tidytable::tidytable(
      feature_id = f_vec,
      target_id = unlist(res_t, use.names = FALSE),
      score = unlist(res_s, use.names = FALSE),
      matched_peaks = unlist(res_m, use.names = FALSE)
    )
    rm(
      res_f,
      res_t,
      res_s,
      res_m,
      bin_counts,
      bin_labels,
      sanitized_indices,
      nspecs
    )
    log_complete(
      ctx,
      n_edges = nrow(result),
      n_comparisons = n_comparisons,
      pass_rate = sprintf("%.1f%%", 100 * nrow(result) / n_comparisons)
    )
    result
  } else {
    log_complete(ctx, n_edges = 0, note = "No edges passed thresholds")
    empty_result
  }
}
