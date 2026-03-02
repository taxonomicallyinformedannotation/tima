# Helper Functions ----

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
#'
#' @param frags List of aligned fragment spectra matrices
#' @param nspecs Integer number of spectra
#' @param precs Numeric vector of precursor m/z values
#' @param method Similarity method ("entropy", "gnps", or "cosine")
#' @param ms2_tolerance MS2 tolerance in Daltons
#' @param ppm_tolerance PPM tolerance
#' @param threshold Minimum similarity score threshold
#' @param matched_peaks Minimum number of matched peaks required
#'
#' @return Data frame with columns: feature_id, target_id, score, matched_peaks.
#'     Returns empty data frame with NA values if no edges pass thresholds.
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
#'   matched_peaks = 6
#' )
#' }
create_edges <- function(
  frags,
  nspecs,
  precs,
  method,
  ms2_tolerance,
  ppm_tolerance,
  threshold,
  matched_peaks
) {
  empty_result <- tidytable::tidytable(
    feature_id = NA_integer_,
    target_id = NA_integer_,
    score = NA_real_,
    matched_peaks = NA_integer_
  )

  # Initialize logging context
  ctx <- log_operation(
    "create_edges",
    n_spectra = nspecs,
    method = method,
    threshold = threshold,
    min_peaks = matched_peaks
  )

  # Input Validation ----
  validate_choice(method, VALID_SIMILARITY_METHODS, param_name = "method")

  if (nspecs < 2L) {
    log_complete(ctx, n_edges = 0, note = "Less than 2 spectra")
    return(empty_result)
  }

  if (length(frags) != nspecs || length(precs) != nspecs) {
    stop(
      "Length mismatch: frags (",
      length(frags),
      "), ",
      "precs (",
      length(precs),
      "), nspecs (",
      nspecs,
      ")",
      call. = FALSE
    )
  }

  # Pre-flight: sanitize spectra once if needed ----
  sample_idx <- unique(c(
    1L,
    as.integer(seq(1L, nspecs, length.out = min(20L, nspecs)))
  ))
  needs_sanitize <- FALSE
  for (idx in sample_idx) {
    sp <- frags[[idx]]
    if (
      is.matrix(sp) &&
        nrow(sp) >= 2L &&
        !is_spectrum_sanitized(
          sp,
          tolerance = ms2_tolerance,
          ppm = ppm_tolerance
        )
    ) {
      needs_sanitize <- TRUE
      break
    }
  }
  if (needs_sanitize) {
    log_warn(
      "Unsanitized spectra detected. Sanitizing %d spectra before edge creation.",
      nspecs
    )
    frags <- lapply(frags, function(sp) {
      if (
        is.matrix(sp) &&
          nrow(sp) >= 2L &&
          !is_spectrum_sanitized(
            sp,
            tolerance = ms2_tolerance,
            ppm = ppm_tolerance
          )
      ) {
        sanitize_spectrum_matrix(
          sp,
          tolerance = ms2_tolerance,
          ppm = ppm_tolerance
        )
      } else {
        sp
      }
    })
  }

  # Calculate Pairwise Similarities ----

  n_queries <- nspecs - 1L
  n_comparisons <- as.double(nspecs) * (nspecs - 1L) / 2

  log_metadata(ctx, phase = "calculating", n_comparisons = n_comparisons)

  # Score histogram bins (accumulated across all queries)
  bin_counts <- integer(10L)
  use_gnps <- (method == "gnps")
  progress_counter <- 0L

  results <- lapply(seq_len(n_queries), function(i) {
    progress_counter <<- progress_counter + 1L
    if (progress_counter %% 500L == 0L) {
      log_info("Processed %d / %d queries", progress_counter, n_queries)
    }

    q_sp <- frags[[i]]
    q_pre <- precs[i]
    targets <- (i + 1L):nspecs

    # Inner loop: one .Call per target, collect scores + matches
    pairs <- lapply(targets, function(j) {
      t_sp <- frags[[j]]
      t_pre <- precs[j]

      if (use_gnps) {
        res <- .Call(
          "gnps_compute",
          q_sp,
          t_sp,
          q_pre,
          t_pre,
          ms2_tolerance,
          ppm_tolerance
        )
        sc <- res[["score"]]
        mp <- res[["matches"]]
      } else {
        res <- calculate_similarity(
          method = method,
          query_spectrum = q_sp,
          target_spectrum = t_sp,
          query_precursor = q_pre,
          target_precursor = t_pre,
          dalton = ms2_tolerance,
          ppm = ppm_tolerance,
          return_matched_peaks = TRUE
        )
        sc <- if (is.list(res)) res[["score"]] else as.numeric(res)
        mp <- if (is.list(res)) res[["matches"]] else 0L
      }

      # Accumulate score histogram in parent env
      if (is.finite(sc)) {
        bin <- min(9L, max(0L, as.integer(sc * 10)))
        bin_counts[bin + 1L] <<- bin_counts[bin + 1L] + 1L
      }

      if (isTRUE(sc >= threshold) && isTRUE(mp >= matched_peaks)) {
        c(i, j, sc, mp)
      }
    })

    # Drop NULLs and combine into matrix (4 columns) for this query
    pairs <- pairs[!vapply(pairs, is.null, FALSE, USE.NAMES = FALSE)]
    if (length(pairs) > 0L) {
      do.call(rbind, pairs)
    }
  })

  # Score distribution logging ----
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
    title = "Here is the distribution of edge similarity scores (0.1 bins) BEFORE filtering:"
  )

  # Combine all query results into one matrix, then to tidytable ----
  results <- results[!vapply(results, is.null, FALSE, USE.NAMES = FALSE)]

  if (length(results) > 0L) {
    mat <- do.call(rbind, results)
    result <- tidytable::tidytable(
      feature_id = as.integer(mat[, 1L]),
      target_id = as.integer(mat[, 2L]),
      score = mat[, 3L],
      matched_peaks = as.integer(mat[, 4L])
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
