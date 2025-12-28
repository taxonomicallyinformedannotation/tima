# Helper Functions ----

#' Calculate similarity for a target spectrum
#' @keywords internal
.calculate_target_similarity <- function(
  target_idx,
  frags,
  precs,
  method,
  ms2_tolerance,
  ppm_tolerance,
  query_spectrum,
  query_precursor
) {
  target_spectrum <- frags[[target_idx]]
  target_precursor <- precs[[target_idx]]
  calculate_similarity(
    method = method,
    query_spectrum = query_spectrum,
    target_spectrum = target_spectrum,
    query_precursor = query_precursor,
    target_precursor = target_precursor,
    dalton = ms2_tolerance,
    ppm = ppm_tolerance,
    return_matched_peaks = TRUE
  )
}

#' Process query spectrum against all subsequent spectra
#' @keywords internal
.process_query_spectrum <- function(
  index,
  nspecs,
  frags,
  precs,
  method,
  ms2_tolerance,
  ppm_tolerance,
  threshold,
  matched_peaks,
  score_bins_acc
) {
  target_indices <- (index + 1L):nspecs
  query_spectrum <- frags[[index]]
  query_precursor <- precs[[index]]

  # Calculate similarities for all targets - use base lapply for speed
  results <- lapply(
    X = target_indices,
    FUN = .calculate_target_similarity,
    frags = frags,
    precs = precs,
    method = method,
    ms2_tolerance = ms2_tolerance,
    ppm_tolerance = ppm_tolerance,
    query_spectrum = query_spectrum,
    query_precursor = query_precursor
  )

  # Extract scores and matches
  # Handle both list format (score/matches) and numeric format (entropy)

  .extract_score <- function(x) {
    if (is.list(x) && "score" %in% names(x)) {
      x$score
    } else if (is.numeric(x)) {
      x
    } else {
      0.0
    }
  }

  .extract_matches <- function(x) {
    if (is.list(x) && "matches" %in% names(x)) {
      x$matches
    } else {
      0L
    }
  }

  scores <- vapply(
    X = results,
    FUN = .extract_score,
    numeric(1L),
    USE.NAMES = FALSE
  )

  matches <- vapply(
    X = results,
    FUN = .extract_matches,
    integer(1L),
    USE.NAMES = FALSE
  )

  # Accumulate raw score distribution prior to filtering (0.1 bins)
  score_bins_acc <- score_bins_acc + accumulate_similarity_bins(scores)

  # Filter by thresholds (single filtering point)
  valid_indices <- which(scores >= threshold & matches >= matched_peaks)

  if (length(valid_indices) > 0L) {
    list(
      df = data.frame(
        feature_id = index,
        target_id = target_indices[valid_indices],
        score = scores[valid_indices],
        matched_peaks = matches[valid_indices]
      ),
      bins = score_bins_acc
    )
  } else {
    list(df = NULL, bins = score_bins_acc)
  }
}

#' @title Create spectral similarity network edges
#'
#' @description Calculates pairwise spectral similarity between all spectra to
#'     create a network edge list. Uses parallel processing via purrr for
#'     efficiency.
#'
#' @include calculate_similarity.R
#' @include constants.R
#' @include logs_utils.R
#' @include predicates_utils.R
#' @include validations_utils.R
#'
#' @param frags List of fragment spectra matrices
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
#' # Create network edges from spectra
#' edges <- create_edges(
#'   frags = fragment_list,
#'   nspecs = length(fragment_list),
#'   precs = precursor_mz,
#'   method = "cosine",
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

  # Early exit for insufficient spectra
  if (nspecs < 2L) {
    log_complete(ctx, n_edges = 0, note = "Less than 2 spectra")
    return(tidytable::tidytable(
      feature_id = NA_integer_,
      target_id = NA_integer_,
      score = NA_real_,
      matched_peaks = NA_integer_
    ))
  }

  # Validate input consistency
  if (length(frags) != nspecs || length(precs) != nspecs) {
    stop(
      "Length mismatch: frags (",
      length(frags),
      "), ",
      "precs (",
      length(precs),
      "), nspecs (",
      nspecs,
      ") must be consistent"
    )
  }

  # Calculate Pairwise Similarities ----

  # Create all pairwise comparisons
  indices <- seq_len(nspecs - 1L)
  n_comparisons <- sum(indices)

  log_metadata(ctx,
    phase = "calculating",
    n_comparisons = n_comparisons
  )

  # Pre-calculate length once for efficiency
  n_queries <- length(indices)

  # Progress counter for logging
  progress_counter <- 0L

  # Initialize score bins accumulator (all-zero vector with labels)
  score_bins_acc <- accumulate_similarity_bins(numeric(0))

  lst <- lapply(
    X = indices,
    FUN = function(i, ...) {
      # Increment progress counter in parent environment
      progress_counter <<- progress_counter + 1L

      res <- .process_query_spectrum(i, ..., score_bins_acc = score_bins_acc)

      # Update accumulator with returned bins
      score_bins_acc <<- res$bins

      if (progress_counter %% 500L == 0L) {
        log_info("Processed %d / %d queries", progress_counter, n_queries)
      }
      res$df
    },
    nspecs = nspecs,
    frags = frags,
    precs = precs,
    method = method,
    ms2_tolerance = ms2_tolerance,
    ppm_tolerance = ppm_tolerance,
    threshold = threshold,
    matched_peaks = matched_peaks
  )

  # Combine Results ----

  # Remove NULL entries and combine
  edges <- lst[
    !vapply(
      X = lst,
      FUN = is.null,
      logical(1L),
      USE.NAMES = FALSE
    )
  ]

  # Log the raw similarity distribution before threshold filtering (accumulated)
  # Note: distribution is for all pairwise comparisons evaluated
  log_similarity_distribution_counts(
    counts = score_bins_acc,
    title = "Here is the distribution of edge similarity scores (0.1 bins) BEFORE filtering:"
  )

  if (length(edges) > 0L) {
    result <- tidytable::bind_rows(edges)
    log_complete(ctx,
      n_edges = nrow(result),
      n_comparisons = n_comparisons,
      pass_rate = sprintf("%.1f%%", 100 * nrow(result) / n_comparisons)
    )
    result
  } else {
    log_complete(ctx, n_edges = 0, note = "No edges passed thresholds")
    tidytable::tidytable(
      feature_id = NA_integer_,
      target_id = NA_integer_,
      score = NA_real_,
      matched_peaks = NA_integer_
    )
  }
}
