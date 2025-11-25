#' @title Create spectral similarity network edges
#'
#' @description Calculates pairwise spectral similarity between all spectra to
#'     create a network edge list. Uses parallel processing via purrr for
#'     efficiency.
#'
#' @include calculate_similarity.R
#' @include constants.R
#' @include validators.R
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
  # Input Validation ----
  validate_choice(method, VALID_SIMILARITY_METHODS, param_name = "method")

  # Early exit for insufficient spectra
  if (nspecs < 2L) {
    logger::log_warn("Less than 2 spectra provided, no edges to create")
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

  logger::log_debug("Calculating {n_comparisons} pairwise similarities")

  # Disable progress bar in subprocess environments to prevent crashes
  show_progress <- interactive() && !isTRUE(getOption("knitr.in.progress"))

  edges <- purrr::map(
    .progress = show_progress,
    .x = indices,
    # TODO
    .f = function(index) {
      target_indices <- (index + 1L):nspecs
      query_spectrum <- frags[[index]]
      query_precursor <- precs[[index]]

      # Calculate similarities for all targets
      results <- purrr::map(
        .x = target_indices,
        # TODO
        .f = function(target_idx) {
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
      )

      # Extract scores and matches
      # Handle both list format (score/matches) and numeric format (entropy)
      scores <- vapply(
        results,
        function(x) {
          if (is.list(x) && "score" %in% names(x)) {
            x$score
          } else if (is.numeric(x)) {
            x
          } else {
            0.0
          }
        },
        numeric(1L),
        USE.NAMES = FALSE
      )

      matches <- vapply(
        results,
        function(x) {
          if (is.list(x) && "matches" %in% names(x)) {
            x$matches
          } else {
            0L # No match count for entropy method
          }
        },
        integer(1L),
        USE.NAMES = FALSE
      )

      # Filter by thresholds
      valid_indices <- which(scores >= threshold & matches >= matched_peaks)

      if (length(valid_indices) > 0L) {
        data.frame(
          feature_id = index,
          target_id = target_indices[valid_indices],
          score = scores[valid_indices],
          matched_peaks = matches[valid_indices]
        )
      } else {
        NULL
      }
    }
  )

  # Combine and Return Results ----

  # Remove NULL entries and combine
  edges <- edges[!vapply(edges, is.null, logical(1L), USE.NAMES = FALSE)]

  if (length(edges) > 0L) {
    result <- tidytable::bind_rows(edges)
    logger::log_info("Created {nrow(result)} edges passing thresholds")
    result
  } else {
    logger::log_warn("No edges passed the specified thresholds")
    tidytable::tidytable(
      feature_id = NA_integer_,
      target_id = NA_integer_,
      score = NA_real_,
      matched_peaks = NA_integer_
    )
  }
}
