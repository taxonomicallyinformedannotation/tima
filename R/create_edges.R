#' @title Create edges
#'
#' @description This function calculates pairwise spectral similarity between
#'     all spectra to create a network edge list. Uses parallel processing
#'     via purrr for efficiency.
#'
#' @include calculate_similarity.R
#'
#' @param frags List of fragment spectra matrices
#' @param nspecs Integer number of spectra
#' @param precs Numeric vector of precursor m/z values
#' @param method Character string similarity method ("entropy", "gnps", or "cosine")
#' @param ms2_tolerance Numeric MS2 tolerance in Daltons
#' @param ppm_tolerance Numeric PPM tolerance
#' @param threshold Numeric minimum similarity score threshold
#' @param matched_peaks Integer minimum number of matched peaks required
#'
#' @return A data frame with columns: feature_id, target_id, score, matched_peaks.
#'     Returns empty data frame with NA values if no edges pass thresholds.
#'
#' @examples NULL
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
  # Validate inputs
  if (nspecs < 2L) {
    logger::log_warn("Less than 2 spectra provided, no edges to create")
    return(tidytable::tidytable(
      feature_id = NA_integer_,
      target_id = NA_integer_,
      score = NA_real_,
      matched_peaks = NA_integer_
    ))
  }

  if (length(frags) != nspecs || length(precs) != nspecs) {
    stop("Length mismatch: frags, precs, and nspecs must be consistent")
  }

  # Create all pairwise comparisons
  indices <- seq_len(nspecs - 1L)

  logger::log_debug(
    "Calculating ",
    sum(seq_len(nspecs - 1L)),
    " pairwise similarities"
  )

  edges <- purrr::map(
    .progress = TRUE,
    .x = indices,
    .f = function(index) {
      target_indices <- (index + 1L):nspecs
      query_spectrum <- frags[[index]]
      query_precursor <- precs[[index]]

      # Calculate similarities for all targets
      results <- purrr::map(
        .x = target_indices,
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

      # Extract scores and matches efficiently
      scores <- vapply(results, `[[`, numeric(1L), "score")
      matches <- vapply(results, `[[`, integer(1L), "matches")

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

  # Remove NULL entries and combine
  edges <- edges[!vapply(edges, is.null, logical(1L))]

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
