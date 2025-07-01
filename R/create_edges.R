#' @title Create edges
#'
#' @description This function applies similarity calculation to a list of
#'        spectra to create edges
#'
#' @include calculate_similarity.R
#'
#' @param frags Fragments
#' @param nspecs Number of spectra
#' @param precs Precursors
#' @param method Method
#' @param ms2_tolerance MS2 tolerance
#' @param ppm_tolerance ppm tolerance
#' @param threshold Threshold
#' @param matched_peaks Matched peaks
#'
#' @return NULL
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
  indices <- 1L:(nspecs - 1L)

  edges <- purrr::map(
    .progress = TRUE,
    .x = indices,
    .f = function(index) {
      target_indices <- (index + 1L):nspecs
      query_spectrum <- frags[[index]]
      query_precursor <- precs[[index]]

      results <- purrr::map(
        .x = target_indices,
        .f = function(index) {
          target_spectrum <- frags[[index]]
          target_precursor <- precs[[index]]
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
      scores <- results |>
        purrr::map("score") |>
        as.list() |>
        unlist()
      matches <- results |>
        purrr::map("matches") |>
        as.list() |>
        unlist()

      valid_indices <- which(scores >= threshold & matches >= matched_peaks)

      if (length(valid_indices) > 0) {
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
  edges <- edges[!sapply(edges, is.null)]
  if (length(edges) > 0) {
    tidytable::bind_rows(edges)
  } else {
    tidytable::tidytable(
      feature_id = NA_integer_,
      target_id = NA_integer_,
      score = NA_real_,
      matched_peaks = NA_integer_
    )
  }
}
