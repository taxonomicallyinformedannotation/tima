#' @title Create edges
#'
#' @description This function applies similarity calculation to a list of
#'        spectra to create edges
#'
#' @param frags Fragments
#' @param nspecs Number of spectra
#' @param precs Precursors
#' @param method Method
#' @param ms2_tolerance MS2 tolerance
#' @param ppm_tolerance ppm tolerance
#' @param threshold Threshold
#'
#' @return NULL
#'
#' @examples NULL
create_edges <- function(frags,
                         nspecs,
                         precs,
                         method,
                         ms2_tolerance,
                         ppm_tolerance,
                         threshold) {
  indices <- 1:(nspecs - 1)

  edges <- purrr::map(
    .progress = TRUE,
    .x = indices,
    .f = function(index) {
      target_indices <- (index + 1):nspecs

      scores <- vapply(target_indices, function(target) {
        calculate_similarity(
          method = method,
          query_spectrum = frags[[index]],
          target_spectrum = frags[[target]],
          query_precursor = precs[[index]],
          target_precursor = precs[[target]],
          dalton = ms2_tolerance,
          ppm = ppm_tolerance
        )
      }, numeric(1))

      valid_indices <- which(scores >= threshold)

      if (length(valid_indices) > 0) {
        data.frame(
          feature_id = index,
          target_id = target_indices[valid_indices],
          score = scores[valid_indices]
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
      score = NA_real_
    )
  }
}
