#' @title Create edges
#'
#' @description This function applies similarity calculation to a list of
#'        spectra to create edges
#'
#' @param frags Fragments
#' @param nspecs Number of spectra
#' @param ms2_tolerance MS2 tolerance
#' @param ppm_tolerance ppm tolerance
#' @param threshold Threshold
#'
#' @return NULL
#'
#' @examples NULL
create_edges <- function(frags,
                         nspecs,
                         ms2_tolerance,
                         ppm_tolerance,
                         threshold) {
  indices <- 1:(nspecs - 1)
  p <- progressr::progressor(along = indices)

  edges <- furrr::future_map(
    .x = indices,
    .f = function(index) {
      target_indices <- (index + 1):nspecs
      p()

      scores <- vapply(
        target_indices,
        function(target) {
          msentropy::calculate_entropy_similarity(
            peaks_a = frags[[index]],
            peaks_b = frags[[target]],
            min_mz = 0,
            max_mz = 5000,
            noise_threshold = 0,
            ms2_tolerance_in_da = ms2_tolerance,
            ms2_tolerance_in_ppm = ppm_tolerance,
            max_peak_num = -1,
            clean_spectra = TRUE
          )
        },
        numeric(1)
      )

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

  tidytable::bind_rows(edges[!sapply(edges, is.null)])
}
