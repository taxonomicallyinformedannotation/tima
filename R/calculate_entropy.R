#' @title Calculate entropy
#'
#' @description This function is part of the creation of edges
#'
#' @param index Index
#' @param target Target
#' @param frags Fragments
#' @param ms2_tolerance MS2 tolerance
#' @param ppm_tolerance ppm tolerance
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
calculate_entropy <- function(index,
                              target,
                              frags,
                              ms2_tolerance,
                              ppm_tolerance) {
  score <-
    msentropy::calculate_entropy_similarity(
      frags[[index]],
      frags[[target]],
      min_mz = 0,
      max_mz = 5000,
      noise_threshold = 0,
      ms2_tolerance_in_da = ms2_tolerance,
      ms2_tolerance_in_ppm = ppm_tolerance,
      max_peak_num = -1,
      clean_spectra = TRUE
    )

  if (score >= 0.1) {
    return(
      list(
        "feature_id" = index,
        "target_id" = target,
        "score" = as.numeric(score),
        "count_peaks_matched" = NA_integer_,
        "presence_ratio" = NA_real_
      )
    )
  }
}
