#' @title Calculate entropy
#'
#' @description This function calculates entropy similarity between two spectra
#'
#' @importFrom msentropy calculate_entropy_similarity
#' @importFrom tidytable tidytable
#'
#' @param index Index of the first spectrum
#' @param target Index of the second spectrum
#' @param frags List of fragments
#' @param ms2_tolerance MS2 tolerance
#' @param ppm_tolerance ppm tolerance
#' @param threshold Threshold value for the score
#'
#' @return A list containing the calculated entropy similarity values
#'         or NULL if the score is below the threshold
#'
#' @export
#'
#' @examples NULL
calculate_entropy <- function(index,
                              target,
                              frags,
                              ms2_tolerance,
                              ppm_tolerance,
                              threshold = 0.1) {
  score <- calculate_entropy_similarity(
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

  if (score >= threshold) {
    return(
      tidytable(
        feature_id = index,
        target_id = target,
        score = as.numeric(score),
        count_peaks_matched = NA_integer_,
        presence_ratio = NA_real_
      )
    )
  } else {
    return(NULL)
  }
}
