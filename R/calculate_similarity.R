#' @title Calculate similarity
#'
#' @description This function applies similarity calculation to a list of
#'        spectra to obtain similarity scores
#'
#' @param method Method
#' @param query_spectrum Query spectrum
#' @param target_spectrum Target spectrum
#' @param query_precursor Query precursor
#' @param target_precursor Target precursor
#' @param dalton Dalton
#' @param ppm Ppm
#'
#' @return NULL
#'
#' @examples NULL
calculate_similarity <- function(method,
                                 query_spectrum,
                                 target_spectrum,
                                 query_precursor,
                                 target_precursor,
                                 dalton,
                                 ppm) {
  switch(method,
    "entropy" = tryCatch(
      msentropy::calculate_entropy_similarity(
        peaks_a = query_spectrum,
        peaks_b = target_spectrum,
        min_mz = 0,
        max_mz = 5000,
        noise_threshold = 0,
        ms2_tolerance_in_da = dalton,
        ms2_tolerance_in_ppm = ppm,
        max_peak_num = -1,
        clean_spectra = TRUE
      ),
      error = function(e) {
        NA_real_
      }
    ),
    "gnps" = tryCatch(
      {
        lib_precursor <- lib_precursors[[lib_idx]]
        map <- MsCoreUtils::join_gnps(
          x = query_spectrum[, 1],
          y = target_spectrum[, 1],
          xPrecursorMz = query_precursor,
          yPrecursorMz = target_precursor,
          tolerance = dalton,
          ppm = ppm
        )
        MsCoreUtils::gnps(x = query_spectrum[map[[1]], ], y = target_spectrum[map[[2]], ])
      },
      error = function(e) {
        NA_real_
      }
    ),
    NA_real_
  )
}
