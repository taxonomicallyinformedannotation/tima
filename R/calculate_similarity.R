#' @title Calculate similarity between spectra
#'
#' @description Efficiently calculates similarity scores between query and target spectra
#'        using either entropy or GNPS methods
#'
#' @param method Method ("entropy" or "gnps")
#' @param query_spectrum Query spectrum matrix
#' @param target_spectrum Target spectrum matrix
#' @param query_precursor Query precursor
#' @param target_precursor Target precursor
#' @param dalton Dalton tolerance
#' @param ppm PPM tolerance
#'
#' @return Similarity score or NA_real_ if calculation fails
#'
#' @examples NULL
calculate_similarity <- function(method,
                                 query_spectrum,
                                 target_spectrum,
                                 query_precursor,
                                 target_precursor,
                                 dalton,
                                 ppm) {
  # Pre-validate inputs to avoid unnecessary computations
  if (!is.matrix(query_spectrum) || !is.matrix(target_spectrum)) {
    return(NA_real_)
  }

  # Create function lookup table instead of using switch
  similarity_functions <- list(
    entropy = function() {
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
      )
    },
    gnps = function() {
      # Pre-compute the mass values to avoid repeated column access
      query_masses <- query_spectrum[, 1]
      target_masses <- target_spectrum[, 1]

      map <- MsCoreUtils::join_gnps(
        x = query_masses,
        y = target_masses,
        xPrecursorMz = query_precursor,
        yPrecursorMz = target_precursor,
        tolerance = dalton,
        ppm = ppm
      )

      # Only proceed if we have matches
      if (length(map[[1]]) > 0 && length(map[[2]]) > 0) {
        MsCoreUtils::gnps(
          x = query_spectrum[map[[1]], ],
          y = target_spectrum[map[[2]], ],
          .check = FALSE
        )
      } else {
        NA_real_
      }
    }
  )

  # Execute selected method with error handling
  tryCatch(
    similarity_functions[[method]](),
    error = function(e) NA_real_
  )
}
