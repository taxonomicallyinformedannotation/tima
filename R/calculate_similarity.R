#' @title Calculate similarity between spectra
#'
#' @description Efficiently calculates similarity scores between query and target spectra
#'        using either entropy or GNPS methods
#'
#' @include c_wrappers.R
#'
#' @param method Method ("entropy" or "gnps")
#' @param query_spectrum Query spectrum matrix
#' @param target_spectrum Target spectrum matrix
#' @param query_precursor Query precursor
#' @param target_precursor Target precursor
#' @param dalton Dalton tolerance
#' @param ppm PPM tolerance
#' @param return_matched_peaks Return matched peaks. Not compatible with 'entropy'. Default: FALSE
#' @param ... Not documented for now
#'
#' @return Similarity score or NA_real_ if calculation fails
#'
#' @examples NULL
calculate_similarity <- function(
  method,
  query_spectrum,
  target_spectrum,
  query_precursor,
  target_precursor,
  dalton,
  ppm,
  return_matched_peaks = FALSE,
  ...
) {
  if (!method %in% c("cosine", "entropy", "gnps")) {
    logger::log_fatal("Invalid method. Choose 'cosine', 'entropy' or 'gnps'.")
    stop()
  }
  if (method == "entropy") {
    return(
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
    )
  }
  query_masses <- query_spectrum[, 1]
  target_masses <- target_spectrum[, 1]

  map <- switch(
    method,
    "gnps" = join_gnps_wrapper(
      x = query_masses,
      y = target_masses,
      xPrecursorMz = query_precursor,
      yPrecursorMz = target_precursor,
      tolerance = dalton,
      ppm = ppm
    ),
    "cosine" = MsCoreUtils::join(
      x = query_masses,
      y = target_masses,
      tolerance = dalton,
      ppm = ppm,
      ## allows for type = c("outer", "inner", "left", "right")
      ...
    )
  )

  matched_x <- map[[1]]
  matched_y <- map[[2]]

  if (length(matched_x) == 0L) {
    return(0.0)
  }

  x_mat <- query_spectrum[matched_x, , drop = FALSE]
  y_mat <- target_spectrum[matched_y, , drop = FALSE]

  result <- gnps_wrapper(x = x_mat, y = y_mat)

  if (return_matched_peaks) {
    result
  } else {
    ## Do not report number of matched peaks by default for now
    result$score
  }
}
