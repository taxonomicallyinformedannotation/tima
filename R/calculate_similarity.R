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
  } else if (method == "gnps") {
    query_masses <- query_spectrum[, 1]
    target_masses <- target_spectrum[, 1]

    ## Replaced with internal C version
    # map <- MsCoreUtils::join_gnps(
    map <- .Call(
      "join_gnps",
      x = query_masses,
      y = target_masses,
      xPrecursorMz = query_precursor,
      yPrecursorMz = target_precursor,
      tolerance = dalton,
      ppm = ppm
    )

    matched_x <- map[[1]]
    matched_y <- map[[2]]

    if (length(matched_x) == 0 || length(matched_y) == 0) {
      return(0.0)
    }

    x_mat <- query_spectrum[matched_x, , drop = FALSE]
    y_mat <- target_spectrum[matched_y, , drop = FALSE]

    return(
      # MsCoreUtils::gnps(
      .Call(
        "gnps",
        x = x_mat,
        y = y_mat
      )
    )
  } else {
    stop("Invalid method. Choose 'entropy' or 'gnps'.")
  }
}
