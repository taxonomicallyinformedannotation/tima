#' @title Calculate similarity between spectra
#'
#' @description Efficiently calculates similarity scores between query and
#'     target spectra using either entropy, cosine, or GNPS methods
#'
#' @include c_wrappers.R
#'
#' @param method Character string specifying method: "entropy", "gnps", or "cosine"
#' @param query_spectrum Numeric matrix with columns for mz and intensity
#' @param target_spectrum Numeric matrix with columns for mz and intensity
#' @param query_precursor Numeric precursor m/z value for query
#' @param target_precursor Numeric precursor m/z value for target
#' @param dalton Numeric Dalton tolerance for peak matching
#' @param ppm Numeric PPM tolerance for peak matching
#' @param return_matched_peaks Logical; return matched peaks count?
#'     Not compatible with 'entropy' method. Default: FALSE
#' @param ... Additional arguments passed to MsCoreUtils::join
#'
#' @return Numeric similarity score (0-1), or list with score and matches
#'     if return_matched_peaks = TRUE. Returns 0.0 if calculation fails.
#'
#' @export
#'
#' @examples
#' sp_1 <- cbind(
#'   mz = c(10, 36, 63, 91, 93),
#'   intensity = c(14, 15, 999, 650, 1)
#' )
#' precursor_1 <- 123.4567
#' precursor_2 <- precursor_1 + 14
#' sp_2 <- cbind(
#'   mz = c(10, 12, 50, 63, 105),
#'   intensity = c(35, 5, 16, 999, 450)
#' )
#' calculate_similarity(
#'   method = "entropy",
#'   query_spectrum = sp_1,
#'   target_spectrum = sp_2,
#'   query_precursor = precursor_1,
#'   target_precursor = precursor_2,
#'   dalton = 0.005,
#'   ppm = 10.0
#' )
#' calculate_similarity(
#'   method = "gnps",
#'   query_spectrum = sp_1,
#'   target_spectrum = sp_2,
#'   query_precursor = precursor_1,
#'   target_precursor = precursor_2,
#'   dalton = 0.005,
#'   ppm = 10.0,
#'   return_matched_peaks = TRUE
#' )
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
  # Validate method
  valid_methods <- c("cosine", "entropy", "gnps")
  if (!method %in% valid_methods) {
    stop(
      "Invalid method '",
      method,
      "'. Must be one of: ",
      paste(valid_methods, collapse = ", ")
    )
  }

  # Validate spectrum inputs
  if (!is.matrix(query_spectrum) || !is.matrix(target_spectrum)) {
    stop("Spectra must be matrices")
  }

  if (ncol(query_spectrum) < 2L || ncol(target_spectrum) < 2L) {
    stop("Spectra must have at least 2 columns (mz, intensity)")
  }

  # Handle entropy method separately
  if (method == "entropy") {
    if (return_matched_peaks) {
      logger::log_warn("return_matched_peaks not supported with entropy method")
    }

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

  # Extract masses for matching
  query_masses <- query_spectrum[, 1L]
  target_masses <- target_spectrum[, 1L]

  # Get peak matching map
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

  matched_x <- map[[1L]]
  matched_y <- map[[2L]]

  # No matches found
  if (length(matched_x) == 0L) {
    return(if (return_matched_peaks) list(score = 0.0, matches = 0L) else 0.0)
  }

  # Extract matched peaks
  x_mat <- query_spectrum[matched_x, , drop = FALSE]
  y_mat <- target_spectrum[matched_y, , drop = FALSE]

  # Calculate similarity
  result <- gnps_wrapper(x = x_mat, y = y_mat)

  # Return appropriate format
  if (return_matched_peaks) {
    result
  } else {
    result$score
  }
}
