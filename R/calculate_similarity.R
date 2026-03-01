#' @title Calculate similarity between spectra
#'
#' @description Calculates similarity scores between query and
#'     target spectra using either entropy, cosine, or GNPS methods.
#'
#'     **Important:** For correct results with the GNPS and cosine methods,
#'     input spectra should be sanitized (unique, well-separated m/z values;
#'     no NaN; sorted by m/z). This is automatically done by
#'     [import_spectra()] with `sanitize = TRUE`. If unsanitized spectra are
#'     detected (duplicate m/z within tolerance), they are sanitized on the fly
#'     with a warning.
#'
#' @include c_wrappers.R
#' @include sanitize_spectrum_matrix.R
#' @include validations_utils.R
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
  # ---- Input Validation (centralized helpers) ----
  assert_choice(method, VALID_SIMILARITY_METHODS, "method")
  if (!is.matrix(query_spectrum) || !is.matrix(target_spectrum)) {
    stop("Spectra must be matrices", call. = FALSE)
  }
  if (ncol(query_spectrum) < 2L || ncol(target_spectrum) < 2L) {
    stop("Spectra must have at least 2 columns (mz, intensity)", call. = FALSE)
  }
  assert_scalar_numeric(dalton, "dalton", min = 0)
  assert_scalar_numeric(ppm, "ppm", min = 0)
  assert_flag(return_matched_peaks, "return_matched_peaks")
  # Early exit for empty spectra
  if (nrow(query_spectrum) == 0L || nrow(target_spectrum) == 0L) {
    return(
      if (return_matched_peaks) {
        list(score = 0.0, matches = 0L)
      } else {
        0.0
      }
    )
  }

  # Sanity check: ensure spectra are sanitized (unique m/z, sorted, no NaN)
  # The C scoring functions assume well-separated peaks. Unsanitized input
  # (e.g. duplicate m/z values) produces mathematically incorrect scores.
  # Uses a package-level env to warn only once per session, not per call.
  tryCatch(
    {
      if (
        !is_spectrum_sanitized(query_spectrum, tolerance = dalton, ppm = ppm)
      ) {
        if (!isTRUE(.tima_sanitize_env$warned_query)) {
          .tima_sanitize_env$warned_query <- TRUE
          warning(
            "Unsanitized query spectrum detected (duplicate/unsorted m/z or NaN). ",
            "Auto-fixing on the fly. For best performance, use ",
            "import_spectra(sanitize = TRUE) upstream.",
            call. = FALSE
          )
        }
        query_spectrum <- sanitize_spectrum_matrix(
          query_spectrum,
          tolerance = dalton,
          ppm = ppm
        )
      }
      if (
        !is_spectrum_sanitized(target_spectrum, tolerance = dalton, ppm = ppm)
      ) {
        if (!isTRUE(.tima_sanitize_env$warned_target)) {
          .tima_sanitize_env$warned_target <- TRUE
          warning(
            "Unsanitized target spectrum detected (duplicate/unsorted m/z or NaN). ",
            "Auto-fixing on the fly. For best performance, use ",
            "import_spectra(sanitize = TRUE) upstream.",
            call. = FALSE
          )
        }
        target_spectrum <- sanitize_spectrum_matrix(
          target_spectrum,
          tolerance = dalton,
          ppm = ppm
        )
      }
    },
    error = function(e) {
      warning(
        "Spectrum sanitization check failed: ",
        conditionMessage(e),
        ". Proceeding with original spectra.",
        call. = FALSE
      )
    }
  )

  # Re-check after sanitization (may have become empty)
  if (nrow(query_spectrum) == 0L || nrow(target_spectrum) == 0L) {
    return(
      if (return_matched_peaks) {
        list(score = 0.0, matches = 0L)
      } else {
        0.0
      }
    )
  }

  # Method: Entropy (no peak matching required) ----

  if (method == "entropy") {
    # if (return_matched_peaks) {
    #   log_warn(
    #     "return_matched_peaks not supported with entropy method, ignoring"
    #   )
    # }

    result <- tryCatch(
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
        log_warn("Entropy calculation failed: %s", e$message)
        return(
          if (return_matched_peaks) {
            list(score = 0.0, matches = 0L)
          } else {
            0.0
          }
        )
      }
    )
    return(result)
  }

  # Methods: GNPS or Cosine (require peak matching) ----

  # Extract masses efficiently (direct column access)
  query_masses <- query_spectrum[, 1L]
  target_masses <- target_spectrum[, 1L]

  # Get peak matching map using optimized C implementation for GNPS
  map <- tryCatch(
    switch(
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
        ...
      )
    ),
    error = function(e) {
      log_warn("Peak matching failed: %s", e$message)
      list(integer(0L), integer(0L))
    }
  )

  matched_x <- map[[1L]]
  matched_y <- map[[2L]]

  # Early exit if no matches at all
  if (length(matched_x) == 0L || all(is.na(matched_x) | is.na(matched_y))) {
    return(
      if (return_matched_peaks) list(score = 0.0, matches = 0L) else 0.0
    )
  }

  # Deduplicate: precursor-shift pass can produce pairs already in direct pass
  # (keep NAs — they carry unmatched peaks needed for correct denominators)
  both_valid <- !is.na(matched_x) & !is.na(matched_y)
  dups <- both_valid & duplicated(data.frame(x = matched_x, y = matched_y))
  matched_x <- matched_x[!dups]
  matched_y <- matched_y[!dups]

  # Build aligned matrices WITH NAs for unmatched peaks.
  # gnps() needs ALL peaks (matched + unmatched) to compute the correct
  # intensity-sum denominators: sqrt(int_i)/sqrt(sum_ALL_unique_int).
  # Stripping NAs here would inflate scores.
  x_mat <- query_spectrum[matched_x, , drop = FALSE]
  y_mat <- target_spectrum[matched_y, , drop = FALSE]

  # Calculate similarity using optimized C GNPS algorithm
  result <- tryCatch(
    gnps_wrapper(x = x_mat, y = y_mat),
    error = function(e) {
      log_warn("Similarity calculation failed: %s", e$message)
      list(score = 0.0, matches = 0L)
    }
  )

  # Return appropriate format
  if (return_matched_peaks) {
    result
  } else {
    result$score
  }
}
