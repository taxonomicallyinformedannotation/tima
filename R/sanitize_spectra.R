#' @title Sanitize spectra
#'
#' @description This function sanitizes MS spectra by removing noise, artifacts,
#'     precursor peaks, and empty spectra. It applies multiple cleaning steps
#'     including intensity filtering, peak reduction, and normalization.
#'
#' @param spectra Spectra object from the Spectra package
#' @param cutoff Numeric absolute minimal intensity threshold (default: NULL).
#'     Peaks below this intensity are removed. If NULL, a dynamic threshold
#'     based on each spectrum's intensity distribution will be used.
#' @param dalton Numeric Dalton tolerance for peak matching (default: 0.01)
#' @param ppm Numeric PPM tolerance for peak matching (default: 10)
#'
#' @return A sanitized Spectra object with noise and artifacts removed
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' data.frame(
#'   FEATURE_ID = c("FT001", "FT002", "FT003"),
#'   mz = c(list(123.4567, 234.5678, 345.6789))
#' ) |>
#'   Spectra::Spectra() |>
#'   sanitize_spectra()
#' }
sanitize_spectra <- function(
  spectra,
  cutoff = NULL,
  dalton = 0.01,
  ppm = 10
) {
  # Input Validation ----

  # Validate spectra object first
  if (!inherits(spectra, "Spectra")) {
    stop("Input must be a Spectra object from the Spectra package")
  }

  # Validate numeric parameters
  if (!is.null(cutoff) && (!is.numeric(cutoff) || cutoff < 0)) {
    stop("Cutoff intensity must be non-negative or NULL, got: ", cutoff)
  }

  if (!is.numeric(dalton) || dalton <= 0 || !is.numeric(ppm) || ppm <= 0) {
    stop(
      "Tolerance values must be positive (dalton: ",
      dalton,
      ", ppm: ",
      ppm,
      ")"
    )
  }

  n_initial <- length(spectra)

  # Early exit for empty spectra
  if (n_initial == 0L) {
    log_warn("No spectra to sanitize")
    return(spectra)
  }

  log_info(
    "Sanitizing %d spectra (cutoff: %s)",
    n_initial,
    ifelse(is.null(cutoff), "dynamic", as.character(cutoff))
  )

  # Dynamic Cutoff Calculation ----

  # If cutoff is NULL, calculate dynamic threshold for each spectrum
  if (is.null(cutoff)) {
    log_debug("Calculating dynamic intensity thresholds")

    peaks_data <- spectra@backend@peaksData
    dynamic_thresholds <- vapply(
      peaks_data,
      function(peak_matrix) {
        if (is.null(peak_matrix) || nrow(peak_matrix) == 0L) {
          return(0)
        }

        intensities <- peak_matrix[, 2L]

        # Handle NaN values - filter them out for threshold calculation
        intensities <- intensities[!is.nan(intensities)]

        if (length(intensities) == 0L) {
          return(0)
        }

        # Calculate median absolute deviation (MAD) based threshold
        med_intensity <- median(intensities, na.rm = TRUE)
        mad_intensity <- median(abs(intensities - med_intensity), na.rm = TRUE)

        # Threshold: median - 2*MAD (robust outlier detection)
        # If MAD is 0 or NA, fall back to a percentile-based approach
        if (!is.na(mad_intensity) && mad_intensity > 0) {
          threshold <- max(0, med_intensity - 2 * mad_intensity)
        } else {
          # Use 5th percentile as threshold if MAD is 0
          threshold <- quantile(intensities, 0.05, names = FALSE, na.rm = TRUE)
        }

        return(threshold)
      },
      numeric(1L),
      USE.NAMES = FALSE
    )

    # Store thresholds for later use
    dynamic_cutoff_used <- TRUE
  } else {
    dynamic_cutoff_used <- FALSE
  }

  # Apply Sanitization Steps ----

  n_before <- length(spectra)

  # CRITICAL: Remove NaN values BEFORE processing pipeline
  # The Spectra pipeline functions cannot handle NaN
  log_debug("Pre-filtering NaN values from peak matrices")
  peaks_data <- spectra@backend@peaksData
  peaks_data_clean <- lapply(peaks_data, function(peak_matrix) {
    if (is.null(peak_matrix) || nrow(peak_matrix) == 0L) {
      return(peak_matrix)
    }

    # Remove rows with any NaN values
    has_nan <- is.nan(peak_matrix[, 1]) | is.nan(peak_matrix[, 2])
    if (any(has_nan)) {
      peak_matrix <- peak_matrix[!has_nan, , drop = FALSE]
    }

    peak_matrix
  })
  spectra@backend@peaksData <- peaks_data_clean

  spectra <- spectra |>
    Spectra::dropNaSpectraVariables() |>
    Spectra::reduceSpectra(tolerance = dalton, ppm = ppm) |>
    Spectra::filterFourierTransformArtefacts() |>
    Spectra::filterPrecursorPeaks(
      tolerance = dalton,
      ppm = ppm,
      mz = c(">=")
    ) |>
    Spectra::combinePeaks(tolerance = dalton, ppm = ppm)

  # Apply dynamic or fixed intensity filter
  if (dynamic_cutoff_used) {
    log_debug("Applying dynamic intensity thresholds")
    peaks_data <- spectra@backend@peaksData

    filtered_peaks <- lapply(seq_along(peaks_data), function(i) {
      peak_matrix <- peaks_data[[i]]
      if (is.null(peak_matrix) || nrow(peak_matrix) == 0L) {
        return(peak_matrix)
      }

      threshold <- dynamic_thresholds[i]
      keep_idx <- peak_matrix[, 2L] >= threshold
      return(peak_matrix[keep_idx, , drop = FALSE])
    })

    spectra@backend@peaksData <- filtered_peaks
  } else {
    spectra <- spectra |>
      Spectra::filterIntensity(intensity = c(cutoff, Inf))
  }

  # Scale peaks (sum of intensities = 1)
  spectra <- spectra |>
    Spectra::scalePeaks() |>
    Spectra::applyProcessing()

  # Low Noise Removal ----

  # Only apply to spectra with sufficient peaks
  n_before_noise <- length(spectra)

  peaks_data <- spectra@backend@peaksData
  cleaned_peaks <- lapply(peaks_data, function(peak_matrix) {
    if (is.null(peak_matrix) || nrow(peak_matrix) == 0L) {
      return(peak_matrix)
    }

    intensities <- peak_matrix[, 2L]
    n_peaks <- length(intensities)

    # Only apply filter if spectrum has enough peaks (10L)
    if (n_peaks < 10L) {
      return(peak_matrix)
    }

    # Get the 10 smallest unique intensities
    smallest_10 <- sort(unique(intensities))[
      1:min(10L, length(unique(intensities)))
    ]

    # Count peaks at each of these intensities
    for (intensity_val in smallest_10) {
      n_at_intensity <- sum(intensities == intensity_val)

      # If more than 5 peaks have this intensity, remove them
      if (n_at_intensity > 5L) {
        keep_idx <- intensities != intensity_val
        peak_matrix <- peak_matrix[keep_idx, , drop = FALSE]
        intensities <- intensities[keep_idx]
      }
    }

    # Rescale intensities so sum = 1 if any peaks remain
    if (nrow(peak_matrix) > 0L) {
      total_intensity <- sum(peak_matrix[, 2L])
      if (total_intensity > 0) {
        peak_matrix[, 2L] <- peak_matrix[, 2L] / total_intensity
      }
    }

    return(peak_matrix)
  })

  spectra@backend@peaksData <- cleaned_peaks

  n_affected_noise <- sum(vapply(
    seq_along(peaks_data),
    function(i) {
      old_n <- nrow(peaks_data[[i]])
      new_n <- nrow(cleaned_peaks[[i]])
      if (is.null(old_n)) {
        old_n <- 0L
      }
      if (is.null(new_n)) {
        new_n <- 0L
      }
      return(old_n != new_n)
    },
    logical(1L)
  ))

  if (n_affected_noise > 0L) {
    pct_affected <- round(100 * n_affected_noise / n_before_noise, 1)
    log_debug(
      "Low noise removal: affected %d spectra (%s%%)",
      n_affected_noise,
      pct_affected
    )
  }

  # Post-Processing Filters ----

  # Filter spectra with fewer than 3 peaks
  spectra <- spectra[lengths(spectra@backend@peaksData) > 4L]
  n_removed_peaks <- n_before - length(spectra)
  if (n_removed_peaks > 0L) {
    pct_removed <- round(100 * n_removed_peaks / n_before, 1)
    log_debug(
      "Removed %d spectra with <= 2 peaks (%s%%)",
      n_removed_peaks,
      pct_removed
    )
  }

  # Filter spectra containing NaN values (using vapply for type safety)
  # Helper to check if spectrum data contains NaN
  .has_nan_values <- function(x) any(is.nan(x))

  has_nan <- vapply(
    X = spectra@backend@peaksData,
    FUN = .has_nan_values,
    logical(1L),
    USE.NAMES = FALSE
  )
  n_nan <- sum(has_nan)
  if (n_nan > 0L) {
    spectra <- spectra[!has_nan]
    pct_nan <- round(100 * n_nan / n_before, 1)
    log_debug(
      "Removed %d spectra containing NaN values (%s%%)",
      n_nan,
      pct_nan
    )
  }

  # Filter NULL spectra (workaround for edge cases)
  has_null <- vapply(
    X = spectra@backend@peaksData,
    FUN = is.null,
    logical(1L),
    USE.NAMES = FALSE
  )
  n_null <- sum(has_null)
  if (n_null > 0L) {
    spectra <- spectra[!has_null]
    pct_null <- round(100 * n_null / n_before, 1)
    log_debug(
      "Removed %d NULL spectra (%s%%)",
      n_null,
      pct_null
    )
  }

  # Final Summary ----

  n_final <- length(spectra)
  n_total_removed <- n_initial - n_final
  pct_retained <- round(100 * n_final / n_initial, 1)

  log_info(
    "Sanitization complete: %d/%d spectra retained (%s%%, %d removed)",
    n_final,
    n_initial,
    pct_retained,
    n_total_removed
  )

  return(spectra)
}
