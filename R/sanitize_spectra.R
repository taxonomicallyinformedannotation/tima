#' @title Sanitize spectra
#'
#' @description This function sanitizes MS spectra by removing noise, artifacts,
#'     precursor peaks, and empty spectra. It applies multiple cleaning steps
#'     including intensity filtering, peak reduction, and normalization.
#'
#' @param spectra [Spectra] Spectra object from the Spectra package
#' @param cutoff [numeric] Absolute minimal intensity threshold (default: NULL).
#'     Peaks below this intensity are removed. If NULL, a dynamic threshold
#'     based on each spectrum's intensity distribution will be used.
#' @param dalton [numeric] Dalton tolerance for peak matching (default: 0.01)
#' @param min_fragments [integer] Minimum number of fragment peaks a spectrum
#'     must have after cleaning to be retained (default: 1). Spectra with
#'     fewer peaks are discarded.
#' @param ppm [numeric] PPM tolerance for peak matching (default: 10)
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
  min_fragments = 1L,
  ppm = 10
) {
  # TODO(M6): Replace @backend@peaksData slot access with Spectra::peaksData()

  # accessor where possible. Write-back has no clean API equivalent yet.
  if (!inherits(spectra, "Spectra")) {
    cli::cli_abort(
      "input must be a Spectra object from the Spectra package",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  if (!is.null(cutoff) && (!is.numeric(cutoff) || cutoff < 0)) {
    cli::cli_abort(
      "cutoff intensity must be non-negative or NULL, got {.val {cutoff}}",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  if (!is.numeric(dalton) || dalton <= 0 || !is.numeric(ppm) || ppm <= 0) {
    cli::cli_abort(
      "tolerance values must be positive (dalton: {.val {dalton}}, ppm: {.val {ppm}})",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  min_fragments <- as.integer(min_fragments)
  if (is.na(min_fragments) || min_fragments < 1L) {
    cli::cli_abort(
      "min_fragments must be a positive integer, got {.val {min_fragments}}",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  n_initial <- length(spectra)
  if (n_initial == 0L) {
    log_warn("No spectra to sanitize")
    return(spectra)
  }

  log_info(
    "Sanitizing %d spectra (cutoff: %s)",
    n_initial,
    ifelse(is.null(cutoff), "dynamic", as.character(cutoff))
  )

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
        intensities <- intensities[!is.nan(intensities)]
        if (length(intensities) == 0L) {
          return(0)
        }
        med_intensity <- median(intensities, na.rm = TRUE)
        mad_intensity <- median(abs(intensities - med_intensity), na.rm = TRUE)
        if (!is.na(mad_intensity) && mad_intensity > 0) {
          threshold <- max(0, med_intensity - 2 * mad_intensity)
        } else {
          threshold <- quantile(intensities, 0.05, names = FALSE, na.rm = TRUE)
        }
        threshold
      },
      numeric(1L),
      USE.NAMES = FALSE
    )
    dynamic_cutoff_used <- TRUE
  } else {
    dynamic_cutoff_used <- FALSE
  }

  n_before <- length(spectra)

  log_debug("Pre-filtering NaN values from peak matrices")
  peaks_data <- spectra@backend@peaksData
  peaks_data_clean <- lapply(peaks_data, function(peak_matrix) {
    if (is.null(peak_matrix) || nrow(peak_matrix) == 0L) {
      return(peak_matrix)
    }
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
      mz = ">="
    ) |>
    Spectra::combinePeaks(tolerance = dalton, ppm = ppm)

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
      peak_matrix[keep_idx, , drop = FALSE]
    })
    spectra@backend@peaksData <- filtered_peaks
  } else {
    spectra <- spectra |>
      Spectra::filterIntensity(intensity = c(cutoff, Inf))
  }

  spectra <- spectra |>
    Spectra::scalePeaks() |>
    Spectra::applyProcessing()

  n_before_noise <- length(spectra)
  peaks_data <- spectra@backend@peaksData
  cleaned_peaks <- lapply(peaks_data, function(peak_matrix) {
    if (is.null(peak_matrix) || nrow(peak_matrix) == 0L) {
      return(peak_matrix)
    }

    intensities <- peak_matrix[, 2L]
    n_peaks <- length(intensities)
    if (n_peaks < 10L) {
      return(peak_matrix)
    }

    smallest_10 <- sort(unique(intensities))[
      seq_len(min(10L, length(unique(intensities))))
    ]

    # Remove intensity values that appear more than 5 times (noise artifacts)
    noise_vals <- smallest_10[
      vapply(smallest_10, function(v) sum(intensities == v) > 5L, logical(1L))
    ]
    if (length(noise_vals) > 0L) {
      keep_idx <- !intensities %in% noise_vals
      peak_matrix <- peak_matrix[keep_idx, , drop = FALSE]
      intensities <- intensities[keep_idx]
    }

    if (nrow(peak_matrix) > 0L) {
      total_intensity <- sum(peak_matrix[, 2L])
      if (total_intensity > 0) {
        peak_matrix[, 2L] <- peak_matrix[, 2L] / total_intensity
      }
    }

    peak_matrix
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
      old_n != new_n
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

  n_peaks <- vapply(
    spectra@backend@peaksData,
    function(x) if (is.null(x)) 0L else nrow(x),
    integer(1L),
    USE.NAMES = FALSE
  )
  spectra <- spectra[n_peaks >= min_fragments]
  n_removed_peaks <- n_before - length(spectra)
  if (n_removed_peaks > 0L) {
    pct_removed <- round(100 * n_removed_peaks / n_before, 1)
    log_debug(
      "Removed %d spectra with < %d fragments (%s%%)",
      n_removed_peaks,
      min_fragments,
      pct_removed
    )
  }

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

  spectra
}
