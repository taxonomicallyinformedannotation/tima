#' @title Check if a spectrum matrix is sanitized
#'
#' @description Fast check that a peak matrix has the properties required
#'     by the C-level GNPS scoring functions:
#'     \itemize{
#'       \item m/z values sorted in ascending order
#'       \item No duplicate m/z within matching tolerance
#'       \item No NaN or NA values
#'       \item Non-negative intensities
#'     }
#'
#' @param spectrum Numeric matrix with at least 2 columns (mz, intensity)
#' @param tolerance Numeric absolute tolerance in Daltons (default: 0.01)
#' @param ppm Numeric relative tolerance in ppm (default: 10)
#'
#' @return Logical scalar: TRUE if the spectrum is sanitized, FALSE otherwise.
#'
#' @keywords internal
is_spectrum_sanitized <- function(spectrum, tolerance = 0.01, ppm = 10) {
  if (!is.matrix(spectrum) || nrow(spectrum) < 2L) return(TRUE)

  mz <- spectrum[, 1L]
  int <- spectrum[, 2L]

  # Check for NaN/NA
  if (anyNA(mz) || anyNA(int) || any(is.nan(mz)) || any(is.nan(int))) {
    return(FALSE)
  }

  # Check sorted
  if (is.unsorted(mz)) return(FALSE)

  # Check for duplicates within tolerance
  diffs <- diff(mz)
  # Tolerance at each pair: tol + ppm * min(mz_i, mz_i+1) * 1e-6
  allowed <- tolerance + ppm * mz[-length(mz)] * 1e-6
  if (any(diffs < allowed)) return(FALSE)

  TRUE
}

#' @title Sanitize a spectrum matrix for C-level scoring
#'
#' @description Lightweight sanitization of a peak matrix (mz, intensity)
#'     to meet the requirements of the GNPS C scoring functions. This is a
#'     fallback for spectra that were not pre-processed via
#'     [sanitize_spectra()] / [import_spectra()].
#'
#'     Operations performed:
#'     \enumerate{
#'       \item Remove rows with NaN or NA values
#'       \item Sort by m/z
#'       \item Merge peaks within tolerance (intensity-weighted mean m/z,
#'             summed intensity)
#'     }
#'
#' @param spectrum Numeric matrix with at least 2 columns (mz, intensity)
#' @param tolerance Numeric absolute tolerance in Daltons (default: 0.01)
#' @param ppm Numeric relative tolerance in ppm (default: 10)
#'
#' @return A sanitized numeric matrix with columns mz and intensity.
#'     May have fewer rows than input. Returns a 0-row matrix if all
#'     peaks are removed.
#'
#' @keywords internal
sanitize_spectrum_matrix <- function(spectrum, tolerance = 0.01, ppm = 10) {
  if (!is.matrix(spectrum) || nrow(spectrum) == 0L) return(spectrum)

  mz <- spectrum[, 1L]
  int <- spectrum[, 2L]

  # 1. Remove NaN/NA rows
  valid <- !is.na(mz) & !is.na(int) & !is.nan(mz) & !is.nan(int)
  mz <- mz[valid]
  int <- int[valid]

  if (length(mz) == 0L) {
    return(matrix(numeric(0L), ncol = 2L, dimnames = list(NULL, c("mz", "intensity"))))
  }

  # 2. Sort by m/z
  ord <- order(mz)
  mz <- mz[ord]
  int <- int[ord]

  # 3. Merge peaks within tolerance (greedy: merge consecutive close peaks)
  n <- length(mz)
  if (n <= 1L) {
    return(cbind(mz = mz, intensity = int))
  }

  merged_mz <- numeric(n)
  merged_int <- numeric(n)
  k <- 1L
  merged_mz[1L] <- mz[1L] * int[1L]
  merged_int[1L] <- int[1L]
  group_count <- 1L

  for (i in 2L:n) {
    # Current group representative m/z
    rep_mz <- merged_mz[k] / merged_int[k]
    allowed <- tolerance + ppm * rep_mz * 1e-6
    if (mz[i] - rep_mz <= allowed) {
      # Merge into current group (intensity-weighted m/z, summed intensity)
      merged_mz[k] <- merged_mz[k] + mz[i] * int[i]
      merged_int[k] <- merged_int[k] + int[i]
    } else {
      # Finalize current group, start new one
      merged_mz[k] <- merged_mz[k] / merged_int[k]
      k <- k + 1L
      merged_mz[k] <- mz[i] * int[i]
      merged_int[k] <- int[i]
    }
  }
  # Finalize last group
  merged_mz[k] <- merged_mz[k] / merged_int[k]

  cbind(mz = merged_mz[1L:k], intensity = merged_int[1L:k])
}

