## These wrappers are needed for compatibility with the targets package
## to prevent unnecessary re-running of steps that call C code.
## See https://github.com/ropensci/targets/issues/721
## A future improvement could return a hash of the source C file

#' @title Wrapper for the C function "gnps"
#'
#' @description Calculates GNPS-style spectral similarity score between
#'     two matched peak matrices using a chain-DP optimal assignment
#'     algorithm (O(n+m) time).
#'
#'     **Spectra must be sanitized** before calling this function:
#'     no two peaks in the same spectrum should have m/z values close enough
#'     to match each other (i.e., |mz_i - mz_j| > tolerance), sorted by m/z,
#'     no NaN/NA. This is guaranteed by [sanitize_spectra()] via
#'     [import_spectra()]. See [sanitize_spectrum_matrix()] for a
#'     lightweight fallback.
#'
#' @param x Numeric matrix with matched peaks from query spectrum.
#'     Must have columns for mz and intensity.
#' @param y Numeric matrix with matched peaks from target spectrum.
#'     Must have columns for mz and intensity.
#'
#' @return A numeric vector of length 2:
#'   \item{\[1\]}{Numeric similarity score (0-1)}
#'   \item{\[2\]}{Numeric count of matched peaks}
#'
#' @examples
#' \dontrun{
#' # Matched peaks from two spectra
#' x_peaks <- cbind(mz = c(100, 200), intensity = c(50, 100))
#' y_peaks <- cbind(mz = c(100, 200), intensity = c(45, 95))
#' result <- gnps_wrapper(x = x_peaks, y = y_peaks)
#' print(result\[1\])  # score
#' print(result\[2\])  # matches
#' }
#' @keywords internal
gnps_wrapper <- function(x, y) {
  .Call(C_gnps, x = x, y = y)
}

#' @title Wrapper for the C function "join_gnps"
#'
#' @description Performs GNPS-style peak matching between query and target
#'     mass lists with tolerance-based matching and precursor filtering
#'
#' @param x Numeric vector of query m/z values
#' @param y Numeric vector of target m/z values
#' @param xPrecursorMz Numeric precursor m/z for query spectrum
#' @param yPrecursorMz Numeric precursor m/z for target spectrum
#' @param tolerance Numeric value specifying the absolute tolerance in Daltons
#' @param ppm Numeric value specifying the relative tolerance in ppm
#'
#' @return A list with two integer vectors:
#'   \item{indices_x}{Indices of matched peaks in x}
#'   \item{indices_y}{Indices of matched peaks in y}
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' query_mz <- c(100.05, 200.10, 300.15)
#' target_mz <- c(100.06, 200.11, 400.20)
#' matches <- join_gnps_wrapper(
#'   x = query_mz,
#'   y = target_mz,
#'   xPrecursorMz = 500.0,
#'   yPrecursorMz = 500.0,
#'   tolerance = 0.01,
#'   ppm = 10
#' )
#' }
join_gnps_wrapper <- function(
  x,
  y,
  xPrecursorMz,
  yPrecursorMz,
  tolerance,
  ppm
) {
  .Call(
    C_join_gnps,
    x = x,
    y = y,
    xPrecursorMz = xPrecursorMz,
    yPrecursorMz = yPrecursorMz,
    tolerance = tolerance,
    ppm = ppm
  )
}

#' @title Wrapper for the C function "gnps_chain_dp"
#'
#' @description Fused GNPS join + score in a single C call. Performs
#'     peak matching and scoring internally without R-level allocation.
#'     Uses chain-DP optimal assignment (O(n+m) time).
#'
#'     **Spectra must be sanitized** before calling this function:
#'     no two peaks in the same spectrum should have m/z values close enough
#'     to match each other (i.e., |mz_i - mz_j| > tolerance), sorted by m/z,
#'     no NaN/NA. This is guaranteed by [sanitize_spectra()] via
#'     [import_spectra()]. See [sanitize_spectrum_matrix()] for a
#'     lightweight fallback.
#'
#' @param x Numeric matrix with query spectrum peaks (mz, intensity).
#' @param y Numeric matrix with target spectrum peaks (mz, intensity).
#' @param xPrecursorMz Numeric precursor m/z for query spectrum
#' @param yPrecursorMz Numeric precursor m/z for target spectrum
#' @param tolerance Numeric value specifying the absolute tolerance in Daltons
#' @param ppm Numeric value specifying the relative tolerance in ppm
#' @param matchedPeaksCount Logical flag; if `TRUE`, return both score and
#'   matched-peak count, otherwise return score only.
#'
#' @return A numeric vector of length 1 by default (score), or length 2 when
#'   `matchedPeaksCount = TRUE` (`c(score, matched_peaks)`).
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' query <- cbind(mz = c(100.05, 200.10, 300.15), intensity = c(100, 50, 25))
#' target <- cbind(mz = c(100.06, 200.11, 400.20), intensity = c(90, 45, 30))
#' result <- gnps_chain_dp_wrapper(
#'   x = query,
#'   y = target,
#'   xPrecursorMz = 500.0,
#'   yPrecursorMz = 500.0,
#'   tolerance = 0.01,
#'   ppm = 10
#' )
#' print(result\[1\])  # score
#' print(result\[2\])  # matches
#' }
gnps_chain_dp_wrapper <- function(
  x,
  y,
  xPrecursorMz,
  yPrecursorMz,
  tolerance,
  ppm,
  matchedPeaksCount = FALSE
) {
  res <- .Call(
    C_gnps_chain_dp,
    x,
    y,
    xPrecursorMz,
    yPrecursorMz,
    tolerance,
    ppm
  )

  if (matchedPeaksCount) res else res[1L]
}
