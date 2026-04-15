#' @title Transform SIRIUS CSI score
#'
#' @description This function transforms SIRIUS CSI (Compound Structure Identification)
#'     scores using a sigmoid function. The transformation maps raw scores to a
#'     0-1 range for better interpretability.
#'
#' @details This is an experimental transformation not officially approved by
#'     SIRIUS developers. The sigmoid function is: 1 / (1 + exp(-(score + K) / scale))
#'
#'     SIRIUS CSI:FingerID scores are expected to be log-likelihood-like values
#'     on (-Inf, 0], where values closer to 0 are better. A practical rule of
#'     thumb is:
#'     \itemize{
#'       \item score > -10: excellent/awesome
#'       \item score > -100: acceptable/okay
#'       \item score <= -100: weak/low confidence
#'     }
#'
#'     The defaults K = 100 and scale = 20 place the sigmoid midpoint at
#'     score = -100 and strongly reward scores near 0:
#'     \itemize{
#'       \item score = -10  -> ~0.989
#'       \item score = -100 -> 0.500
#'       \item score = -200 -> ~0.007
#'     }
#'     Previous defaults (K = 50, scale = 10) placed the midpoint at -50 and
#'     compressed the useful range to [-70, -30], mapping most realistic SIRIUS
#'     hits to near-zero scores.
#'
#' @include validations_utils.R
#'
#' @param csi_score [numeric] Numeric SIRIUS CSI score (expected mostly <= 0;
#'     can be negative, NA, NULL, or absent)
#' @param K [numeric] Numeric shift parameter to adjust the sigmoid center
#'     (default: 100, midpoint at score = -100)
#' @param scale [numeric] Numeric scale parameter controlling sigmoid steepness
#'     (default: 20)
#'
#' @return Numeric transformed score in the range (0, 1), or NA if input is NA/NULL/absent
#'
#' @examples
#' \dontrun{
#' # Transform a single score
#' transform_score_sirius_csi(csi_score = -100)
#'
#' # Transform with custom parameters
#' transform_score_sirius_csi(csi_score = -100, K = 100, scale = 20)
#'
#' # Vectorized transformation
#' scores <- c(-300, -100, -10, -1, 0)
#' transform_score_sirius_csi(csi_score = scores)
#'
#' # Handle NA values
#' scores_with_na <- c(-100, NA, -10, -300)
#' transform_score_sirius_csi(csi_score = scores_with_na)
#'
#' # Handle missing/absent score
#' transform_score_sirius_csi()
#' }
transform_score_sirius_csi <- function(csi_score = NULL, K = 100, scale = 20) {
  # Handle NULL input
  if (is.null(csi_score)) {
    return(NA_real_)
  }

  # Validate inputs
  if (!is.numeric(csi_score)) {
    cli::cli_abort(
      "csi_score must be numeric, NA, or NULL",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  assert_scalar_numeric(K, "K")
  assert_scalar_numeric(scale, "scale", min = .Machine$double.eps)

  # Apply sigmoid transformation: sigmoid((score + K) / scale)
  # This maps the shifted and scaled score to (0, 1)
  # NA values are preserved automatically
  shifted_score <- as.numeric(csi_score) + K
  transformed_score <- 1 / (1 + exp(-shifted_score / scale))

  transformed_score
}
