#' @title Transform SIRIUS CSI score
#'
#' @description This function transforms SIRIUS CSI (Compound Structure Identification)
#'     scores using a sigmoid function. The transformation maps raw scores to a
#'     0-1 range for better interpretability.
#'
#' @details This is an experimental transformation not officially approved by
#'     SIRIUS developers. The sigmoid function is: 1 / (1 + exp(-(score + K) / scale))
#'
#' @param csi_score Numeric SIRIUS CSI score (can be negative)
#' @param K Numeric shift parameter to adjust the sigmoid center (default: 50)
#' @param scale Numeric scale parameter controlling sigmoid steepness (default: 10)
#'
#' @return Numeric transformed score in the range (0, 1)
#'
#' @examples
#' # Transform a single score
#' transform_score_sirius_csi(csi_score = -20)
#'
#' # Transform with custom parameters
#' transform_score_sirius_csi(csi_score = -20, K = 30, scale = 5)
#'
#' # Vectorized transformation
#' scores <- c(-50, -20, 0, 20, 50)
#' transform_score_sirius_csi(csi_score = scores)
transform_score_sirius_csi <- function(csi_score, K = 50, scale = 10) {
  # Validate inputs
  if (!is.numeric(csi_score)) {
    stop("csi_score must be numeric")
  }

  if (scale <= 0) {
    stop("scale parameter must be positive")
  }

  # Apply sigmoid transformation: sigmoid((score + K) / scale)
  # This maps the shifted and scaled score to (0, 1)
  shifted_score <- as.numeric(csi_score) + K
  transformed_score <- 1 / (1 + exp(-shifted_score / scale))

  return(transformed_score)
}
