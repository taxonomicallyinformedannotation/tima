#' @title Transform score sirius CSI
#'
#' @description This function calculates the mass of M
#'
#' @export
#'
#' @noRd
#'
#' @param csi_score Original CSI score
#' @param K Shift
#' @param scale Scale
#'
#' @return A mass
#'
#' @examples NULL
transform_score_sirius_csi <- function(csi_score,
                                       K = 50,
                                       scale = 10) {
  ## COMMENT: This is a random transform, not approved by anyone
  LL_prime <- as.numeric(csi_score) + K
  return(1 / (1 + exp(-LL_prime * 1 / scale)))
}
