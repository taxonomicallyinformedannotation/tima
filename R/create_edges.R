#' @title Create edges
#'
#' @description This function applies similarity calculation to a list of
#'        spectra to create edges
#'
#' @include calculate_entropy.R
#'
#' @param index Indices
#' @param frags Fragments
#' @param precs Precursors
#' @param nspecs Number of spectra
#' @param ms2_tolerance MS2 tolerance
#' @param ppm_tolerance ppm tolerance
#' @param threshold Threshold
#'
#' @return NULL
#'
#' @examples NULL
create_edges <- function(index,
                         frags,
                         precs,
                         nspecs,
                         ms2_tolerance,
                         ppm_tolerance,
                         threshold) {
  # Calculate the similarity using lapply
  inner_list <- (index + 1):nspecs |>
    lapply(
      FUN = function(target) {
        calculate_entropy(
          index,
          target,
          frags,
          ms2_tolerance,
          ppm_tolerance,
          threshold
        )
      }
    ) |>
    tidytable::bind_rows()

  return(inner_list)
}
