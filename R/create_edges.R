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
  p <- progressr::progressor(along = (index + 1):nspecs)
  # Calculate the similarity
  inner_list <- (index + 1):nspecs |>
    furrr::future_map(
      .f = function(target) {
        p()
        tima:::calculate_entropy(
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
