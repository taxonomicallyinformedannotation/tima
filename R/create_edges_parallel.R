utils::globalVariables(
  c(
    "params"
  )
)

#' @title Create edges parallel
#'
#' @description This function is slow so it had to be parallelized
#'
#' @include calculate_entropy.R
#'
#' @param index Indices
#' @param frags Fragments
#' @param precs Precursors
#' @param nspecs Number of spectra
#' @param ms2_tolerance MS2 tolerance
#' @param ppm_tolerance ppm tolerance
#' @param p Progressor
#' @param parallel Parallel
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
create_edges_parallel <- function(index,
                                  frags,
                                  precs,
                                  nspecs,
                                  ms2_tolerance,
                                  ppm_tolerance,
                                  p = NA,
                                  parallel) {
  if (parallel) {
    p(sprintf("spectra=%g", nspecs))
  }
  # Calculate the similarity using lapply
  inner_list <- lapply(X = (index + 1):nspecs, FUN = function(target) {
    calculate_entropy(index, target, frags, ms2_tolerance, ppm_tolerance)
  })

  return(inner_list)
}
