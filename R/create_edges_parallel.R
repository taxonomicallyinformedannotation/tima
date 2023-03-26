#' @title Create edges parallel
#'
#' @description This function part of the creation of edges
#'
#' @param x Indices
#' @param frags Fragments
#' @param precs Precursors
#' @param nspecs Number of spectra
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
create_edges_parallel <- function(x,
                                  frags = fragments_norm,
                                  precs = precursors,
                                  nspecs = nspe) {
  lapply(
    X = (x + 1):nspecs,
    FUN = create_edges_progress,
    query = x,
    s1 = cbind(mz = frags[[x]][, 1], intensity = frags[[x]][, 2]),
    fragments = frags,
    precursors = precs
  )
}
