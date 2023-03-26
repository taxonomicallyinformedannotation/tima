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
create_edges_parallel <- function(index,
                                  frags,
                                  precs,
                                  nspecs) {
  list <- lapply(
    X = (index + 1):nspecs,
    FUN = create_edges_progress,
    query = index,
    s1 = cbind(mz = frags[[index]][, 1], intensity = frags[[index]][, 2]),
    fragments = frags,
    precursors = precs
  )
  return(list)
}
