#' @title Create edges parallel
#'
#' @description This function part of the creation of edges
#'
#' @include create_edges_progress.R
#'
#' @param index Indices
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
    s1 = frags[[index]],
    frags = frags,
    precs = precs
  ) |>
    tidytable::bind_rows()
  return(list)
}
