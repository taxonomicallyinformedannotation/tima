#' @title Create edges progress
#'
#' @description This function is slow so it outputs the progression of the creation of edges
#'
#' @param xs Indices of spectra
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
create_edges_progress <- function(xs) {
  ## Originally written with future but too slow...TODO investigate
  pbmcapply::pbmclapply(
    X = xs,
    mc.cores = parallelly::availableCores(),
    ignore.interactive = TRUE,
    FUN = function(x,
                   frags = fragments_norm,
                   precs = precursors,
                   nspecs = nspe) {
      lapply(
        X = (x + 1):nspecs,
        FUN = create_edges_sub
      )
    }
  )
}
