utils::globalVariables(
  c(
    "params"
  )
)

#' @title Create edges progress
#'
#' @description This function is slow so it outputs
#'    the progression of the creation of edges
#'
#' @param target Indices of target spectra
#' @param query Index of query spectra
#' @param s1 Query spectrum
#' @param frags Fragments
#' @param precs Precursors
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
create_edges_progress <- function(target,
                                  query,
                                  s1,
                                  frags,
                                  precs) {
  s2 <-
    cbind(mz = frags[[target]][, 1], intensity = frags[[target]][, 2])
  map <-
    MsCoreUtils::join_gnps(
      x = s1[, 1],
      y = s2[, 1],
      xPrecursorMz = precs[query],
      yPrecursorMz = precs[target],
      tolerance = params$ms$tolerances$mass$dalton$ms2,
      ppm = params$ms$tolerances$mass$ppm$ms2
    )
  matched_peaks_count <- length((map$x * map$y)[!is.na(map$x * map$y)])
  presence_ratio <-
    return(
      data.frame(
        "feature_id" = query,
        "target_id" = target,
        "score" = MsCoreUtils::gnps(s1[map$x, ], s2[map$y, ]),
        "matched_peaks_count" = matched_peaks_count,
        "presence_ratio" = matched_peaks_count / length(map$y)
      )
    )
}
