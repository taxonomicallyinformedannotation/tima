library(future.apply)
library(MsCoreUtils)
library(progressr)

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
  p <- progressor(along = xs)
  future_lapply(
    X = xs,
    FUN = function(x,
                   frags = fragments_norm,
                   precs = precursors,
                   nspecs = nspe) {
      p()
      lapply(
        X = (x + 1):nspecs,
        FUN = function(y,
                       id = x,
                       s1 = cbind(mz = frags[[x]][, 1], intensity = frags[[x]][, 2])) {
          s2 <-
            cbind(mz = frags[[y]][, 1], intensity = frags[[y]][, 2])
          map <-
            join_gnps(
              x = s1[, 1],
              y = s2[, 1],
              xPrecursorMz = precs[id],
              yPrecursorMz = precs[y],
              tolerance = params$ms$tolerances$mass$dalton$ms2,
              ppm = params$ms$tolerances$mass$ppm$ms2
            )
          score <- gnps(s1[map$x, ], s2[map$y, ])
          matched_peaks_count <-
            length((map$x * map$y)[!is.na(map$x * map$y)])
          presence_ratio <- matched_peaks_count / length(map$y)
          return(
            data.frame(
              "feature_id" = id,
              "target_id" = y,
              "score" = score,
              "matched_peaks_count" = matched_peaks_count,
              "presence_ratio" = presence_ratio
            )
          )
        }
      )
    }
  )
}
