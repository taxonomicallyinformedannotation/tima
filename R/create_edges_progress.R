utils::globalVariables(
  c(
    "params"
  )
)

#' @title Create edges progress
#'
#' @description This function is slow so it had to be parallelized
#'
#' @param index Indices
#' @param frags Fragments
#' @param precs Precursors
#' @param nspecs Number of spectra
#' @param p Progressor
#' @param parallel Parallel
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
create_edges_progress <- function(index,
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

  s1 <- frags[[index]]
  query_prec <- precs[index]

  inner_list <- list()

  for (target in (index + 1):nspecs) {
    s2 <- frags[[target]]
    map <- MsCoreUtils::join_gnps(
      x = s1[, 1],
      y = s2[, 1],
      xPrecursorMz = query_prec,
      yPrecursorMz = precs[target],
      tolerance = ms2_tolerance,
      ppm = ppm_tolerance
    )
    xy_product <- map$x * map$y
    matched_peaks_count <- sum(!is.na(xy_product))

    inner_list[[target - index]] <- list(
      "feature_id" = index,
      "target_id" = target,
      "score" = MsCoreUtils::gnps(s1[map$x, ], s2[map$y, ]),
      "matched_peaks_count" = matched_peaks_count,
      "presence_ratio" = matched_peaks_count / length(map$y)
    )
  }

  return(inner_list)
}
