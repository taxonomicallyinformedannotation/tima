#' @title Create edges parallel
#'
#' @description This function is part of the creation of edges
#'
#' @include create_edges_progress.R
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
create_edges_parallel <- function(spectra,
                                  ms2_tolerance =
                                    params$ms$tolerances$mass$dalton$ms2,
                                  ppm_tolerance =
                                    params$ms$tolerances$mass$ppm$ms2,
                                  parallel =
                                    params$options$parallel) {
  nspecz <- length(spectra)
  precz <- spectra$precursorMz
  fragz <- spectra@backend@peaksData
  msz <- ms2_tolerance
  ppmz <- ppm_tolerance

  if (parallel) {
    result_list <-
      pbmcapply::pbmclapply(
        X = 1:(nspecz - 1),
        FUN = create_edges_progress,
        mc.cores = parallelly::availableCores(),
        frags = fragz,
        precs = precz,
        nspecs = nspecz,
        ms2_tolerance = msz,
        ppm_tolerance = ppmz
      )
  } else {
    result_list <-
      lapply(
        X = 1:(nspecz - 1),
        FUN = create_edges_progress,
        frags = fragz,
        precs = precz,
        nspecs = nspecz,
        ms2_tolerance = msz,
        ppm_tolerance = ppmz
      )
  }

  # Combine the results into a single matrix
  edges <-
    do.call(rbind, unlist(result_list, recursive = FALSE)) |>
    data.frame()
  return(edges)
}
