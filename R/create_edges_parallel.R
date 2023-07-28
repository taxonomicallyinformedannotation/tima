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
    future::plan(future::multisession)
    progressr::handlers(
      list(
        progressr::handler_progress(
          format = ":current/:total [:bar] :percent in :elapsed ETA: :eta"
        )
      )
    )

    result_list <-
      future.apply::future_lapply(
        X = 1:(nspecz - 1),
        FUN = create_edges_progress,
        p = progressr::progressor(along = 1:(nspecz - 1)),
        future.chunk.size = structure(TRUE, ordering = "random"),
        frags = fragz,
        precs = precz,
        nspecs = nspecz,
        ms2_tolerance = msz,
        ppm_tolerance = ppmz,
        parallel = parallel
      ) |>
      progressr::with_progress()
  } else {
    result_list <-
      lapply(
        X = 1:(nspecz - 1),
        FUN = create_edges_progress,
        frags = fragz,
        precs = precz,
        nspecs = nspecz,
        ms2_tolerance = msz,
        ppm_tolerance = ppmz,
        p = NA,
        parallel = parallel
      )
  }

  # Combine the results into a single matrix
  edges <-
    do.call(rbind, unlist(result_list, recursive = FALSE)) |>
    data.frame()
  return(edges)
}
