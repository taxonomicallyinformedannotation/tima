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

  inner_list <- list()

  for (target in (index + 1):nspecs) {
    score <-
      msentropy::calculate_entropy_similarity(
        frags[[index]],
        frags[[target]],
        min_mz = 0,
        max_mz = 5000,
        noise_threshold = 0,
        ms2_tolerance_in_da = ms2_tolerance,
        ms2_tolerance_in_ppm = ppm_tolerance,
        max_peak_num = -1,
        clean_spectra = TRUE
      )

    ## Can be very large otherwise
    inner_list[[target - index]] <- if (score >= 0.1) {
      list(
        "feature_id" = index,
        "target_id" = target,
        "score" = as.numeric(score),
        "count_peaks_matched" = NA_integer_,
        "presence_ratio" = NA_real_
      )
    }
  }

  return(inner_list)
}
