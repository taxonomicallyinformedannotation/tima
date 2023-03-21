#' @title Create edges spectra
#'
#' @description This function create edges based on fragmentation spectra similarity
#'
#' @param input Query file containing spectra. Currently an '.mgf' file
#' @param output Output file.
#' @param name_source Name of the source features column
#' @param name_target Name of the target features column
#' @param method Method to be used to perform spectral comparison
#' @param threshold Minimal similarity to report
#' @param ppm Relative ppm tolerance to be used
#' @param dalton Absolute Dalton tolerance to be used
#' @param npeaks Absolute minimum number of peaks to be matched
#' @param rpeaks Relative minimum number of peaks to be matched
#' @param condition Condition to be fulfilled. Either 'OR' or 'AND' (mass and peaks minima).
#' @param qutoff Intensity under which ms2 fragments will be removed previous to comparison.
#' @param parallel Boolean. Process in parallel
#' @param fast Boolean. Do it fast
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
create_edges_spectra <- function(input = params$files$spectral$raw,
                                 output = params$files$networks$spectral$edges$raw,
                                 name_source = params$names$source,
                                 name_target = params$names$target,
                                 method = params$annotations$ms2$method,
                                 threshold = params$annotations$ms2$thresholds$similarity,
                                 ppm = params$ms$tolerances$mass$ppm$ms2,
                                 dalton = params$ms$tolerances$mass$dalton$ms2,
                                 npeaks = params$annotations$ms2$thresholds$peaks$absolute,
                                 rpeaks = params$annotations$ms2$thresholds$peaks$ratio,
                                 condition = params$annotations$ms2$thresholds$condition,
                                 qutoff = params$ms$intensity$thresholds$ms2,
                                 parallel = params$options$parallel,
                                 fast = params$options$fast,
                                 parameters = params) {
  stopifnot("Your input file does not exist." = file.exists(input))
  stopifnot(
    "Your similarity is not supported, supported similarities are 'gnps', 'navdist', 'ndotproduct', 'neuclidean', 'nspectraangle'" = method %in%
      c(
        "gnps",
        "navdist",
        "ndotproduct",
        "neuclidean",
        "nspectraangle"
      )
  )

  ## Not checking for ppm and Da limits, everyone is free.

  params <<- parameters

  par <- if (parallel) {
    BiocParallel::MulticoreParam()
  } else {
    BiocParallel::SerialParam()
  }

  log_debug("Loading spectra...")
  spectra <- input |>
    import_spectra()

  sim_fun <- switch(
    EXPR = method,
    "gnps" = MsCoreUtils::gnps,
    "navdist" = MsCoreUtils::navdist,
    "ndotproduct" = MsCoreUtils::ndotproduct,
    "neuclidean" = MsCoreUtils::neuclidean,
    "nspectraangle" = MsCoreUtils::nspectraangle
  )

  if (fast) {
    params_sim <- MetaboAnnotation::CompareSpectraParam(
      ppm = ppm,
      tolerance = dalton,
      MAPFUN = Spectra::joinPeaksGnps,
      FUN = sim_fun,
      requirePrecursor = FALSE,
      THRESHFUN = function(x) {
        which(x >= threshold)
      },
      BPPARAM = par
    )
  } else {
    params_sim <- MetaboAnnotation::MatchForwardReverseParam(
      ppm = ppm,
      tolerance = dalton,
      MAPFUN = Spectra::joinPeaksGnps,
      FUN = sim_fun,
      requirePrecursor = FALSE,
      THRESHFUN = function(x) {
        which(x >= threshold)
      },
      BPPARAM = par
    )
  }

  ## COMMENT (AR): TODO Maybe implement some safety sanitization of the spectra?
  ## Can be very slow otherwise

  log_debug("Applying initial intensity filter to query spectra")
  spectra <- spectra |>
    Spectra::filterIntensity(intensity = c(qutoff, Inf))

  log_debug("Performing spectral comparison")
  log_debug(
    "If you do not need the number/ratio of matched peaks,
    computation can be much faster by setting the parameter fast to TRUE"
  )
  log_debug(
    "If you need it, the score threshold greatly impacts speed.
    A higher threshold will lead to faster results."
  )

  matches_sim <- MetaboAnnotation::matchSpectra(
    query = spectra,
    target = spectra,
    param = params_sim
  )

  edges <- MetaboAnnotation::matchedData(matches_sim) |>
    data.frame() |>
    dplyr::filter(acquisitionNum != target_acquisitionNum) |>
    dplyr::select(
      !!as.name(name_source) := "acquisitionNum",
      !!as.name(name_target) := "target_acquisitionNum",
      dplyr::everything()
    )

  if (!fast) {
    if (condition == "AND") {
      edges <- edges |>
        dplyr::filter(score >= threshold &
          matched_peaks_count >= npeaks &
          presence_ratio >= rpeaks)
    } else {
      edges <- edges |>
        dplyr::filter(score >= threshold |
          matched_peaks_count >= npeaks |
          presence_ratio >= rpeaks)
    }
  }

  edges <- edges |>
    dplyr::select(dplyr::any_of(c(
      name_source,
      name_target,
      "score",
      "reverse_score",
      "presence_ratio",
      "matched_peaks_count"
    )))

  export_params(step = "create_edges_spectra")
  export_output(x = edges, file = output[[1]])

  return(output[[1]])
}
