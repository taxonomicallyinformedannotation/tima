utils::globalVariables(c(
  "matched_peaks_count",
  "params",
  "presence_ratio",
  "score"
))

#' @title Create edges spectra
#'
#' @description This function create edges
#'    based on fragmentation spectra similarity
#'
#' @include create_edges_parallel.R
#' @include import_spectra.R
#' @include normalize_peaks.R
#' @include remove_above_precursor.R
#' @include sanitize_spectra.R
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
#' @param condition Condition to be fulfilled.
#'    Either 'OR' or 'AND' (mass and peaks minima).
#' @param qutoff Intensity under which ms2 fragments will be removed.
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
    "Your similarity is not supported, supported similarities are
    'gnps', 'navdist', 'ndotproduct', 'neuclidean', 'nspectraangle'" =
      method %in%
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

  if (!fast) {
    log_debug("Loading spectra...")
    spectra <- input |>
      import_spectra() |>
      sanitize_spectra(cutoff = qutoff) |>
      Spectra::addProcessing(remove_above_precursor(),
        spectraVariables = c("precursorMz")
      ) |>
      Spectra::addProcessing(normalize_peaks()) |>
      Spectra::applyProcessing()

    log_debug("Extracting fragments and precursors...")
    fragments_norm <- spectra@backend@peaksData
    precursors <- spectra$precursorMz
    nspe <- length(spectra)

    log_debug("Performing spectral comparison")
    log_debug(
      "As we do not bin the spectra, nor limit the precursors delta, expect a long processing time."
    )
    log_debug("Take yourself a break, you deserve it.")

    ## Originally written with future but too slow...TODO investigate
    matches_sim <- pbmcapply::pbmclapply(
      X = 1:(nspe - 1),
      mc.cores = parallelly::availableCores(),
      ignore.interactive = TRUE,
      FUN = create_edges_parallel,
      frags = fragments_norm,
      precs = precursors,
      nspecs = nspe
    )

    edges <- matches_sim |>
      tidytable::bind_rows()

    ## old version, keep in case
    # par <- if (parallel) {
    #   if (.Platform$OS.type == "windows") {
    #     BiocParallel::SnowParam(progressbar = TRUE)
    #   } else {
    #     BiocParallel::MulticoreParam(progressbar = TRUE)
    #   }
    # } else {
    #   BiocParallel::SerialParam(progressbar = TRUE)
    # }
    # sim_fun <- switch(
    #   EXPR = method,
    #   "gnps" = MsCoreUtils::gnps,
    #   "navdist" = MsCoreUtils::navdist,
    #   "ndotproduct" = MsCoreUtils::ndotproduct,
    #   "neuclidean" = MsCoreUtils::neuclidean,
    #   "nspectraangle" = MsCoreUtils::nspectraangle
    # )
    # params_sim <- MetaboAnnotation::MatchForwardReverseParam(
    #   ppm = ppm,
    #   tolerance = dalton,
    #   MAPFUN = Spectra::joinPeaksGnps,
    #   FUN = sim_fun,
    #   requirePrecursor = FALSE,
    #   THRESHFUN = function(x) {
    #     which(x >= threshold)
    #   },
    #   BPPARAM = par
    # )
    # matches_sim_2 <- MetaboAnnotation::matchSpectra(
    #   query = spectra,
    #   target = spectra,
    #   param = params_sim
    # )
    # edges <- MetaboAnnotation::matchedData(matches_sim_2) |>
    #   data.frame() |>
    #   tidytable::filter(acquisitionNum != target_acquisitionNum) |>
    #   tidytable::select(
    #     !!as.name(name_source) := "acquisitionNum",
    #     !!as.name(name_target) := "target_acquisitionNum",
    #     tidytable::everything()
    #   )

    edges <- edges |>
      tidytable::select(
        !!as.name(name_source) := "feature_id",
        !!as.name(name_target) := "target_id",
        tidytable::everything()
      )

    edges <- edges |>
      tidytable::select(tidytable::any_of(
        c(
          name_source,
          name_target,
          "score",
          "reverse_score",
          "presence_ratio",
          "matched_peaks_count"
        )
      ))
  } else {
    log_debug(
      "The computation of all vs all spectra is very expensive,
      skipping it to get quick results."
    )
    log_debug(
      "Chemical consistency will only be calculated based on the mass edges,
      no spectral edges taken into account."
    )
    log_debug("Pre-computed GNPS edges are a way to go.")
    edges <- data.frame(
      name_source = NA,
      name_target = NA,
      "score" = NA,
      "reverse_score" = NA,
      "presence_ratio" = NA,
      "matched_peaks_count" = NA
    )
  }

  if (condition == "AND") {
    edges <- edges |>
      dplyr::filter(
        score >= threshold,
        matched_peaks_count >= npeaks,
        presence_ratio >= rpeaks
      )
  } else {
    edges <- edges |>
      dplyr::filter(score >= threshold |
        matched_peaks_count >= npeaks |
        presence_ratio >= rpeaks)
  }

  export_params(step = "create_edges_spectra")
  export_output(x = edges, file = output[[1]])

  return(output[[1]])
}
