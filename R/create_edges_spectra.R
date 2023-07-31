utils::globalVariables(c(
  "count_peaks_matched",
  "params",
  "presence_ratio",
  "score"
))

#' @title Create edges spectra
#'
#' @description This function create edges
#'    based on fragmentation spectra similarity
#'
#' @include create_edges.R
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
#' @param qutoff Intensity under which ms2 fragments will be removed.
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
create_edges_spectra <- function(
    input = params$files$spectral$raw,
    output = params$files$networks$spectral$edges$raw,
    name_source = params$names$source,
    name_target = params$names$target,
    threshold = params$annotations$ms2$thresholds$similarity,
    ppm = params$ms$tolerances$mass$ppm$ms2,
    dalton = params$ms$tolerances$mass$dalton$ms2,
    qutoff = params$ms$intensity$thresholds$ms2,
    parameters = params) {
  stopifnot("Your input file does not exist." = file.exists(input))
  ## Not checking for ppm and Da limits, everyone is free.

  params <<- parameters

  log_debug("Loading spectra...")
  spectra <- input |>
    import_spectra() |>
    ## TODO make it a param
    sanitize_spectra(cutoff = qutoff, deeper = TRUE) |>
    Spectra::addProcessing(remove_above_precursor(),
      spectraVariables = c("precursorMz")
    ) |>
    Spectra::addProcessing(normalize_peaks()) |>
    Spectra::applyProcessing()

  log_debug("Performing spectral comparison")
  log_debug(
    "As we do not limit the precursors delta,
      expect a (relatively) long processing time."
  )
  log_debug("Take yourself a break, you deserve it.")
  nspecz <- length(spectra)
  precz <- spectra$precursorMz
  fragz <- spectra@backend@peaksData

  edges <-
    pbapply::pblapply(
      X = 1:(nspecz - 1),
      FUN = create_edges,
      frags = fragz,
      precs = precz,
      nspecs = nspecz,
      ms2_tolerance = dalton,
      ppm_tolerance = ppm,
      threshold = threshold
    ) |>
    dplyr::bind_rows()

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
        "count_peaks_matched"
      )
    ))

  edges <- edges |>
    dplyr::filter(
      score >= threshold
    )

  export_params(step = "create_edges_spectra")
  export_output(x = edges, file = output[[1]])

  return(output[[1]])
}
