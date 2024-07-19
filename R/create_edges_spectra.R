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
#' @param threshold Minimal similarity to report
#' @param ppm Relative ppm tolerance to be used
#' @param dalton Absolute Dalton tolerance to be used
#' @param qutoff Intensity under which ms2 fragments will be removed.
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
create_edges_spectra <- function(
    input = get_params(step = "create_edges_spectra")$files$spectral$raw,
    output = get_params(step = "create_edges_spectra")$files$networks$spectral$edges$raw,
    name_source = get_params(step = "create_edges_spectra")$names$source,
    name_target = get_params(step = "create_edges_spectra")$names$target,
    threshold = get_params(step = "create_edges_spectra")$annotations$thresholds$ms2$similarity$edges,
    ppm = get_params(step = "create_edges_spectra")$ms$tolerances$mass$ppm$ms2,
    dalton = get_params(step = "create_edges_spectra")$ms$tolerances$mass$dalton$ms2,
    qutoff = get_params(step = "create_edges_spectra")$ms$thresholds$ms2$intensity) {
  stopifnot("Your input file does not exist." = file.exists(input))
  ## Not checking for ppm and Da limits, everyone is free.

  log_debug("Loading spectra...")
  spectra <- input |>
    import_spectra()
  if (length(spectra) > 1) {
    spectra <- spectra |>
      sanitize_spectra(cutoff = qutoff) |>
      # Spectra::addProcessing(remove_above_precursor(),
      #   spectraVariables = c("precursorMz")
      # ) |>
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
      tidytable::bind_rows()
    rm(precz)

    log_debug("Calculating features' entropy")
    entropy <- pbapply::pblapply(
      X = seq_along(1:nspecz),
      FUN = function(x, peaks = fragz) {
        return(peaks[[x]] |> msentropy::calculate_spectral_entropy())
      }
    )
    log_debug("Calculating features' number of peaks")
    npeaks <- pbapply::pblapply(
      X = seq_along(1:nspecz),
      FUN = function(x, peaks = fragz) {
        return(peaks[[x]] |> length())
      }
    )
    rm(nspecz, fragz)

    edges <- edges |>
      tidytable::select(
        !!as.name(name_source) := "feature_id",
        !!as.name(name_target) := "target_id",
        tidytable::everything()
      )

    ## ISSUE see #148 find a way to have consistency in spectrum IDs
    idz <- spectra@backend@spectraData$acquisitionNum
    rm(spectra)
    edges <- edges |>
      tidytable::mutate(
        name_source = idz[name_source],
        name_target = idz[name_target]
      )
    entropy_df <- tidytable::tidytable(entropy) |>
      tidyfst::rn_col(var = name_source) |>
      tidytable::mutate(
        name_source = idz[name_source],
        feature_spectrum_entropy = as.character(entropy),
        feature_spectrum_peaks = as.character(npeaks)
      ) |>
      tidytable::mutate(
        !!as.name(name_source) := as.integer(!!as.name(name_source))
      ) |>
      tidytable::distinct(
        !!as.name(name_source),
        feature_spectrum_entropy,
        feature_spectrum_peaks
      )
    rm(entropy, npeaks, idz)

    edges <- edges |>
      tidytable::select(tidytable::any_of(
        c(
          name_source,
          name_target,
          "candidate_score_similarity" = "score",
          "candidate_count_similarity_peaks_matched"
        )
      ))

    edges <- edges |>
      tidytable::filter(
        candidate_score_similarity >= threshold
      )

    edges <- edges |>
      tidytable::full_join(entropy_df) |>
      tidytable::mutate(
        !!as.name(name_target) := tidytable::coalesce(
          !!as.name(name_target),
          !!as.name(name_source)
        )
      )
    rm(entropy_df)
  } else {
    log_debug("No spectra were found, returning an empty dataframe instead")
    edges <- tidytable::tidytable(
      !!as.name(name_source) := NA,
      "feature_spectrum_entropy" = NA,
      "feature_spectrum_peaks" = NA,
      !!as.name(name_target) := NA,
      "candidate_score_similarity" = NA,
      "candidate_count_similarity_peaks_matched" = NA
    )
  }

  export_params(parameters = get_params(step = "create_edges_spectra"), step = "create_edges_spectra")
  export_output(x = edges, file = output[[1]])
  rm(edges)

  return(output[[1]])
}
