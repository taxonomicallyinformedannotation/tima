#' @title Create edges spectra
#'
#' @description This function creates molecular network edges based on MS2
#'     fragmentation spectra similarity. Compares all spectra against each
#'     other using spectral similarity metrics to identify related features.
#'
#' @include create_edges.R
#' @include get_params.R
#' @include get_spectra_ids.R
#' @include import_spectra.R
#'
#' @param input Character string path to query MGF file containing spectra
#' @param output Character string path for output edges file
#' @param name_source Character string name of source feature column
#' @param name_target Character string name of target feature column
#' @param method Character string similarity method to use
#' @param threshold Numeric minimum similarity threshold (0-1) to report edge
#' @param matched_peaks Integer minimum number of matched peaks required
#' @param ppm Numeric relative mass tolerance in ppm
#' @param dalton Numeric absolute mass tolerance in Daltons
#' @param qutoff Numeric intensity cutoff below which MS2 fragments are removed
#'
#' @return Character string path to the created spectral edges file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' get_file(
#'   url = get_default_paths()$urls$examples$spectra_mini,
#'   export = get_params(step = "create_edges_spectra")$files$spectral$raw
#' )
#' create_edges_spectra()
#' unlink("data", recursive = TRUE)
#' }
create_edges_spectra <- function(
  input = get_params(step = "create_edges_spectra")$files$spectral$raw,
  output = get_params(
    step = "create_edges_spectra"
  )$files$networks$spectral$edges$raw$spectral,
  name_source = get_params(step = "create_edges_spectra")$names$source,
  name_target = get_params(step = "create_edges_spectra")$names$target,
  method = get_params(step = "create_edges_spectra")$similarities$methods$edges,
  threshold = get_params(
    step = "create_edges_spectra"
  )$similarities$thresholds$edges,
  matched_peaks = get_params(
    step = "create_edges_spectra"
  )$similarities$thresholds$matched_peaks,
  ppm = get_params(step = "create_edges_spectra")$ms$tolerances$mass$ppm$ms2,
  dalton = get_params(
    step = "create_edges_spectra"
  )$ms$tolerances$mass$dalton$ms2,
  qutoff = get_params(step = "create_edges_spectra")$ms$thresholds$ms2$intensity
) {
  # Validate file paths
  if (!is.character(input) || length(input) != 1L) {
    stop("input must be a single character string")
  }

  if (!file.exists(input)) {
    stop("Input file not found: ", input)
  }

  if (!is.character(output) || length(output) != 1L) {
    stop("output must be a single character string")
  }

  # Validate numeric parameters
  if (!is.numeric(threshold) || threshold < 0 || threshold > 1) {
    stop("threshold must be between 0 and 1, got: ", threshold)
  }

  if (!is.numeric(matched_peaks) || matched_peaks < 1) {
    stop("matched_peaks must be a positive integer")
  }

  if (!is.numeric(ppm) || ppm <= 0) {
    stop("ppm must be a positive number")
  }

  if (!is.numeric(dalton) || dalton <= 0) {
    stop("dalton must be a positive number")
  }

  if (!is.numeric(qutoff) || qutoff < 0) {
    stop("qutoff must be a non-negative number")
  }

  logger::log_info("Creating spectral similarity network edges")
  logger::log_debug("Parameters - Threshold: ", threshold, ", Method: ", method)
  logger::log_debug("Tolerances - PPM: ", ppm, ", Dalton: ", dalton)

  spectra <- input |>
    import_spectra(
      cutoff = qutoff,
      dalton = dalton,
      ppm = ppm
    )
  if (length(spectra) > 1) {
    logger::log_trace("Performing spectral comparison")
    logger::log_trace(
      "As we do not limit the precursors delta,
      expect a (relatively) long processing time."
    )
    logger::log_with_separator("Take yourself a break, you deserve it.")
    nspecz <- length(spectra)
    fragz <- spectra@backend@peaksData
    precz <- spectra@backend@spectraData$precursorMz

    edges <- create_edges(
      frags = fragz,
      nspecs = nspecz,
      precs = precz,
      method = method,
      ms2_tolerance = dalton,
      ppm_tolerance = ppm,
      threshold = threshold,
      matched_peaks = matched_peaks
    )

    logger::log_trace("Calculating features' entropy")
    entropy <- purrr::map(
      .x = seq_along(1:nspecz),
      .f = function(x, peaks = fragz) {
        return(
          peaks[[x]] |>
            msentropy::calculate_spectral_entropy()
        )
      }
    )
    logger::log_trace("Counting features' number of peaks")
    npeaks <- purrr::map(
      .x = seq_along(1:nspecz),
      .f = function(x, peaks = fragz) {
        return(
          peaks[[x]] |>
            length() /
            2
        )
      }
    )
    rm(nspecz, fragz)

    edges <- edges |>
      tidytable::select(
        !!as.name(name_source) := "feature_id",
        !!as.name(name_target) := "target_id",
        tidyselect::everything()
      )

    idz <- spectra |>
      get_spectra_ids()
    rm(spectra)
    edges <- edges |>
      tidytable::mutate(
        name_source = idz[name_source],
        name_target = idz[name_target]
      )
    entropy_df <- tidytable::tidytable(entropy) |>
      tidytable::mutate(!!as.name(name_source) := tidytable::row_number()) |>
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
      tidytable::select(tidyselect::any_of(
        c(
          name_source,
          name_target,
          "candidate_score_similarity" = "score",
          "candidate_count_similarity_peaks_matched" = "matched_peaks"
        )
      ))

    edges <- edges |>
      tidytable::filter(candidate_score_similarity >= threshold)

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
    logger::log_warn(
      "No spectra were found, returning an empty dataframe instead"
    )
    edges <- tidytable::tidytable(
      !!as.name(name_source) := NA,
      "feature_spectrum_entropy" = NA,
      "feature_spectrum_peaks" = NA,
      !!as.name(name_target) := NA,
      "candidate_score_similarity" = NA,
      "candidate_count_similarity_peaks_matched" = NA
    )
  }

  export_params(
    parameters = get_params(step = "create_edges_spectra"),
    step = "create_edges_spectra"
  )
  export_output(x = edges, file = output[[1]])
  rm(edges)

  return(output[[1]])
}
