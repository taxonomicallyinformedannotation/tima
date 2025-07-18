#' @title Create edges spectra
#'
#' @description This function create edges
#'    based on fragmentation spectra similarity
#'
#' @include create_edges.R
#' @include get_params.R
#' @include get_spectra_ids.R
#' @include import_spectra.R
#'
#' @param input Query file containing spectra. Currently an '.mgf' file
#' @param output Output file.
#' @param name_source Name of the source features column
#' @param name_target Name of the target features column
#' @param method Similarity method
#' @param threshold Minimal similarity to report
#' @param matched_peaks Minimal number of matched peaks
#' @param ppm Relative ppm tolerance to be used
#' @param dalton Absolute Dalton tolerance to be used
#' @param qutoff Intensity under which ms2 fragments will be removed.
#'
#' @return The path to the created spectral edges
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
  )$files$networks$spectral$edges$raw,
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
  stopifnot("Your input file does not exist." = file.exists(input))
  ## Not checking for ppm and Da limits, everyone is free.

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
