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
#' @param input Character string path or list of paths to query MGF file(s) containing spectra
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
  # Validate similarity method early
  if (!method %in% VALID_SIMILARITY_METHODS) {
    stop(
      "Similarity method must be one of: ",
      paste(VALID_SIMILARITY_METHODS, collapse = ", "),
      "; got: ",
      method
    )
  }

  # Input Validation ----

  # Validate numeric parameters first (cheap checks)
  if (!is.numeric(threshold) || threshold < 0 || threshold > 1) {
    stop("threshold must be between 0 and 1, got: ", threshold)
  }

  if (!is.numeric(matched_peaks) || matched_peaks < 1) {
    stop("matched_peaks must be a positive integer, got: ", matched_peaks)
  }

  if (!is.numeric(ppm) || ppm <= 0) {
    stop("ppm must be a positive number, got: ", ppm)
  }

  if (!is.numeric(dalton) || dalton <= 0) {
    stop("dalton must be a positive number, got: ", dalton)
  }

  if (!is.null(qutoff) && (!is.numeric(qutoff) || qutoff < 0)) {
    stop("qutoff intensity must be non-negative or NULL, got: ", qutoff)
  }

  # Validate output path
  if (!is.character(output) || length(output) != 1L) {
    stop("output must be a single character string")
  }

  # Validate input file paths
  if (is.list(input)) {
    input_vec <- unlist(input)
    if (!is.character(input_vec)) {
      stop("All input elements must be character strings")
    }

    missing_files <- input_vec[!file.exists(input_vec)]
    if (length(missing_files) > 0L) {
      stop(
        "Input file(s) not found: ",
        paste(missing_files, collapse = ", ")
      )
    }
  } else {
    if (!is.character(input) || length(input) != 1L) {
      stop(
        "input must be a single character string or a list of character strings"
      )
    }
    if (!file.exists(input)) {
      stop("Input file not found: ", input)
    }
  }

  # Import and Process Spectra ----

  log_info("Creating spectral similarity network edges")
  log_debug("Parameters - Threshold: %f2, Method: %s", threshold, method)
  log_debug("Tolerances - PPM: %f2, Dalton: %f2", ppm, dalton)

  # Import spectra with specified parameters
  spectra <- input |>
    import_spectra(
      cutoff = qutoff,
      dalton = dalton,
      ppm = ppm
    )

  # Early exit if only one or no spectra
  if (length(spectra) <= 1L) {
    log_warn(
      "Only %d spectrum found - need at least 2 for network edges",
      length(spectra)
    )
    edges <- tidytable::tidytable(
      !!as.name(name_source) := NA,
      "feature_spectrum_entropy" = NA,
      "feature_spectrum_peaks" = NA,
      !!as.name(name_target) := NA,
      "candidate_score_similarity" = NA,
      "candidate_count_similarity_peaks_matched" = NA
    )
    export_output(x = edges, file = output)
    return(output)
  }

  # Compute Spectral Similarities ----

  # log_trace(
  #  "Performing spectral comparison on %d spectra", length(spectra)
  # )
  # log_trace(
  #  "As the precursors delta is not limited, expect a long processing time."
  # )
  log_info("======================================")
  log_info("Take yourself a break, you deserve it.")
  log_info("======================================")

  # Extract data (cache for reuse)
  nspecz <- length(spectra)
  fragz <- spectra@backend@peaksData
  precz <- spectra@backend@spectraData$precursorMz

  # Create edges
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

  # Calculate spectral entropy
  # log_trace("Calculating features' entropy")
  entropy <- vapply(
    X = fragz,
    FUN = msentropy::calculate_spectral_entropy,
    FUN.VALUE = numeric(1),
    USE.NAMES = FALSE
  )
  # log_trace("Counting features' number of peaks")
  npeaks <- vapply(
    X = fragz,
    FUN = nrow,
    FUN.VALUE = numeric(1)
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

  # Ensure consistent typing for IDs - keep as-is from source
  # This prevents type mismatch in the later join
  edges <- edges |>
    tidytable::mutate(
      !!as.name(name_source) := idz[!!as.name(name_source)],
      !!as.name(name_target) := idz[!!as.name(name_target)]
    )

  entropy_df <- tidytable::tidytable(entropy) |>
    tidytable::mutate(
      !!as.name(name_source) := tidytable::row_number()
    ) |>
    tidytable::mutate(
      !!as.name(name_source) := idz[!!as.name(name_source)],
      feature_spectrum_entropy = as.character(entropy),
      feature_spectrum_peaks = as.character(npeaks)
    ) |>
    tidytable::distinct(
      !!as.name(name_source),
      feature_spectrum_entropy,
      feature_spectrum_peaks
    )
  rm(entropy, npeaks, idz)

  edges <- edges |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          name_source,
          name_target,
          "candidate_score_similarity" = "score",
          "candidate_count_similarity_peaks_matched" = "matched_peaks"
        )
      )
    )

  # Drop placeholder NA row(s) from create_edges; threshold already applied upstream
  edges <- edges |>
    tidytable::filter(!is.na(!!as.name(name_source)))

  edges <- edges |>
    tidytable::full_join(y = entropy_df) |>
    tidytable::mutate(
      !!as.name(name_target) := tidytable::coalesce(
        !!as.name(name_target),
        !!as.name(name_source)
      )
    )
  rm(entropy_df)

  export_params(
    parameters = get_params(step = "create_edges_spectra"),
    step = "create_edges_spectra"
  )

  export_output(x = edges, file = output[[1]])
  rm(edges)

  return(output[[1]])
}
