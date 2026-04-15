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
#' @param input [character] Path or list of paths to query MGF file(s)
#'     containing spectra
#' @param output [character] Path for output edges file
#' @param name_source [character] Name of source feature column
#' @param name_target [character] Name of target feature column
#' @param method [character] Similarity method to use
#' @param threshold [numeric] Minimum similarity threshold (0-1) to report edge
#' @param matched_peaks [integer] Minimum number of matched peaks required
#' @param ppm [numeric] Relative mass tolerance in ppm
#' @param dalton [numeric] Absolute mass tolerance in Daltons
#' @param cutoff [numeric] Intensity cutoff below which MS2 fragments are
#'     removed.
#'     Non-negative numeric or NULL for dynamic thresholding.
#' @param qutoff `r lifecycle::badge("deprecated")` Use `cutoff` instead.
#'
#' @return Character string path to the created spectral edges file
#'
#' @family workflow
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
  cutoff = get_params(
    step = "create_edges_spectra"
  )$ms$thresholds$ms2$intensity,
  qutoff = deprecated()
) {
  # Handle deprecated qutoff parameter
  if (lifecycle::is_present(qutoff)) {
    lifecycle::deprecate_warn(
      "2.13.0",
      "create_edges_spectra(qutoff)",
      "create_edges_spectra(cutoff)"
    )
    cutoff <- qutoff
  }

  ctx <- log_operation(
    "create_edges_spectra",
    method = method,
    threshold = threshold,
    n_input_files = length(input)
  )

  # Validate similarity method early
  if (!method %in% VALID_SIMILARITY_METHODS) {
    cli::cli_abort(
      "{.arg method} must be one of {.or {.val {VALID_SIMILARITY_METHODS}}}, not {.val {method}}",
      class = "tima_validation_error"
    )
  }

  # Input Validation ----

  # Validate numeric parameters first (cheap checks)
  if (!is.numeric(threshold) || threshold < 0 || threshold > 1) {
    cli::cli_abort(
      "{.arg threshold} must be between 0 and 1, got {.val {threshold}}",
      class = "tima_validation_error"
    )
  }

  if (!is.numeric(matched_peaks) || matched_peaks < 1) {
    cli::cli_abort(
      "{.arg matched_peaks} must be a positive integer, got {.val {matched_peaks}}",
      class = "tima_validation_error"
    )
  }

  if (!is.numeric(ppm) || ppm <= 0) {
    cli::cli_abort(
      "{.arg ppm} must be a positive number, got {.val {ppm}}",
      class = "tima_validation_error"
    )
  }

  if (!is.numeric(dalton) || dalton <= 0) {
    cli::cli_abort(
      "{.arg dalton} must be a positive number, got {.val {dalton}}",
      class = "tima_validation_error"
    )
  }

  if (!is.null(cutoff) && (!is.numeric(cutoff) || cutoff < 0)) {
    cli::cli_abort(
      "{.arg cutoff} must be non-negative or NULL, got {.val {cutoff}}",
      class = "tima_validation_error"
    )
  }

  # Validate output path
  if (!is.character(output) || length(output) != 1L) {
    cli::cli_abort(
      "output must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate input file paths
  if (is.list(input)) {
    input_vec <- unlist(input)
    if (!is.character(input_vec)) {
      cli::cli_abort(
        "all input elements must be character strings",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }

    missing_files <- input_vec[!file.exists(input_vec)]
    if (length(missing_files) > 0L) {
      cli::cli_abort(
        c(
          "input file(s) not found",
          "x" = paste(missing_files, collapse = ", ")
        ),
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }
  } else {
    if (!is.character(input) || length(input) != 1L) {
      cli::cli_abort(
        "input must be a single character string or a list of character strings",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }
    if (!file.exists(input)) {
      cli::cli_abort(
        c(
          "input file not found",
          "x" = input
        ),
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }
  }

  # Import and Process Spectra ----

  log_info("Creating spectral similarity network edges")
  log_debug("Parameters - Threshold: %.2f, Method: %s", threshold, method)
  log_debug("Tolerances - PPM: %.2f, Dalton: %.2f", ppm, dalton)

  # Import spectra with specified parameters
  spectra <- input |>
    import_spectra(
      cutoff = cutoff,
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
  entropy <- vapply(
    X = fragz,
    FUN = msentropy::calculate_spectral_entropy,
    FUN.VALUE = numeric(1),
    USE.NAMES = FALSE
  )
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

  # Drop placeholder NA row(s) from create_edges; threshold already applied
  # upstream
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

  n_edges <- nrow(edges)
  export_output(x = edges, file = output[[1L]])
  rm(edges)

  log_complete(ctx, n_edges = n_edges)

  output[[1L]]
}
