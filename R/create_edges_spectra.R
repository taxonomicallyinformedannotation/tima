#' @title Create edges spectra
#'
#' @description This function creates molecular network edges based on MS2
#'     fragmentation spectra similarity. Compares all spectra against each
#'     other using spectral similarity metrics to identify related features.
#'     It annotates the resulting edges with community labels derived from
#'     the similarity network and retains only intra-community edges, so the
#'     edge file itself carries the community structure.
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
#' @param ppm [numeric] Relative mass tolerance in ppm
#' @param dalton [numeric] Absolute mass tolerance in Daltons
#' @param cutoff [numeric] Intensity cutoff below which MS2 fragments are
#'     removed.
#'     Non-negative numeric or NULL for dynamic thresholding.
#' @param min_fragments [integer] Minimum number of fragment peaks a spectrum
#'     must have after cleaning to be retained
#' @param resolution [numeric] Resolution parameter for the Leiden/Louvain algorithm
#'    (higher values -> more, smaller communities). Default: 0.1.
#' @param n_iterations [integer] Number of iterations for the Leiden algorithm.
#'    Ignored for Louvain. Default: 2.
#' @param seed [integer] Random seed for reproducible clustering. If `NULL`,
#'    a random seed is used. Default: NULL.
#'
#' @return Character string path to the created spectral edges file.
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
  ppm = get_params(step = "create_edges_spectra")$ms$tolerances$mass$ppm$ms2,
  dalton = get_params(
    step = "create_edges_spectra"
  )$ms$tolerances$mass$dalton$ms2,
  cutoff = get_params(
    step = "create_edges_spectra"
  )$ms$thresholds$ms2$intensity,
  min_fragments = get_params(
    step = "create_edges_spectra"
  )$ms$thresholds$ms2$min_fragments,
  resolution = 0.1,
  n_iterations = 2L,
  seed = NULL
) {
  ctx <- log_operation(
    "create_edges_spectra",
    method = method,
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
  if (
    !is.numeric(cutoff) ||
      length(cutoff) != 1L ||
      !is.finite(cutoff) ||
      cutoff < 0
  ) {
    cli::cli_abort(
      "{.arg cutoff} must be a non-negative number, got {.val {cutoff}}",
      class = "tima_validation_error"
    )
  }

  if (
    !is.numeric(resolution) ||
      length(resolution) != 1L ||
      !is.finite(resolution) ||
      resolution <= 0
  ) {
    cli::cli_abort(
      "{.arg resolution} must be a positive number, got {.val {resolution}}",
      class = "tima_validation_error"
    )
  }

  if (
    !is.numeric(n_iterations) ||
      length(n_iterations) != 1L ||
      !is.finite(n_iterations) ||
      n_iterations < 1
  ) {
    cli::cli_abort(
      "{.arg n_iterations} must be a positive integer, got {.val {n_iterations}}",
      class = "tima_validation_error"
    )
  }

  if (
    !is.null(seed) &&
      (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed))
  ) {
    cli::cli_abort(
      "{.arg seed} must be NULL or a single numeric value, got {.val {seed}}",
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

  # Validate output path
  if (!is.character(output) || length(output) != 1L) {
    cli::cli_abort(
      "output must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  source_col <- if (
    is.null(name_source) ||
      length(name_source) != 1L ||
      !is.character(name_source) ||
      !nzchar(name_source)
  ) {
    "source"
  } else {
    as.character(name_source)
  }
  target_col <- if (
    is.null(name_target) ||
      length(name_target) != 1L ||
      !is.character(name_target) ||
      !nzchar(name_target)
  ) {
    "target"
  } else {
    as.character(name_target)
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
  log_debug("Parameters - Method: %s", method)
  log_debug("Tolerances - PPM: %.2f, Dalton: %.2f", ppm, dalton)

  # Import spectra with specified parameters
  spectra <- input |>
    import_spectra(
      cutoff = cutoff,
      dalton = dalton,
      min_fragments = min_fragments,
      ppm = ppm
    )

  # Fail fast when no usable spectra were parsed from input.
  if (length(spectra) == 0L) {
    cli::cli_abort(
      c(
        "No usable spectra found in input",
        "x" = "All spectra were empty, malformed, or removed by filtering",
        "i" = "Check MGF formatting and ensure at least one MS2 spectrum has fragment peaks"
      ),
      class = c("tima_runtime_error", "tima_error"),
      call = NULL
    )
  }

  # Early exit if only one spectrum
  if (length(spectra) == 1L) {
    log_warn(
      "Only %d spectrum found - need at least 2 for network edges",
      length(spectra)
    )
    edges <- tidytable::tidytable(
      source = NA,
      target = NA,
      feature_spectrum_entropy = NA,
      feature_spectrum_peaks = NA,
      candidate_score_similarity = NA,
      candidate_count_similarity_peaks_matched = NA
    )
    names(edges) <- c(
      source_col,
      target_col,
      names(edges)[3:length(names(edges))]
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
  precz <- get_precursors(spectra)

  # Compute the full pairwise similarity graph once and reuse it for both
  # community detection and the exported edge list.
  all_edges <- create_edges(
    frags = fragz,
    nspecs = nspecz,
    precs = precz,
    method = method,
    ms2_tolerance = dalton,
    ppm_tolerance = ppm,
    assume_sanitized = TRUE
  )

  edges <- all_edges
  community_edges <- all_edges

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

  idz <- spectra |>
    get_spectra_ids()
  if (is.null(idz) || length(idz) != length(spectra)) {
    idz <- seq_along(spectra)
  }
  rm(spectra)

  normalize_edge_table <- function(edge_table) {
    if ("feature_id" %in% names(edge_table)) {
      names(edge_table)[names(edge_table) == "feature_id"] <- source_col
    }
    if ("target_id" %in% names(edge_table)) {
      names(edge_table)[names(edge_table) == "target_id"] <- target_col
    }

    edge_table[[source_col]] <- idz[edge_table[[source_col]]]
    edge_table[[target_col]] <- idz[edge_table[[target_col]]]

    edge_table[!is.na(edge_table[[source_col]]), , drop = FALSE]
  }

  edges <- normalize_edge_table(edges)

  community_edges <- normalize_edge_table(community_edges)

  entropy_df <- tidytable::tidytable(
    source = seq_along(entropy),
    feature_spectrum_entropy = as.character(entropy),
    feature_spectrum_peaks = as.character(npeaks)
  )
  names(entropy_df)[names(entropy_df) == "source"] <- source_col
  entropy_df[[source_col]] <- idz[entropy_df[[source_col]]]
  entropy_df <- entropy_df[
    !duplicated(entropy_df[[source_col]]),
    c(source_col, "feature_spectrum_entropy", "feature_spectrum_peaks"),
    drop = FALSE
  ]
  rm(entropy, npeaks, idz)

  edges <- edges |>
    tidytable::full_join(y = entropy_df)
  edges[[target_col]] <- tidytable::coalesce(
    edges[[target_col]],
    edges[[source_col]]
  )
  drop_legacy_similarity_columns <- function(edge_table) {
    legacy_columns <- intersect(c("score", "matched_peaks"), names(edge_table))
    if (length(legacy_columns) > 0L) {
      edge_table <- edge_table |>
        tidytable::select(-tidyselect::any_of(legacy_columns))
    }
    edge_table
  }

  if (
    !"candidate_score_similarity" %in% names(edges) && "score" %in% names(edges)
  ) {
    edges <- edges |>
      tidytable::mutate(
        candidate_score_similarity = as.numeric(score)
      )
  }

  if (
    !"candidate_count_similarity_peaks_matched" %in% names(edges) &&
      "matched_peaks" %in% names(edges)
  ) {
    edges <- edges |>
      tidytable::mutate(
        candidate_count_similarity_peaks_matched = as.integer(matched_peaks)
      )
  }

  edges <- drop_legacy_similarity_columns(edges)

  rm(entropy_df)

  edges_before_community_cut <- edges

  community_result <- .build_components_from_edges(
    edges = community_edges,
    name_source = source_col,
    name_target = target_col,
    resolution = resolution,
    n_iterations = n_iterations,
    seed = seed,
    label_column = "community_id",
    cut_to_communities = FALSE
  )

  if (nrow(edges) > 0L && length(community_result$component_membership) > 0L) {
    edges <- .prune_intra_community_edges(
      edges = edges,
      component_membership = community_result$component_membership,
      name_source = source_col,
      name_target = target_col
    )

    if (nrow(edges) == 0L) {
      log_warn(
        "No intra-community edges remained after community cut; keeping only isolated features"
      )
      edges <- edges_before_community_cut[0L, , drop = FALSE]
    }

    retained_features <- unique(c(edges[[source_col]], edges[[target_col]]))
    all_features <- unique(c(
      edges_before_community_cut[[source_col]],
      edges_before_community_cut[[target_col]]
    ))
    isolated_features <- setdiff(all_features, retained_features)

    if (length(isolated_features) > 0L) {
      isolated_edges <- tidytable::tidytable(
        source = isolated_features,
        target = isolated_features,
        candidate_score_similarity = NA_real_,
        candidate_count_similarity_peaks_matched = NA_integer_
      )
      names(isolated_edges) <- c(
        source_col,
        target_col,
        names(isolated_edges)[3:length(names(isolated_edges))]
      )
      edges <- tidytable::bind_rows(edges, isolated_edges) |>
        tidytable::distinct()
    }
  }

  if (nrow(edges) > 0L) {
    log_info(
      "Found %d communities using %s",
      length(unique(community_result$component_membership)),
      community_result$method_used
    )

    pre_cut_features <- unique(c(
      edges_before_community_cut[[source_col]],
      edges_before_community_cut[[target_col]]
    ))
    retained_features <- unique(c(edges[[source_col]], edges[[target_col]]))
    dropped_features <- setdiff(pre_cut_features, retained_features)

    if (length(dropped_features) > 0L) {
      log_info(
        "Community cut removed %d feature(s) that had no intra-community edges; %d feature(s) remain in the exported edge graph",
        length(dropped_features),
        length(retained_features)
      )
    }
  } else {
    log_info("No edges found for community detection")
  }

  export_output(x = edges, file = output)

  n_edges <- nrow(edges)
  n_features <- length(unique(c(edges[[source_col]], edges[[target_col]])))
  rm(edges)
  log_info("Edges written to: %s", output)

  log_complete(
    ctx,
    n_edges = n_edges,
    n_features = n_features
  )
  invisible(output)
}
