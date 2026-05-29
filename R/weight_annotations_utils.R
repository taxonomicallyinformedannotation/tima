#' Validate Inputs for weight_annotations
#'
#' @description Internal helper to validate all input parameters for
#'     weight_annotations.
#' Checks file existence, data types, ranges, and logical consistency of
#'     weights.
#'     Stops execution with informative messages if any condition is violated.
#'
#' @include add_xrefs_to_annotations.R
#' @include constants.R
#' @include safe_fread.R
#' @include validations_utils.R
#'
#' @param library Character, path to library file (required).
#' @param components Character, path to components file (required).
#' @param edges Character, path to edges file (required).
#' @param taxa Character, path to taxa file (required).
#' @param annotations Character vector, paths to annotation files (required).
#' @param str_stereo Character, path to stereo structures file (optional).
#' @param org_tax_ott Character, path to organism taxonomy file (optional).
#' @param canopus Character, path to canopus file (optional).
#' @param formula Character, path to formula file (optional).
#' @param minimal_ms1_condition Character, "OR" or "AND".
#' @param weight_spectral Numeric (0-Inf), weight for spectral score.
#' @param weight_chemical Numeric (0-Inf), weight for chemical score.
#' @param weight_biological Numeric (0-Inf), weight for biological score.
#' @param minimal_consistency Numeric (0-1), min consistency threshold.
#' @param minimal_ms1_bio Numeric (0-1), min biological score for MS1.
#' @param minimal_ms1_chemo Numeric (0-1), min chemical score for MS1.
#' @param ms1_only Logical, filter for MS1-only annotations.
#' @param compounds_names Logical, include compound names.
#' @param high_confidence Logical, report high-confidence candidates only.
#' @param remove_ties Logical, remove tied candidates.
#' @param summarize Logical, summarize to one row per feature.
#' @param force Logical, force execution (override warnings).
#' @param candidates_neighbors Integer >= 1, neighbors to keep.
#' @param candidates_final Integer >= 1, final candidates to keep.
#'
#' @return NULL (invisible). Stops execution if validation fails.
#' @keywords internal
validate_weight_annotations_inputs <- function(
  library,
  components,
  edges,
  taxa,
  annotations,
  str_stereo = NULL,
  org_tax_ott = NULL,
  canopus = NULL,
  formula = NULL,
  minimal_ms1_condition,
  weight_spectral,
  weight_chemical,
  weight_biological,
  minimal_consistency,
  minimal_ms1_bio,
  minimal_ms1_chemo,
  ms1_only,
  compounds_names,
  high_confidence,
  remove_ties,
  summarize,
  force,
  candidates_neighbors,
  candidates_final
) {
  # Validate required file existence
  required_files <- list(
    library = library,
    components = components,
    edges = edges,
    taxa = taxa
  )
  missing_required <- purrr::keep(.x = required_files, .p = ~ !file.exists(.x))
  if (length(missing_required) > 0L) {
    cli::cli_abort(
      c(
        "Required file(s) not found",
        "x" = paste(
          names(missing_required),
          missing_required,
          sep = " at ",
          collapse = "; "
        )
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate annotation files
  missing_annotations <- annotations[!file.exists(annotations)]
  if (length(missing_annotations) > 0L) {
    cli::cli_abort(
      c(
        "Annotation file(s) not found",
        "x" = paste(missing_annotations, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate optional files (log warnings only)
  optional_files <- list(
    str_stereo = str_stereo,
    org_tax_ott = org_tax_ott,
    canopus = canopus,
    formula = formula
  )
  purrr::iwalk(.x = optional_files, .f = function(path, name) {
    if (!is.null(path) && !file.exists(path)) {
      log_warn("Optional file '%s' not found at: %s", name, path)
    }
  })

  # Validate minimal_ms1_condition
  assert_choice(minimal_ms1_condition, c("OR", "AND"), "minimal_ms1_condition")

  # Validate weights: must be numeric, non-negative, sum to ~1
  weights <- c(
    spectral = weight_spectral,
    chemical = weight_chemical,
    biological = weight_biological
  )
  if (!all(is.numeric(weights)) || anyNA(weights)) {
    cli::cli_abort(
      "All weights (spectral, chemical, biological) must be numeric and non-NA",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  if (any(weights < 0)) {
    cli::cli_abort(
      c(
        "All weights must be non-negative",
        "x" = paste(names(weights), "=", weights, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  weight_sum <- sum(weights)
  if (abs(weight_sum - 1) > WEIGHT_SUM_TOLERANCE) {
    cli::cli_abort(
      c(
        "Weights must sum to 1",
        "x" = paste0(
          "tolerance=",
          WEIGHT_SUM_TOLERANCE,
          "; sum=",
          signif(weight_sum, 6),
          " (",
          paste(names(weights), weights, sep = "=", collapse = ", "),
          ")"
        )
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate score thresholds (0-1)
  assert_scalar_numeric(
    minimal_consistency,
    "minimal_consistency",
    min = 0,
    max = 1
  )
  assert_scalar_numeric(minimal_ms1_bio, "minimal_ms1_bio", min = 0, max = 1)
  assert_scalar_numeric(
    minimal_ms1_chemo,
    "minimal_ms1_chemo",
    min = 0,
    max = 1
  )

  # Validate logical flags
  assert_flag(ms1_only, "ms1_only")
  assert_flag(compounds_names, "compounds_names")
  assert_flag(high_confidence, "high_confidence")
  assert_flag(remove_ties, "remove_ties")
  assert_flag(summarize, "summarize")
  assert_flag(force, "force")

  # Validate candidate counts
  assert_positive_integer(candidates_neighbors, "candidates_neighbors")
  assert_positive_integer(candidates_final, "candidates_final")

  invisible(NULL)
}

#' Load and Prepare Annotation Tables
#'
#' @description Internal helper to load, combine, and filter annotation tables.
#'     Reads multiple annotation files, binds rows, and optionally filters to
#' retain only MS1-based annotations (those without MS2 similarity or SIRIUS CSI
#'     scores).
#'
#' @param annotations [character] Character vector of file paths to annotation
#'     files.
#' @param ms1_only [logical] Logical; if TRUE, keep only annotations where both
#'     `candidate_score_similarity` and `candidate_score_sirius_csi` are NA.
#'
#' @return Data frame with combined (and optionally filtered) annotations.
#' @keywords internal
load_annotation_tables <- function(annotations, ms1_only) {
  log_debug("Loading %d annotation file(s)", length(annotations))
  annotation_table <- tryCatch(
    purrr::map2(
      .x = annotations,
      .y = seq_along(annotations),
      .f = ~ safe_fread(
        file = .x,
        file_type = paste0("annotation file ", .y),
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    ) |>
      tidytable::bind_rows(),
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to load annotation files",
          "x" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  if (ms1_only) {
    n_before <- nrow(annotation_table)
    has_sirius_confidence <- "candidate_score_sirius_confidence" %in%
      names(annotation_table)
    has_similarity <- "candidate_score_similarity" %in% names(annotation_table)
    has_sirius_csi <- "candidate_score_sirius_csi" %in% names(annotation_table)

    if (has_sirius_confidence) {
      if (has_similarity && has_sirius_csi) {
        annotation_table <- annotation_table |>
          tidytable::filter(
            is.na(candidate_score_similarity) &
              is.na(candidate_score_sirius_csi) &
              (is.na(candidate_score_sirius_confidence) |
                as.numeric(candidate_score_sirius_confidence) == 0)
          )
      } else if (has_similarity) {
        annotation_table <- annotation_table |>
          tidytable::filter(
            is.na(candidate_score_similarity) &
              (is.na(candidate_score_sirius_confidence) |
                as.numeric(candidate_score_sirius_confidence) == 0)
          )
      } else if (has_sirius_csi) {
        annotation_table <- annotation_table |>
          tidytable::filter(
            is.na(candidate_score_sirius_csi) &
              (is.na(candidate_score_sirius_confidence) |
                as.numeric(candidate_score_sirius_confidence) == 0)
          )
      } else {
        annotation_table <- annotation_table |>
          tidytable::filter(
            is.na(candidate_score_sirius_confidence) |
              as.numeric(candidate_score_sirius_confidence) == 0
          )
      }
    } else {
      if (has_similarity && has_sirius_csi) {
        annotation_table <- annotation_table |>
          tidytable::filter(
            is.na(candidate_score_similarity) &
              is.na(candidate_score_sirius_csi)
          )
      } else if (has_similarity) {
        annotation_table <- annotation_table |>
          tidytable::filter(is.na(candidate_score_similarity))
      } else if (has_sirius_csi) {
        annotation_table <- annotation_table |>
          tidytable::filter(is.na(candidate_score_sirius_csi))
      }
    }

    log_debug(
      "MS1-only filter: %d -> %d rows",
      n_before,
      nrow(annotation_table)
    )
  }

  if (nrow(annotation_table) == 0L) {
    log_warn("No annotations remaining after loading/filtering")
  }

  annotation_table
}

#' Load Structure-Organism Pairs
#'
#' @description Internal helper to load the main library table and sequentially
#' left-join optional supplemental tables (stereo structures, organism
#'     taxonomy).
#'     Returns a combined data frame suitable for biological scoring.
#'
#' @param library [character] Character, path to main library file (required).
#' @param str_stereo [character] Character, path to stereo structures file
#'     (optional; NULL allowed).
#' @param org_tax_ott [character] Character, path to organism taxonomy file
#'     (optional; NULL allowed).
#'
#' @return Data frame with structure-organism pairs, including any joined
#'     metadata.
#' @keywords internal
load_structure_organism_pairs <- function(library, str_stereo, org_tax_ott) {
  log_debug("Loading library from: %s", library)
  library_table <- tryCatch(
    safe_fread(
      file = library,
      file_type = "structure library",
      na.strings = c("", "NA"),
      colClasses = "character"
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to load library file",
          "x" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  supp_files <- list(str_stereo, org_tax_ott)
  supp_names <- c("stereochemistry", "organism taxonomy")
  supp_tables <- purrr::map2(
    .x = supp_files,
    .y = supp_names,
    .f = function(path, name) {
      if (is.null(path) || !file.exists(path)) {
        return(tidytable::tidytable())
      }
      safe_fread(
        file = path,
        file_type = name,
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    }
  )

  purrr::reduce(
    .x = supp_tables,
    .init = library_table,
    .f = function(acc, tbl) {
      if (nrow(tbl) == 0L) {
        return(acc)
      }
      tidytable::left_join(x = acc, y = tbl)
    }
  )
}

#' Load Edges Table with Neighbor Filtering
#'
#' @description Internal helper to load the edges file and retain only the top N
#'     neighbors (by similarity score) for each feature. This reduces complexity
#'     for downstream annotation propagation and scoring.
#'
#' @param edges [character] Character, path to edges file.
#' @param candidates_neighbors [integer] Integer, number of top neighbors to
#'     keep per feature.
#'
#' @return Data frame with filtered edges (top `candidates_neighbors` per
#'     `feature_source`).
#' @keywords internal
load_edges_table <- function(edges, candidates_neighbors) {
  log_debug("Loading edges from: %s", edges)
  edges_table <- tryCatch(
    safe_fread(
      file = edges,
      file_type = "spectral edges table",
      na.strings = c("", "NA"),
      colClasses = "character"
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to load edges file",
          "x" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  n_before <- nrow(edges_table)
  edges_table <- edges_table |>
    tidytable::group_by(feature_source) |>
    tidytable::slice_max(
      order_by = candidate_score_similarity,
      n = candidates_neighbors,
      with_ties = FALSE
    ) |>
    tidytable::ungroup()

  log_debug(
    "Edges filtered to top %d neighbors/feature: %d -> %d rows",
    candidates_neighbors,
    n_before,
    nrow(edges_table)
  )
  edges_table
}

#' Log Annotation Statistics
#'
#' @description Internal helper to log annotation counts by library.
#'
#' @param annotation_table [data.frame] Data frame with annotations
#'
#' @return NULL (logs statistics as side effect)
#' @keywords internal
log_annotation_stats <- function(annotation_table) {
  annotation_stats <- annotation_table |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::distinct(
      feature_id,
      candidate_library,
      candidate_structure_inchikey_connectivity_layer
    ) |>
    tidytable::group_by(candidate_library) |>
    tidytable::count() |>
    tidytable::arrange(tidytable::desc(x = n))

  annotation_stats <- add_percentage_column(
    annotation_stats,
    count_col = "n",
    out_col = "Pct"
  )

  log_info(
    "\n%s",
    paste(
      utils::capture.output(print.data.frame(
        x = annotation_stats,
        row.names = FALSE
      )),
      collapse = "\n"
    )
  )

  invisible(NULL)
}

#' Rearrange Annotation Tables
#'
#' @description Internal helper to reorganize and merge annotation tables.
#'
#' @include columns_utils.R
#'
#' @param annotation_table [data.frame] Data frame with annotations
#' @param formula_table [data.frame] Data frame with formula data
#' @param canopus_table [data.frame] Data frame with canopus data
#' @param edges_table [data.frame] Data frame with edges
#'
#' @return Data frame with rearranged annotations
#' @keywords internal
rearrange_annotations <- function(
  annotation_table,
  formula_table,
  canopus_table,
  edges_table
) {
  model <- columns_model()

  # Table 1: deduplicated annotations (all sources, spectra + structure cols)
  annotation_table_1 <- annotation_table |>
    tidytable::select(tidyselect::any_of(
      x = c(
        model$features_columns,
        model$candidates_calculated_columns,
        model$candidates_spectra_columns,
        model$candidates_structures_columns
      )
    )) |>
    tidytable::mutate(
      candidate_score_similarity = as.numeric(candidate_score_similarity)
    ) |>
    tidytable::arrange(tidytable::desc(x = candidate_score_similarity)) |>
    tidytable::distinct(
      tidyselect::any_of(c(
        "feature_id",
        "candidate_structure_inchikey_no_stereo",
        "candidate_structure_inchikey_connectivity_layer"
      )),
      .keep_all = TRUE
    )

  # Table 2: SIRIUS CSI scores only (join keys + score columns).
  # Selecting only the score columns (not all structure columns) prevents

  # the full_join from using ~18 structure columns as implicit keys,
  # which caused SIRIUS scores to fail to merge with spectral annotations
  # whenever any structure detail (name, SMILES, taxonomy, ...) differed.
  annotation_table_2 <- annotation_table |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          "feature_id",
          "candidate_structure_inchikey_connectivity_layer",
          model$candidates_sirius_str_columns
        )
      )
    ) |>
    tidytable::filter(!is.na(candidate_score_sirius_csi)) |>
    tidytable::mutate(
      candidate_score_sirius_csi = as.numeric(candidate_score_sirius_csi)
    ) |>
    tidytable::arrange(tidytable::desc(candidate_score_sirius_csi)) |>
    tidytable::distinct(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    )

  # Step 1: Merge SIRIUS scores into deduplicated annotations.
  # Using explicit keys ensures SIRIUS CSI scores are matched to
  # spectral annotations for the same structure even when other
  # structure columns (name, SMILES, taxonomy) differ between sources.
  annotation_table_merged <- tidytable::full_join(
    annotation_table_1,
    annotation_table_2,
    by = c("feature_id", "candidate_structure_inchikey_connectivity_layer")
  )

  # Step 2: Add formula-level SIRIUS scores.
  # left_join (not full_join) so that formula entries without a matching
  # annotation row do NOT create phantom rows with NA connectivity_layer.
  formula_new_cols <- setdiff(
    names(formula_table),
    names(annotation_table_merged)
  )
  formula_join_by <- intersect(
    c("feature_id", "candidate_structure_molecular_formula"),
    names(formula_table)
  )
  if (
    length(formula_new_cols) > 0L &&
      length(formula_join_by) > 0L &&
      nrow(formula_table) > 0L
  ) {
    annotation_table_merged <- tidytable::left_join(
      annotation_table_merged,
      formula_table |>
        tidytable::select(
          tidyselect::any_of(c(formula_join_by, formula_new_cols))
        ),
      by = formula_join_by
    )
  }

  # Step 3: Add CANOPUS predictions (feature-level).
  # left_join (not full_join) so that CANOPUS entries for features
  # without annotations do NOT create phantom rows.
  canopus_new_cols <- setdiff(
    names(canopus_table),
    names(annotation_table_merged)
  )
  if (length(canopus_new_cols) > 0L && nrow(canopus_table) > 0L) {
    annotation_table_merged <- tidytable::left_join(
      annotation_table_merged,
      canopus_table |>
        tidytable::select(
          tidyselect::any_of(c("feature_id", canopus_new_cols))
        ),
      by = "feature_id"
    )
  }

  # Step 4: Add edge metadata (feature-level).
  edge_new_cols <- setdiff(
    c("feature_spectrum_entropy", "feature_spectrum_peaks"),
    names(annotation_table_merged)
  )
  edges_distinct <- edges_table |>
    tidytable::distinct(
      feature_id = feature_source,
      feature_spectrum_entropy,
      feature_spectrum_peaks
    )
  if (length(edge_new_cols) > 0L) {
    annotation_table_merged <- annotation_table_merged |>
      tidytable::left_join(
        y = edges_distinct |>
          tidytable::select(
            tidyselect::any_of(c("feature_id", edge_new_cols))
          ),
        by = "feature_id"
      )
  }

  annotation_table_merged
}

#' Export Results to Files
#'
#' @description Internal helper to export all result tiers and parameters.
#'
#' @param results_list [list] List with full, filtered, mini data frames
#' @param output [character] Character, base output filename
#' @param pattern [character] Character, pattern for directory naming
#'
#' @return Named character vector with output file paths
#' @keywords internal
export_results <- function(results_list, output, pattern) {
  time <- format(Sys.time(), "%Y%m%d_%H%M%S")
  dir_time <- file.path(
    get_default_paths()$data$processed$path,
    paste0(time, "_", pattern)
  )

  final_output <- file.path(dir_time, output)
  final_output_filtered <- file.path(
    dir_time,
    gsub(output, pattern = ".tsv", replacement = "_filtered.tsv", fixed = TRUE)
  )
  final_output_mini <- file.path(
    dir_time,
    gsub(output, pattern = ".tsv", replacement = "_mini.tsv", fixed = TRUE)
  )

  # Export parameters
  export_params(
    parameters = get_params(step = "prepare_params"),
    directory = dir_time,
    step = "prepare_params"
  )
  export_params(
    parameters = get_params(step = "prepare_params_advanced"),
    directory = dir_time,
    step = "prepare_params_advanced"
  )

  # Export results
  export_output(x = results_list$mini, file = final_output_mini)
  export_output(x = results_list$filtered, file = final_output_filtered)
  export_output(x = results_list$full, file = final_output)

  c(
    "filtered" = final_output_filtered,
    "full" = final_output
  )
}
