#' Validate Inputs for filter_annotations
#'
#' @description Internal helper to validate all input parameters.
#'
#' @param annotations Character vector or list of annotation file paths
#' @param features Character path to features file
#' @param rts Character vector of RT library paths (optional)
#' @param output Character path for output
#' @param tolerance_rt Numeric RT tolerance
#'
#' @return NULL (stops execution if validation fails)
#' @keywords internal
validate_filter_annotations_inputs <- function(
  annotations,
  features,
  rts,
  output,
  tolerance_rt
) {
  # Validate tolerance (only enforce when RT library is provided)
  if (
    !is.numeric(tolerance_rt) ||
      length(tolerance_rt) != 1L ||
      is.na(tolerance_rt) ||
      tolerance_rt <= 0
  ) {
    if (length(rts) > 0) {
      cli::cli_abort(
        c(
          "tolerance_rt must be a positive number when RT library is provided",
          "x" = as.character(tolerance_rt)
        ),
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }
  }

  # Validate output path
  if (!is.character(output) || length(output) != 1L) {
    cli::cli_abort(
      "output must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate features
  if (!is.character(features) || length(features) != 1L) {
    cli::cli_abort(
      "features must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!file.exists(features)) {
    cli::cli_abort(
      c(
        "features file not found",
        "x" = features
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate annotations (handle both list and character vector)
  if (is.list(annotations)) {
    if (length(annotations) == 0L) {
      cli::cli_abort(
        "annotations must be a non-empty list or character vector",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }

    ann_vec <- unlist(annotations)
    if (!is.character(ann_vec)) {
      cli::cli_abort(
        "all annotation elements must be character strings",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }

    missing_annotations <- ann_vec[!file.exists(ann_vec)]
    if (length(missing_annotations) > 0L) {
      cli::cli_abort(
        c(
          "annotation file(s) not found",
          "x" = paste(missing_annotations, collapse = ", ")
        ),
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }
  } else {
    if (!is.character(annotations) || length(annotations) == 0L) {
      cli::cli_abort(
        "annotations must be a non-empty character vector or list",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }

    missing_annotations <- annotations[!file.exists(annotations)]
    if (length(missing_annotations) > 0L) {
      cli::cli_abort(
        c(
          "annotation file(s) not found",
          "x" = paste(missing_annotations, collapse = ", ")
        ),
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }
  }

  # Validate RT files
  if (length(rts) > 0) {
    rts_exist <- purrr::map_lgl(.x = rts, .f = file.exists)
    if (!all(rts_exist)) {
      cli::cli_abort(
        c(
          "retention time file(s) not found",
          "x" = paste(rts[!rts_exist], collapse = ", ")
        ),
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }
  }

  invisible(NULL)
}

#' Filter MS1 Annotations
#'
#' @description Internal helper to remove MS1 annotations that have
#'     quality spectral matches. Only suppresses MS1 when the spectral
#'     evidence is genuinely informative (similarity > threshold and
#'     at least `min_peaks` matched peaks). Experimental library matches
#'     (e.g. GNPS) suppress MS1 more readily than in-silico matches
#'     (e.g. SIRIUS).
#'
#' @param annotation_tables_list Named list of annotation tables
#' @param similarity_threshold Minimum spectral similarity for a spectral
#'     match to suppress the corresponding MS1 annotation (default 0.3)
#' @param min_peaks Minimum matched peaks for a spectral match to suppress
#'     the corresponding MS1 annotation (default 2)
#'
#' @return Data frame with filtered annotations
#' @keywords internal
filter_ms1_redundancy <- function(
  annotation_tables_list,
  similarity_threshold = 0.3,
  min_peaks = 2L
) {
  if (!"ms1" %in% names(annotation_tables_list)) {
    log_debug("No MS1 annotations to filter, combining all annotations")
    return(tidytable::bind_rows(annotation_tables_list))
  }

  log_info("Removing MS1 annotations superseded by quality spectral matches")

  # Extract spectral annotations (all non-MS1)
  spectral_names <- names(annotation_tables_list)[
    names(annotation_tables_list) != "ms1"
  ]
  annotations_tables_spectral <- annotation_tables_list[spectral_names] |>
    tidytable::bind_rows()

  n_spectral <- nrow(annotations_tables_spectral)
  log_debug("Found %d spectral annotations", n_spectral)

  # Create key for anti-join (prefer inchikey_no_stereo when available)
  has_no_stereo <- "candidate_structure_inchikey_no_stereo" %in%
    names(annotations_tables_spectral)
  anti_join_cols <- if (has_no_stereo) {
    c("feature_id", "candidate_structure_inchikey_no_stereo")
  } else {
    c("feature_id", "candidate_structure_inchikey_connectivity_layer")
  }

  # Quality gate: only consider spectral matches that are genuinely

  # informative. This prevents a noisy spectral hit from killing a valid

  # MS1 annotation backed by accurate mass.
  has_similarity <- "candidate_score_similarity" %in%
    names(annotations_tables_spectral)
  has_peaks <- "candidate_count_similarity_peaks_matched" %in%
    names(annotations_tables_spectral)

  quality_spectral <- annotations_tables_spectral
  if (has_similarity) {
    quality_spectral <- quality_spectral |>
      tidytable::filter(
        is.na(candidate_score_similarity) |
          as.numeric(candidate_score_similarity) >= similarity_threshold
      )
  }
  if (has_peaks) {
    quality_spectral <- quality_spectral |>
      tidytable::filter(
        is.na(candidate_count_similarity_peaks_matched) |
          as.integer(candidate_count_similarity_peaks_matched) >= min_peaks
      )
  }

  n_quality <- nrow(quality_spectral)
  log_debug(
    "Quality-gated spectral annotations for MS1 suppression: %d / %d",
    n_quality,
    n_spectral
  )

  spectral_keys <- quality_spectral |>
    tidytable::distinct(tidyselect::any_of(anti_join_cols))

  n_ms1_before <- nrow(annotation_tables_list[["ms1"]])

  # Remove redundant MS1 and combine
  annotation_table <- annotation_tables_list[["ms1"]] |>
    tidytable::anti_join(
      y = spectral_keys,
      by = anti_join_cols
    ) |>
    tidytable::bind_rows(annotations_tables_spectral)

  n_ms1_removed <- n_ms1_before - (nrow(annotation_table) - n_spectral)
  log_info("Removed %d redundant MS1 annotations", n_ms1_removed)

  annotation_table
}

#' Apply RT Filtering
#'
#' @description Internal helper to join RT library information onto
#'     annotations. Computes the RT error but does NOT apply a hard cutoff;
#'     the downstream scoring system uses a sigmoid penalty to handle RT
#'     deviations gracefully, avoiding information-destroying discontinuities.
#'
#' @param features_annotated_table Data frame with features and annotations
#' @param rt_table Data frame with RT standards
#' @param tolerance_rt Numeric RT tolerance in minutes (used only for
#'     deduplication of multiple RT matches for the same compound)
#'
#' @return Data frame with `candidate_structure_error_rt` column added
#' @keywords internal
apply_rt_filter <- function(features_annotated_table, rt_table, tolerance_rt) {
  log_info("Joining RT library and computing RT deltas")

  joined <- features_annotated_table |>
    tidytable::left_join(
      y = rt_table,
      by = "candidate_structure_inchikey_connectivity_layer"
    ) |>
    tidytable::mutate(
      candidate_structure_error_rt = as.numeric(rt) -
        as.numeric(rt_target)
    )

  # When a compound has multiple RT library entries, keep only the

  # best-matching one per (feature, compound, library) triplet.
  n_before_dedup <- nrow(joined)
  dedup_cols <- intersect(
    c(
      "feature_id",
      "candidate_structure_inchikey_connectivity_layer",
      "candidate_library"
    ),
    names(joined)
  )

  # Only deduplicate when there are actual duplicate RT matches
  if (length(dedup_cols) > 0L) {
    joined <- joined |>
      tidytable::arrange(abs(candidate_structure_error_rt)) |>
      tidytable::distinct(
        tidyselect::all_of(dedup_cols),
        .keep_all = TRUE
      )
  }

  n_dedup <- n_before_dedup - nrow(joined)
  if (n_dedup > 0L) {
    log_info(
      "Removed %d duplicate RT library matches (keeping best match per annotation)",
      n_dedup
    )
  }

  log_info(
    "RT deltas computed for %d annotations (no hard cutoff applied; scoring handles RT penalty)",
    sum(!is.na(joined$candidate_structure_error_rt))
  )

  joined |>
    tidytable::select(-tidyselect::any_of(x = c("rt_target", "type")))
}

#' Enforce strong MS1 adduct semantics on non-MS1 annotations
#'
#' @description If a feature has a strong primary MS1 adduct assignment,
#' non-MS1 candidates are constrained to either the same adduct state
#' (`exact_adduct`) or a neutral-mass-consistent rescue (`m_delta_rescued`).
#'
#' @param features_annotated_table Data frame resulting from feature/annotation join
#' @param tolerance_ppm Numeric ppm tolerance for neutral-mass rescue
#' @param tolerance_dalton Numeric absolute tolerance for neutral-mass rescue
#'
#' @return Filtered data frame with `candidate_adduct_match_mode`
#' @keywords internal
enforce_ms1_adduct_semantics <- function(
  features_annotated_table,
  tolerance_ppm = 10,
  tolerance_dalton = 0.005
) {
  if (nrow(features_annotated_table) == 0L) {
    return(features_annotated_table)
  }

  required <- c("feature_id", "candidate_adduct")
  if (!all(required %in% names(features_annotated_table))) {
    return(features_annotated_table)
  }

  out <- features_annotated_table
  if (!"candidate_adduct_match_mode" %in% names(out)) {
    out$candidate_adduct_match_mode <- NA_character_
  }
  if (!"annotation_note" %in% names(out)) {
    out$annotation_note <- NA_character_
  }

  # Parse adduct text once per unique adduct to avoid O(n) parser calls on
  # large annotation tables.
  adduct_values <- unique(stats::na.omit(as.character(out$candidate_adduct)))
  adduct_state_map <- build_adduct_state_key_map(adduct_values) |>
    tidytable::rename(
      candidate_adduct = adduct,
      adduct_state_key = state_key
    )

  is_ms1_row <- if ("candidate_library" %in% names(out)) {
    grepl("ms1", tolower(as.character(out$candidate_library)), fixed = TRUE)
  } else {
    rep(FALSE, nrow(out))
  }

  has_ms1_semantics <- all(
    c(
      "candidate_annotation_level",
      "candidate_evidence_tier"
    ) %in%
      names(out)
  )
  if (has_ms1_semantics) {
    is_ms1_row <- is_ms1_row |
      (!is.na(out$candidate_annotation_level) |
        !is.na(out$candidate_evidence_tier))
  }

  strong_ms1 <- out |>
    tidytable::filter(is_ms1_row) |>
    tidytable::filter(!is.na(candidate_adduct) & nzchar(candidate_adduct))

  if (
    all(
      c("candidate_annotation_level", "candidate_evidence_tier") %in%
        names(strong_ms1)
    )
  ) {
    strong_ms1 <- strong_ms1 |>
      tidytable::filter(
        candidate_annotation_level == "primary",
        candidate_evidence_tier == "supported_strong"
      )
  }

  if (!"mz" %in% names(strong_ms1) || nrow(strong_ms1) == 0L) {
    out$candidate_adduct_match_mode <- tidytable::coalesce(
      out$candidate_adduct_match_mode,
      tidytable::if_else(is_ms1_row, "ms1_reference", "unconstrained")
    )
    return(out)
  }

  if (!"candidate_structure_error_mz" %in% names(strong_ms1)) {
    strong_ms1$candidate_structure_error_mz <- NA_real_
  }
  strong_map <- strong_ms1 |>
    tidytable::left_join(adduct_state_map, by = "candidate_adduct") |>
    tidytable::mutate(
      .strong_state_key = tidytable::coalesce(
        adduct_state_key,
        candidate_adduct
      ),
      .strong_M = compute_candidate_M(
        mz = as.numeric(mz),
        adduct_string = as.character(candidate_adduct)
      )
    ) |>
    tidytable::arrange(
      feature_id,
      abs(as.numeric(candidate_structure_error_mz))
    ) |>
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::select(
      feature_id,
      .strong_adduct = candidate_adduct,
      .strong_state_key,
      .strong_M
    )

  if (nrow(strong_map) == 0L) {
    out$candidate_adduct_match_mode <- tidytable::coalesce(
      out$candidate_adduct_match_mode,
      tidytable::if_else(is_ms1_row, "ms1_reference", "unconstrained")
    )
    return(out)
  }

  n_rows <- nrow(out)
  out <- out |>
    tidytable::left_join(strong_map, by = "feature_id") |>
    tidytable::left_join(adduct_state_map, by = "candidate_adduct") |>
    tidytable::mutate(
      .is_ms1_row = is_ms1_row,
      .is_spectral_row = if ("candidate_library" %in% names(out)) {
        grepl(
          "spectral",
          tolower(as.character(candidate_library)),
          fixed = TRUE
        )
      } else {
        !.is_ms1_row
      },
      .has_strong = !is.na(.strong_state_key),
      .candidate_state_key = tidytable::coalesce(
        adduct_state_key,
        candidate_adduct
      ),
      .exact_adduct_match = !is.na(.candidate_state_key) &
        !is.na(.strong_state_key) &
        .candidate_state_key == .strong_state_key,
      .needs_m_check = !.is_ms1_row &
        .has_strong &
        !is.na(candidate_adduct) &
        !.exact_adduct_match,
      .candidate_M = rep(NA_real_, n_rows),
      .mass_diff = NA_real_,
      .ppm_window = NA_real_,
      .m_delta_rescued = FALSE
    )

  if ("mz" %in% names(out)) {
    idx_m <- which(out$.needs_m_check)
    if (length(idx_m) > 0L) {
      out$.candidate_M[idx_m] <- compute_candidate_M(
        mz = as.numeric(out$mz[idx_m]),
        adduct_string = as.character(out$candidate_adduct[idx_m])
      )
      out$.mass_diff[idx_m] <- abs(
        out$.candidate_M[idx_m] - out$.strong_M[idx_m]
      )
      out$.ppm_window[idx_m] <- tolerance_ppm *
        1e-6 *
        pmax(abs(out$.candidate_M[idx_m]), abs(out$.strong_M[idx_m]))
      out$.m_delta_rescued[idx_m] <- !is.na(out$.candidate_M[idx_m]) &
        !is.na(out$.strong_M[idx_m]) &
        (out$.mass_diff[idx_m] <= out$.ppm_window[idx_m] |
          out$.mass_diff[idx_m] <= tolerance_dalton)
    }
  }

  out <- out |>
    tidytable::mutate(
      .keep_row = .is_ms1_row |
        !.has_strong |
        is.na(candidate_adduct) |
        .exact_adduct_match |
        .m_delta_rescued,
      candidate_adduct_match_mode = tidytable::if_else(
        is.na(candidate_adduct_match_mode),
        tidytable::case_when(
          .is_ms1_row ~ "ms1_reference",
          !.has_strong ~ "unconstrained",
          .exact_adduct_match ~ "exact_adduct",
          .m_delta_rescued ~ "m_delta_rescued",
          TRUE ~ "incompatible"
        ),
        candidate_adduct_match_mode
      ),
      annotation_note = tidytable::if_else(
        !.is_ms1_row & .m_delta_rescued,
        tidytable::coalesce(
          annotation_note,
          "Adduct differs from strong MS1 state; retained by neutral-mass delta consistency"
        ),
        annotation_note
      )
    )

  dropped_n <- out |>
    tidytable::filter(!.keep_row) |>
    nrow()
  dropped_spectral_n <- out |>
    tidytable::filter(!.keep_row, .is_spectral_row) |>
    nrow()
  if (dropped_n > 0L) {
    log_info(
      "Removed %d non-MS1 annotation row(s) incompatible with strong MS1 adduct assignments",
      dropped_n
    )
  }
  if (dropped_spectral_n > 0L) {
    log_info(
      "Removed %d spectral annotation row(s) due to adduct mismatch with strong MS1 assignments",
      dropped_spectral_n
    )
  }

  out_filtered <- out |>
    tidytable::filter(.keep_row) |>
    tidytable::select(
      -tidyselect::any_of(c(
        ".strong_adduct",
        ".strong_state_key",
        ".strong_M",
        ".is_ms1_row",
        ".is_spectral_row",
        ".has_strong",
        ".candidate_state_key",
        "adduct_state_key",
        ".needs_m_check",
        ".candidate_M",
        ".mass_diff",
        ".ppm_window",
        ".exact_adduct_match",
        ".m_delta_rescued",
        ".keep_row"
      ))
    )

  attr(out_filtered, "adduct_semantics_audit") <- list(
    n_input = nrow(out),
    n_removed = dropped_n,
    n_removed_spectral = dropped_spectral_n,
    n_output = nrow(out_filtered)
  )
  out_filtered
}

#' @title Filter annotations
#'
#' @description This function filters initial annotations by removing MS1-only
#'     annotations that also have quality spectral matches (gated on similarity
#'     and matched peaks), and joins retention time library data when available.
#'     RT deltas are computed but no hard cutoff is applied; the downstream
#'     scoring system uses a sigmoid penalty to handle RT deviations gracefully.
#'
#' @include get_params.R
#' @include safe_fread.R
#'
#' @param annotations Character vector or list of paths to prepared annotation
#'     files
#' @param features Character string path to prepared features file.
#' Must contain a \code{feature_id} column. The \code{rt} column is optional;
#'     if absent, RT filtering is skipped even when an RT library is provided.
#' @param rts Character string path to prepared retention time library
#'     (optional)
#' @param output Character string path for filtered annotations output
#' @param tolerance_rt Numeric RT tolerance in minutes (used for deduplication
#'     of multiple RT library matches; no hard cutoff is applied)
#'
#' @return Character string path to the filtered annotations file
#'
#' @family annotation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' dir <- paste0(github, repo)
#' ann <- get_params(step =
#'     "filter_annotations")$files$annotations$prepared$structural[[2L]] |>
#'   gsub(pattern = ".gz", replacement = "", fixed = TRUE)
#' features <- get_params(step = "filter_annotations")$files$features$prepared
#'     |>
#'   gsub(pattern = ".gz", replacement = "", fixed = TRUE)
#' rts <- get_params(step =
#'     "filter_annotations")$files$libraries$temporal$prepared |>
#'   gsub(pattern = ".gz", replacement = "", fixed = TRUE)
#' get_file(url = paste0(dir, annotations), export = annotations)
#' get_file(url = paste0(dir, features), export = features)
#' get_file(url = paste0(dir, rts), export = rts)
#' filter_annotations(
#'   annotations = ann,
#'   features = features,
#'   rts = rts
#' )
#' unlink("data", recursive = TRUE)
#' }
filter_annotations <- function(
  annotations = get_params(
    step = "filter_annotations"
  )$files$annotations$prepared$structural,
  features = get_params(step = "filter_annotations")$files$features$prepared,
  rts = get_params(
    step = "filter_annotations"
  )$files$libraries$temporal$prepared,
  output = get_params(step = "filter_annotations")$files$annotations$filtered,
  tolerance_rt = get_params(
    step = "filter_annotations"
  )$ms$tolerances$rt$library
) {
  # Start operation logging
  ctx <- log_operation(
    "filter_annotations",
    n_annotation_files = length(unlist(annotations)),
    tolerance_rt = tolerance_rt
  )

  # Input Validation ----

  validate_filter_annotations_inputs(
    annotations = annotations,
    features = features,
    rts = rts,
    output = output,
    tolerance_rt = tolerance_rt
  )

  # Normalize RT input
  if (length(rts) == 0) {
    rts <- NULL
  }

  # Load and Process Data ----

  log_info("Filtering annotations")
  if (
    is.numeric(tolerance_rt) &&
      length(tolerance_rt) == 1L &&
      !is.na(tolerance_rt)
  ) {
    log_debug("RT tolerance: %.2f minutes", tolerance_rt)
  }

  features_table <- safe_fread(
    file = features,
    file_type = "features table",
    required_cols = "feature_id",
    colClasses = "character",
    na.strings = c("", "NA")
  )

  has_rt <- "rt" %in% names(features_table)
  distinct_cols <- c("feature_id", if (has_rt) "rt", "mz")
  distinct_cols <- intersect(distinct_cols, names(features_table))
  features_table <- features_table |>
    tidytable::distinct(tidyselect::all_of(distinct_cols))

  n_features <- nrow(features_table)
  log_info(
    "Processing %d unique features for annotation filtering",
    n_features
  )

  # Load and Merge Annotations ----

  log_debug("Loading %d annotation file(s)", length(annotations))
  annotation_tables_list <- purrr::map2(
    .x = annotations,
    .y = seq_along(annotations),
    .f = ~ safe_fread(
      file = .x,
      file_type = paste0("annotation file ", .y),
      na.strings = c("", "NA"),
      colClasses = "character"
    )
  )

  # Filter MS1 redundancy
  annotation_table <- filter_ms1_redundancy(annotation_tables_list)
  rm(annotation_tables_list)

  n_total_annotations <- nrow(annotation_table)
  log_info(
    "Total annotations after MS1 deduplication: %d",
    n_total_annotations
  )

  # Apply RT Filtering if Library Available ----

  features_annotated_table_1 <- features_table |>
    tidytable::left_join(y = annotation_table)
  rm(annotation_table)

  n_before_adduct_semantics <- nrow(features_annotated_table_1)
  features_annotated_table_1 <- enforce_ms1_adduct_semantics(
    features_annotated_table_1
  )
  adduct_semantics_audit <- attr(
    features_annotated_table_1,
    "adduct_semantics_audit"
  )
  n_after_adduct_semantics <- nrow(features_annotated_table_1)
  n_removed_adduct_semantics <- max(
    0L,
    n_before_adduct_semantics - n_after_adduct_semantics
  )
  n_removed_spectral_adduct_mismatch <- if (
    is.list(adduct_semantics_audit) &&
      !is.null(adduct_semantics_audit$n_removed_spectral)
  ) {
    as.integer(adduct_semantics_audit$n_removed_spectral)
  } else {
    0L
  }
  log_info(
    paste0(
      "Adduct-semantics filter: before=%d, removed_total=%d, ",
      "removed_spectral_mismatch=%d, after=%d"
    ),
    n_before_adduct_semantics,
    n_removed_adduct_semantics,
    n_removed_spectral_adduct_mismatch,
    n_after_adduct_semantics
  )

  if (!is.null(rts) && !has_rt) {
    log_warn(
      paste(
        "RT library provided but features table has no 'rt' column.",
        "Skipping RT filtering."
      )
    )
    rts <- NULL
  }

  if (!is.null(rts)) {
    rt_table <- purrr::map2(
      .x = rts,
      .y = seq_along(rts),
      .f = ~ safe_fread(
        file = .x,
        file_type = paste0("retention time library ", .y),
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    ) |>
      tidytable::bind_rows()

    # Robust rename: support either 'rt' or pre-renamed 'rt_target'
    if ("rt" %in% names(rt_table)) {
      rt_table <- rt_table |>
        tidytable::rename(rt_target = rt)
    } else if (!"rt_target" %in% names(rt_table)) {
      cli::cli_abort(
        "retention time library must contain column 'rt' or 'rt_target'",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }

    n_rt_standards <- nrow(rt_table)
    log_debug("Loaded %d retention time standards", n_rt_standards)

    features_annotated_table_2 <- apply_rt_filter(
      features_annotated_table_1,
      rt_table,
      tolerance_rt
    )
  } else {
    log_debug("No RT library provided, skipping RT filtering")
    features_annotated_table_2 <- features_annotated_table_1 |>
      tidytable::mutate(candidate_structure_error_rt = NA)
  }

  n_dedup_rt <- nrow(features_annotated_table_1) -
    nrow(features_annotated_table_2)
  if (n_dedup_rt > 0L) {
    log_info(
      "Removed %d duplicate RT library matches during join",
      n_dedup_rt
    )
  }
  rm(features_annotated_table_1)

  ## in case some features had a single filtered annotation
  join_cols <- intersect(
    names(features_table),
    names(features_annotated_table_2)
  )
  final_table <- features_table |>
    tidytable::left_join(y = features_annotated_table_2, by = join_cols)

  rm(
    features_table,
    features_annotated_table_2
  )

  export_params(
    parameters = get_params(step = "filter_annotations"),
    step = "filter_annotations"
  )
  export_output(x = final_table, file = output[[1L]])

  log_complete(ctx, n_filtered = nrow(final_table))

  rm(final_table)
  output[[1L]]
}
