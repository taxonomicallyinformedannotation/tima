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
#' @include logs_utils.R
#' @include annotate_masses_consistency.R
#' @include export_params.R
#' @include export_output.R
#' @include clean_chemo_preprocessing.R
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
#' @concept EDAM:operation_3695 Filtering
#' @concept EDAM:operation_3803 Natural product identification
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
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::select(tidyselect::all_of(distinct_cols))

  n_features <- nrow(features_table)
  log_info(
    "Processing %d unique features for annotation filtering",
    n_features
  )

  # Load and Merge Annotations ----

  log_debug("Loading %d annotation file(s)", length(annotations))
  annotation_tables_list <- Map(
    function(x, y) {
      safe_fread(
        file = x,
        file_type = paste0("annotation file ", y),
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    },
    annotations,
    seq_along(annotations)
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

  extra_feat_cols <- setdiff(names(features_table), "feature_id")
  if (
    length(extra_feat_cols) > 0L &&
      !any(extra_feat_cols %in% names(annotation_table))
  ) {
    f_idx <- fastmatch::fmatch(
      annotation_table$feature_id,
      features_table$feature_id
    )
    for (col in extra_feat_cols) {
      data.table::set(
        annotation_table,
        j = col,
        value = features_table[[col]][f_idx]
      )
    }
    features_annotated_table_1 <- annotation_table
  } else {
    features_annotated_table_1 <- annotation_table |>
      tidytable::left_join(y = features_table, by = "feature_id")
  }
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
    rt_table <- Map(
      function(x, y) {
        safe_fread(
          file = x,
          file_type = paste0("retention time library ", y),
          na.strings = c("", "NA"),
          colClasses = "character"
        )
      },
      rts,
      seq_along(rts)
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
    features_annotated_table_2 <- features_annotated_table_1
    features_annotated_table_2$candidate_structure_error_rt <- NA_real_
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
  missing_feature_ids <- setdiff(
    features_table$feature_id,
    features_annotated_table_2$feature_id
  )
  if (length(missing_feature_ids) > 0L) {
    unannotated <- features_table |>
      tidytable::filter(feature_id %in% missing_feature_ids)
    final_table <- tidytable::bind_rows(
      features_annotated_table_2,
      unannotated
    )
  } else {
    final_table <- features_annotated_table_2
  }

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
    rts_exist <- file.exists(unlist(rts))
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

  filter_cols <- intersect(
    c(
      anti_join_cols,
      "candidate_score_similarity",
      "candidate_count_similarity_peaks_matched"
    ),
    names(annotations_tables_spectral)
  )
  quality_spectral <- annotations_tables_spectral |>
    tidytable::select(tidyselect::all_of(filter_cols))
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
  rm(quality_spectral)

  ms1_tbl <- annotation_tables_list[["ms1"]]
  n_ms1_before <- nrow(ms1_tbl)

  join_cols <- intersect(
    anti_join_cols,
    intersect(names(ms1_tbl), names(spectral_keys))
  )
  if (
    n_ms1_before > 0L &&
      nrow(spectral_keys) > 0L &&
      length(join_cols) == 2L
  ) {
    c1 <- join_cols[[1L]]
    c2 <- join_cols[[2L]]
    u_fids <- unique(spectral_keys[[c1]])
    in_feat <- !is.na(fastmatch::fmatch(ms1_tbl[[c1]], u_fids))
    idx_cand <- which(in_feat)
    if (length(idx_cand) > 0L) {
      s1 <- ifelse(is.na(spectral_keys[[c1]]), "\u0001", spectral_keys[[c1]])
      s2 <- ifelse(is.na(spectral_keys[[c2]]), "\u0001", spectral_keys[[c2]])
      spec_k <- paste(s1, s2, sep = "\r")
      m1 <- ifelse(
        is.na(ms1_tbl[[c1]][idx_cand]),
        "\u0001",
        ms1_tbl[[c1]][idx_cand]
      )
      m2 <- ifelse(
        is.na(ms1_tbl[[c2]][idx_cand]),
        "\u0001",
        ms1_tbl[[c2]][idx_cand]
      )
      ms1_k <- paste(m1, m2, sep = "\r")
      matched <- !is.na(fastmatch::fmatch(ms1_k, spec_k))
      if (any(matched)) {
        keep_mask <- !logical(n_ms1_before)
        keep_mask[idx_cand[matched]] <- FALSE
        ms1_tbl <- ms1_tbl[keep_mask, ]
      }
    }
  } else if (n_ms1_before > 0L && nrow(spectral_keys) > 0L) {
    ms1_tbl <- tidytable::anti_join(
      ms1_tbl,
      y = spectral_keys,
      by = anti_join_cols
    )
  }

  n_ms1_removed <- n_ms1_before - nrow(ms1_tbl)
  log_info("Removed %d redundant MS1 annotations", n_ms1_removed)

  tidytable::bind_rows(ms1_tbl, annotations_tables_spectral)
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

  ik_col <- "candidate_structure_inchikey_connectivity_layer"
  if (
    nrow(rt_table) == 0L ||
      !ik_col %in% names(features_annotated_table) ||
      !ik_col %in% names(rt_table)
  ) {
    features_annotated_table$candidate_structure_error_rt <- NA_real_
    log_info(
      "RT deltas computed for 0 annotations (no hard cutoff applied; scoring handles RT penalty)"
    )
    return(
      features_annotated_table |>
        tidytable::select(-tidyselect::any_of(x = c("rt_target", "type")))
    )
  }

  # Fast path when each compound has at most one RT standard:
  # direct fastmatch lookup avoids copying/sorting 10M+ annotation rows
  rt_iks <- rt_table[[ik_col]]
  if (!anyDuplicated(rt_iks)) {
    rt_idx <- fastmatch::fmatch(features_annotated_table[[ik_col]], rt_iks)
    err_rt <- if (!all(is.na(rt_idx))) {
      as.numeric(features_annotated_table$rt) -
        as.numeric(rt_table$rt_target)[rt_idx]
    } else {
      rep(NA_real_, nrow(features_annotated_table))
    }
    features_annotated_table$candidate_structure_error_rt <- err_rt
    log_info(
      "RT deltas computed for %d annotations (no hard cutoff applied; scoring handles RT penalty)",
      sum(!is.na(err_rt))
    )
    return(
      features_annotated_table |>
        tidytable::select(-tidyselect::any_of(x = c("rt_target", "type")))
    )
  }

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
  tolerance_dalton = 0.0025
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
    unique_libs <- unique(stats::na.omit(as.character(out$candidate_library)))
    ms1_libs <- unique_libs[grepl("ms1", tolower(unique_libs), fixed = TRUE)]
    out$candidate_library %in% ms1_libs
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

  strong_idx <- which(
    is_ms1_row & !is.na(out$candidate_adduct) & nzchar(out$candidate_adduct)
  )

  if (has_ms1_semantics && length(strong_idx) > 0L) {
    is_primary_strong <- out$candidate_annotation_level[strong_idx] ==
      "primary" &
      out$candidate_evidence_tier[strong_idx] == "supported_strong"
    strong_idx <- strong_idx[!is.na(is_primary_strong) & is_primary_strong]
  }

  if (length(strong_idx) == 0L || !"mz" %in% names(out)) {
    out$candidate_adduct_match_mode <- tidytable::coalesce(
      out$candidate_adduct_match_mode,
      tidytable::if_else(is_ms1_row, "ms1_reference", "unconstrained")
    )
    return(out)
  }

  # Deduplicate strong MS1 rows to 1 row per feature_id BEFORE computing M:
  # avoids sorting and parsing 9M+ MS1 structure matches when each feature
  # has a single primary strong adduct. Uses integer encoding + radix sort
  # to avoid expensive string paste()/order() on millions of rows.
  u_fids <- unique(out$feature_id)
  f_int <- fastmatch::fmatch(out$feature_id, u_fids)
  cand_adducts <- out$candidate_adduct
  m_add <- fastmatch::fmatch(cand_adducts, adduct_state_map$candidate_adduct)
  n_add_map <- max(1L, nrow(adduct_state_map))

  sf_int <- f_int[strong_idx]
  sa_int <- m_add[strong_idx]
  fa_int <- (sf_int - 1L) * n_add_map + tidytable::coalesce(sa_int, 0L)
  u_fa_pos <- which(!duplicated(fa_int))
  u_sf_int <- sf_int[u_fa_pos]

  chosen_strong_idx <- if (!anyDuplicated(u_sf_int)) {
    strong_idx[u_fa_pos]
  } else {
    dup_f_int <- unique(u_sf_int[duplicated(u_sf_int)])
    is_dup_u <- !is.na(fastmatch::fmatch(u_sf_int, dup_f_int))
    single_idx <- strong_idx[u_fa_pos[!is_dup_u]]

    in_dup_rel <- which(!is.na(fastmatch::fmatch(sf_int, dup_f_int)))
    in_dup <- strong_idx[in_dup_rel]
    err_vals <- if ("candidate_structure_error_mz" %in% names(out)) {
      abs(suppressWarnings(as.numeric(
        out$candidate_structure_error_mz[in_dup]
      )))
    } else {
      rep(NA_real_, length(in_dup))
    }
    sf_dup <- sf_int[in_dup_rel]
    ord <- order(sf_dup, err_vals, na.last = TRUE, method = "radix")
    in_dup_sorted <- in_dup[ord]
    sf_dup_sorted <- sf_dup[ord]
    multi_idx <- in_dup_sorted[!duplicated(sf_dup_sorted)]
    c(single_idx, multi_idx)
  }
  rm(sf_int, sa_int, fa_int, u_fa_pos, u_sf_int)

  if (length(chosen_strong_idx) == 0L) {
    out$candidate_adduct_match_mode <- tidytable::coalesce(
      out$candidate_adduct_match_mode,
      tidytable::if_else(is_ms1_row, "ms1_reference", "unconstrained")
    )
    return(out)
  }

  strong_f_int <- f_int[chosen_strong_idx]
  strong_adducts <- as.character(cand_adducts[chosen_strong_idx])
  strong_m_add <- m_add[chosen_strong_idx]
  strong_state_col <- tidytable::coalesce(
    adduct_state_map$adduct_state_key[strong_m_add],
    strong_adducts
  )
  strong_M_col <- compute_candidate_M(
    mz = as.numeric(out$mz[chosen_strong_idx]),
    adduct_string = strong_adducts
  )

  is_spectral_row <- if ("candidate_library" %in% names(out)) {
    unique_libs <- unique(stats::na.omit(as.character(out$candidate_library)))
    spectral_libs <- unique_libs[
      grepl("spectral", tolower(unique_libs), fixed = TRUE)
    ]
    out$candidate_library %in% spectral_libs
  } else {
    !is_ms1_row
  }

  m_feat <- fastmatch::fmatch(f_int, strong_f_int)
  has_strong <- !is.na(m_feat)
  strong_state_keys <- strong_state_col[m_feat]

  candidate_state_keys <- tidytable::coalesce(
    adduct_state_map$adduct_state_key[m_add],
    cand_adducts
  )

  exact_adduct_match <- has_strong &
    !is.na(candidate_state_keys) &
    (candidate_state_keys == strong_state_keys)
  exact_adduct_match[is.na(exact_adduct_match)] <- FALSE

  needs_m_check <- !is_ms1_row &
    has_strong &
    !is.na(cand_adducts) &
    !exact_adduct_match

  m_delta_rescued <- rep(FALSE, nrow(out))
  idx_m <- which(needs_m_check)
  if (length(idx_m) > 0L && "mz" %in% names(out)) {
    cand_M <- compute_candidate_M(
      mz = out$mz[idx_m],
      adduct_string = cand_adducts[idx_m]
    )
    strong_M_vals <- strong_M_col[m_feat[idx_m]]
    mass_diff <- abs(cand_M - strong_M_vals)
    ppm_window <- tolerance_ppm * 1e-6 * pmax(abs(cand_M), abs(strong_M_vals))
    rescued <- !is.na(cand_M) &
      !is.na(strong_M_vals) &
      (mass_diff <= ppm_window | mass_diff <= tolerance_dalton)
    rescued[is.na(rescued)] <- FALSE
    m_delta_rescued[idx_m] <- rescued
  }

  keep_row <- is_ms1_row |
    !has_strong |
    is.na(out$candidate_adduct) |
    exact_adduct_match |
    m_delta_rescued

  if (all(is.na(out$candidate_adduct_match_mode))) {
    mm <- rep("incompatible", nrow(out))
    mm[!has_strong & !is_ms1_row] <- "unconstrained"
    mm[is_ms1_row] <- "ms1_reference"
    mm[exact_adduct_match] <- "exact_adduct"
    mm[m_delta_rescued] <- "m_delta_rescued"
    out$candidate_adduct_match_mode <- mm
    rm(mm)
  } else {
    missing_mode_idx <- which(is.na(out$candidate_adduct_match_mode))
    if (length(missing_mode_idx) > 0L) {
      mm <- character(length(missing_mode_idx))
      mm[] <- "incompatible"
      mm[!has_strong[missing_mode_idx] & !is_ms1_row[missing_mode_idx]] <-
        "unconstrained"
      mm[is_ms1_row[missing_mode_idx]] <- "ms1_reference"
      mm[exact_adduct_match[missing_mode_idx]] <- "exact_adduct"
      mm[m_delta_rescued[missing_mode_idx]] <- "m_delta_rescued"
      out$candidate_adduct_match_mode[missing_mode_idx] <- mm
      rm(mm)
    }
  }

  rescued_note_idx <- which(!is_ms1_row & m_delta_rescued)
  if (length(rescued_note_idx) > 0L) {
    out$annotation_note[rescued_note_idx] <- tidytable::coalesce(
      out$annotation_note[rescued_note_idx],
      "Adduct differs from strong MS1 state; retained by neutral-mass delta consistency"
    )
  }

  dropped_n <- sum(!keep_row)
  dropped_spectral_n <- if (dropped_n > 0L) {
    sum(!keep_row & is_spectral_row)
  } else {
    0L
  }

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

  out_filtered <- if (dropped_n == 0L) out else out[keep_row, ]
  attr(out_filtered, "adduct_semantics_audit") <- list(
    n_input = nrow(out),
    n_removed = dropped_n,
    n_removed_spectral = dropped_spectral_n,
    n_output = nrow(out_filtered)
  )
  out_filtered
}
