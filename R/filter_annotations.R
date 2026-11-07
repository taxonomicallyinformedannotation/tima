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

  log_info("Filtering annotations")
  if (
    is.numeric(tolerance_rt) &&
      length(tolerance_rt) == 1L &&
      !is.na(tolerance_rt)
  ) {
    log_debug("RT tolerance: %.2f minutes", tolerance_rt)
  }

  feat_hdr <- names(safe_fread(
    file = features,
    file_type = "features table",
    required_cols = "feature_id",
    nrows = 0L
  ))
  has_rt <- "rt" %in% feat_hdr
  distinct_cols <- intersect(
    c("feature_id", if (has_rt) "rt", "mz"),
    feat_hdr
  )
  features_table <- safe_fread(
    file = features,
    file_type = "features table",
    required_cols = "feature_id",
    select = distinct_cols,
    colClasses = "character",
    na.strings = c("", "NA")
  )
  if (anyDuplicated(features_table$feature_id) > 0L) {
    features_table <- features_table[!duplicated(features_table$feature_id), ]
  }

  n_features <- nrow(features_table)
  log_info(
    "Processing %d unique features for annotation filtering",
    n_features
  )

  log_debug("Loading %d annotation file(s)", length(annotations))
  annotation_table <- filter_ms1_redundancy(
    Map(
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
  )

  n_total_annotations <- nrow(annotation_table)
  log_info(
    "Total annotations after MS1 deduplication: %d",
    n_total_annotations
  )

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
  features_annotated_table_1 <- enforce_ms1_adduct_semantics({
    tmp_annotated <- features_annotated_table_1
    features_annotated_table_1 <- NULL
    tmp_annotated
  })
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

  n_before_rt <- nrow(features_annotated_table_1)
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
      {
        tmp_rt_in <- features_annotated_table_1
        features_annotated_table_1 <- NULL
        tmp_rt_in
      },
      rt_table,
      tolerance_rt
    )
    rm(rt_table)
  } else {
    log_debug("No RT library provided, skipping RT filtering")
    features_annotated_table_2 <- features_annotated_table_1
    features_annotated_table_1 <- NULL
    data.table::set(
      features_annotated_table_2,
      j = "candidate_structure_error_rt",
      value = NA_real_
    )
  }

  n_dedup_rt <- n_before_rt - nrow(features_annotated_table_2)
  if (n_dedup_rt > 0L) {
    log_info(
      "Removed %d duplicate RT library matches during join",
      n_dedup_rt
    )
  }
  rm(features_annotated_table_1)

  ## in case some features had a single filtered annotation
  u_ann_f <- unique(features_annotated_table_2$feature_id)
  missing_idx <- which(is.na(
    fastmatch::fmatch(features_table$feature_id, u_ann_f)
  ))
  rm(u_ann_f)
  if (length(missing_idx) > 0L) {
    unannotated <- features_table[missing_idx, ]
    rm(features_table)
    final_table <- tidytable::bind_rows(
      {
        tmp_ann2 <- features_annotated_table_2
        features_annotated_table_2 <- NULL
        tmp_ann2
      },
      unannotated
    )
    rm(unannotated, features_annotated_table_2)
  } else {
    rm(features_table)
    final_table <- features_annotated_table_2
    rm(features_annotated_table_2)
  }

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

  if (!is.character(output) || length(output) != 1L) {
    cli::cli_abort(
      "output must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

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

  ms1_tbl <- annotation_tables_list[["ms1"]]
  annotation_tables_list[["ms1"]] <- NULL
  annotations_tables_spectral <- tidytable::bind_rows(annotation_tables_list)
  rm(annotation_tables_list)

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

  qual_mask <- rep.int(TRUE, n_spectral)
  zero_mask <- rep.int(FALSE, n_spectral)
  if (has_similarity && n_spectral > 0L) {
    sim_col <- annotations_tables_spectral$candidate_score_similarity
    sim_num <- .as_numeric_dedup(sim_col)
    qual_mask <- qual_mask &
      (is.na(sim_col) | (!is.na(sim_num) & sim_num >= similarity_threshold))
    zero_mask <- !is.na(sim_num) & sim_num <= 0
  }
  if (has_peaks && n_spectral > 0L) {
    pks_col <- annotations_tables_spectral$candidate_count_similarity_peaks_matched
    pks_num <- .as_numeric_dedup(pks_col)
    qual_mask <- qual_mask &
      (is.na(pks_col) | (!is.na(pks_num) & as.integer(pks_num) >= min_peaks))
  }
  zero_mask <- zero_mask & !qual_mask

  qual_idx <- which(qual_mask)
  rm(qual_mask)
  n_quality <- length(qual_idx)
  log_debug(
    "Quality-gated spectral annotations for MS1 suppression: %d / %d",
    n_quality,
    n_spectral
  )

  n_ms1_before <- nrow(ms1_tbl)
  join_cols <- intersect(
    anti_join_cols,
    intersect(names(ms1_tbl), names(annotations_tables_spectral))
  )

  if (
    n_ms1_before > 0L &&
      n_quality > 0L &&
      length(join_cols) == 2L
  ) {
    c1 <- join_cols[[1L]]
    c2 <- join_cols[[2L]]
    s1 <- annotations_tables_spectral[[c1]][qual_idx]
    s2 <- annotations_tables_spectral[[c2]][qual_idx]
    u_fids <- unique(s1)
    m_fid <- if (anyNA(u_fids)) {
      match(ms1_tbl[[c1]], u_fids)
    } else {
      fastmatch::fmatch(ms1_tbl[[c1]], u_fids)
    }
    idx_cand <- which(!is.na(m_fid))
    if (length(idx_cand) > 0L) {
      u_iks <- unique(s2)
      m_ik <- if (anyNA(u_iks)) {
        match(ms1_tbl[[c2]][idx_cand], u_iks)
      } else {
        fastmatch::fmatch(ms1_tbl[[c2]][idx_cand], u_iks)
      }
      in_both <- which(!is.na(m_ik))
      if (length(in_both) > 0L) {
        idx_sub <- idx_cand[in_both]
        fid_id_m <- m_fid[idx_sub]
        ik_id_m <- m_ik[in_both]
        fid_id_s <- if (anyNA(u_fids)) {
          match(s1, u_fids)
        } else {
          fastmatch::fmatch(s1, u_fids)
        }
        ik_id_s <- if (anyNA(u_iks)) {
          match(s2, u_iks)
        } else {
          fastmatch::fmatch(s2, u_iks)
        }
        n_ik <- length(u_iks)
        if (as.double(length(u_fids)) * n_ik < .Machine$integer.max) {
          spec_k <- unique((fid_id_s - 1L) * n_ik + ik_id_s)
          ms1_k <- (fid_id_m - 1L) * n_ik + ik_id_m
        } else {
          spec_k <- unique(as.double(fid_id_s - 1L) * n_ik + ik_id_s)
          ms1_k <- as.double(fid_id_m - 1L) * n_ik + ik_id_m
        }
        matched <- !is.na(fastmatch::fmatch(ms1_k, spec_k))
        if (any(matched)) {
          keep_mask <- !logical(n_ms1_before)
          keep_mask[idx_sub[matched]] <- FALSE
          ms1_tbl <- ms1_tbl[keep_mask, ]
        }
      }
    }
  } else if (
    n_ms1_before > 0L &&
      n_quality > 0L &&
      length(join_cols) > 0L
  ) {
    spectral_keys <- tidytable::as_tidytable(
      as.list(annotations_tables_spectral)[join_cols]
    )[qual_idx, ] |>
      tidytable::distinct()
    ms1_tbl <- tidytable::anti_join(
      ms1_tbl,
      y = spectral_keys,
      by = join_cols
    )
  }

  n_ms1_removed <- n_ms1_before - nrow(ms1_tbl)
  log_info("Removed %d redundant MS1 annotations", n_ms1_removed)

  # Transfer measured zero-similarity (0.00 / 0 peaks) spectral results onto
  # matching MS1 rows in-place and drop the standalone 0.00 spectral rows so
  # we preserve the 0.00 vs NA distinction without duplicating rows.
  if (any(zero_mask)) {
    zero_idx <- which(zero_mask)
    if (nrow(ms1_tbl) > 0L && length(join_cols) == 2L) {
      c1 <- join_cols[[1L]]
      c2 <- join_cols[[2L]]
      z1 <- annotations_tables_spectral[[c1]][zero_idx]
      z2 <- annotations_tables_spectral[[c2]][zero_idx]
      u_zf <- unique(z1)
      m_zf <- if (anyNA(u_zf)) {
        match(ms1_tbl[[c1]], u_zf)
      } else {
        fastmatch::fmatch(ms1_tbl[[c1]], u_zf)
      }
      z_cand <- which(!is.na(m_zf))
      if (length(z_cand) > 0L) {
        u_zi <- unique(z2)
        m_zi <- if (anyNA(u_zi)) {
          match(ms1_tbl[[c2]][z_cand], u_zi)
        } else {
          fastmatch::fmatch(ms1_tbl[[c2]][z_cand], u_zi)
        }
        z_both <- which(!is.na(m_zi))
        if (length(z_both) > 0L) {
          z_sub <- z_cand[z_both]
          zf_m <- m_zf[z_sub]
          zi_m <- m_zi[z_both]
          zf_s <- if (anyNA(u_zf)) {
            match(z1, u_zf)
          } else {
            fastmatch::fmatch(z1, u_zf)
          }
          zi_s <- if (anyNA(u_zi)) {
            match(z2, u_zi)
          } else {
            fastmatch::fmatch(z2, u_zi)
          }
          n_zi <- length(u_zi)
          z_spec_k <- as.double(zf_s - 1L) * n_zi + zi_s
          z_ms1_k <- as.double(zf_m - 1L) * n_zi + zi_m
          z_match <- fastmatch::fmatch(z_ms1_k, z_spec_k)
          has_z_match <- !is.na(z_match)
          if (any(has_z_match)) {
            ms1_hit_rows <- z_sub[has_z_match]
            spec_hit_rows <- zero_idx[z_match[has_z_match]]
            ms1_tbl <- tidytable::as_tidytable(as.list(ms1_tbl))
            for (z_col in intersect(
              c(
                "candidate_score_similarity",
                "candidate_score_similarity_forward",
                "candidate_score_similarity_reverse",
                "candidate_count_similarity_peaks_matched",
                "candidate_spectrum_id",
                "candidate_spectrum_entropy"
              ),
              names(annotations_tables_spectral)
            )) {
              spec_v <- annotations_tables_spectral[[z_col]][spec_hit_rows]
              if (z_col %in% names(ms1_tbl)) {
                cur_v <- ms1_tbl[[z_col]]
                na_rel <- is.na(cur_v[ms1_hit_rows])
                if (any(na_rel)) {
                  fill_rows <- ms1_hit_rows[na_rel]
                  fill_vals <- spec_v[na_rel]
                  if (is.character(cur_v)) {
                    cur_v[fill_rows] <- as.character(fill_vals)
                  } else if (is.integer(cur_v)) {
                    cur_v[fill_rows] <- as.integer(fill_vals)
                  } else {
                    cur_v[fill_rows] <- as.numeric(fill_vals)
                  }
                  data.table::set(ms1_tbl, j = z_col, value = cur_v)
                }
              } else {
                new_v <- spec_v[rep.int(NA_integer_, nrow(ms1_tbl))]
                new_v[ms1_hit_rows] <- spec_v
                data.table::set(ms1_tbl, j = z_col, value = new_v)
              }
            }
          }
        }
      }
    }
    annotations_tables_spectral <- annotations_tables_spectral[!zero_mask, ]
  }
  rm(zero_mask)

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
      .as_numeric_dedup(features_annotated_table$rt) -
        .as_numeric_dedup(rt_table$rt_target)[rt_idx]
    } else {
      rep(NA_real_, nrow(features_annotated_table))
    }
    features_annotated_table$candidate_structure_error_rt <- err_rt
    log_info(
      "RT deltas computed for %d annotations (no hard cutoff applied; scoring handles RT penalty)",
      sum(!is.na(err_rt))
    )
    drop_rt_cols <- intersect(
      c("rt_target", "type"),
      names(features_annotated_table)
    )
    if (length(drop_rt_cols) > 0L) {
      features_annotated_table <- features_annotated_table |>
        tidytable::select(-tidyselect::any_of(x = drop_rt_cols))
    }
    return(features_annotated_table)
  }

  extra_rt_cols <- setdiff(names(rt_table), c(ik_col, "rt_target", "type"))
  if (length(extra_rt_cols) == 0L && !anyNA(rt_iks)) {
    ann_iks <- features_annotated_table[[ik_col]]
    ann_rt_num <- .as_numeric_dedup(features_annotated_table$rt)
    rt_target_num <- .as_numeric_dedup(rt_table$rt_target)

    dup_iks <- unique(rt_iks[duplicated(rt_iks)])
    is_dup_rt <- !is.na(fastmatch::fmatch(rt_iks, dup_iks))

    single_iks <- rt_iks[!is_dup_rt]
    single_targets <- rt_target_num[!is_dup_rt]
    m_single <- fastmatch::fmatch(ann_iks, single_iks)
    err_rt <- ann_rt_num - single_targets[m_single]

    m_dup <- fastmatch::fmatch(ann_iks, dup_iks)
    idx_in_dup <- which(!is.na(m_dup))
    n_extra_join <- 0L
    if (length(idx_in_dup) > 0L) {
      dup_rt_idx <- which(is_dup_rt)
      dup_grp <- fastmatch::fmatch(rt_iks[dup_rt_idx], dup_iks)
      targets_by_dup <- split(rt_target_num[dup_rt_idx], dup_grp)
      counts_by_dup <- lengths(targets_by_dup, use.names = FALSE)

      sub_rt <- ann_rt_num[idx_in_dup]
      sub_d <- m_dup[idx_in_dup]
      n_extra_join <- sum(counts_by_dup[sub_d] - 1L)

      u_rt_vals <- unique(sub_rt)
      rt_id <- match(sub_rt, u_rt_vals)
      pair_key <- (sub_d - 1L) * length(u_rt_vals) + rt_id
      u_pos <- which(!duplicated(pair_key))
      u_grp <- fastmatch::fmatch(pair_key, pair_key[u_pos])

      u_rt <- sub_rt[u_pos]
      u_d <- sub_d[u_pos]
      u_err <- vapply(
        seq_along(u_pos),
        function(i) {
          r_val <- u_rt[[i]]
          if (is.na(r_val)) {
            return(NA_real_)
          }
          t_vec <- targets_by_dup[[u_d[[i]]]]
          diffs <- r_val - t_vec
          adiffs <- abs(diffs)
          if (all(is.na(adiffs))) {
            return(NA_real_)
          }
          diffs[[which.min(adiffs)]]
        },
        numeric(1L)
      )
      err_rt[idx_in_dup] <- u_err[u_grp]
    }

    dedup_cols <- intersect(
      c(
        "feature_id",
        "candidate_structure_inchikey_connectivity_layer",
        "candidate_library"
      ),
      names(features_annotated_table)
    )
    out <- features_annotated_table
    out$candidate_structure_error_rt <- err_rt
    n_pre_dup <- 0L
    if (length(dedup_cols) > 0L && nrow(out) > 1L) {
      c1 <- out[[dedup_cols[[1L]]]]
      u1 <- unique(c1)
      comp_key <- as.double(
        (if (anyNA(u1)) match(c1, u1) else fastmatch::fmatch(c1, u1)) - 1L
      )
      if (length(dedup_cols) >= 2L) {
        for (k_i in 2:length(dedup_cols)) {
          ck <- out[[dedup_cols[[k_i]]]]
          uk <- unique(ck)
          idk <- if (anyNA(uk)) match(ck, uk) else fastmatch::fmatch(ck, uk)
          comp_key <- comp_key * (length(uk) + 1L) + (idk - 1L)
        }
      }
      if (anyDuplicated(comp_key) > 0L) {
        n_before_out <- nrow(out)
        ord <- order(abs(err_rt), na.last = TRUE, method = "radix")
        out <- out[ord, ]
        comp_key_ord <- comp_key[ord]
        out <- out[!duplicated(comp_key_ord), ]
        n_pre_dup <- n_before_out - nrow(out)
      }
    }
    n_dedup <- n_extra_join + n_pre_dup
    if (n_dedup > 0L) {
      log_info(
        "Removed %d duplicate RT library matches (keeping best match per annotation)",
        n_dedup
      )
    }
    log_info(
      "RT deltas computed for %d annotations (no hard cutoff applied; scoring handles RT penalty)",
      sum(!is.na(out$candidate_structure_error_rt))
    )
    drop_rt_cols <- intersect(c("rt_target", "type"), names(out))
    if (length(drop_rt_cols) > 0L) {
      out <- out |>
        tidytable::select(-tidyselect::any_of(x = drop_rt_cols))
    }
    return(out)
  }

  joined <- features_annotated_table |>
    tidytable::left_join(
      y = rt_table,
      by = "candidate_structure_inchikey_connectivity_layer"
    ) |>
    tidytable::mutate(
      candidate_structure_error_rt = .as_numeric_dedup(rt) -
        .as_numeric_dedup(rt_target)
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
  n_total <- nrow(features_annotated_table)
  if (n_total == 0L) {
    return(features_annotated_table)
  }

  required <- c("feature_id", "candidate_adduct")
  if (!all(required %in% names(features_annotated_table))) {
    return(features_annotated_table)
  }

  out <- features_annotated_table
  rm(features_annotated_table)
  if (!"candidate_adduct_match_mode" %in% names(out)) {
    data.table::set(
      out,
      j = "candidate_adduct_match_mode",
      value = rep.int(NA_character_, n_total)
    )
  }
  if (!"annotation_note" %in% names(out)) {
    data.table::set(
      out,
      j = "annotation_note",
      value = rep.int(NA_character_, n_total)
    )
  }

  # Parse adduct text once per unique adduct and assign compact integer state IDs
  cand_adducts <- out$candidate_adduct
  u_adducts_all <- unique(cand_adducts)
  adduct_values <- as.character(u_adducts_all[!is.na(u_adducts_all)])
  adduct_state_map <- build_adduct_state_key_map(adduct_values) |>
    tidytable::rename(
      candidate_adduct = adduct,
      adduct_state_key = state_key
    )
  u_state_str <- tidytable::coalesce(
    adduct_state_map$adduct_state_key[
      fastmatch::fmatch(adduct_values, adduct_state_map$candidate_adduct)
    ],
    adduct_values
  )
  u_states <- unique(u_state_str)
  state_id_by_u_add <- fastmatch::fmatch(u_state_str, u_states)

  is_ms1_row <- if ("candidate_library" %in% names(out)) {
    u_libs <- unique(out$candidate_library)
    u_libs_non_na <- as.character(u_libs[!is.na(u_libs)])
    ms1_libs <- u_libs_non_na[
      grepl("ms1", tolower(u_libs_non_na), fixed = TRUE)
    ]
    !is.na(fastmatch::fmatch(out$candidate_library, ms1_libs))
  } else {
    rep.int(FALSE, n_total)
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
    is_ms1_row & !is.na(cand_adducts) & nzchar(cand_adducts)
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
  # has a single primary strong adduct.
  u_fids <- unique(out$feature_id)
  f_int <- fastmatch::fmatch(out$feature_id, u_fids)
  m_add <- fastmatch::fmatch(cand_adducts, adduct_values)
  n_add_map <- max(1L, length(adduct_values))

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
      abs(.as_numeric_dedup(out$candidate_structure_error_mz[in_dup]))
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
  strong_state_id_col <- state_id_by_u_add[m_add[chosen_strong_idx]]
  strong_M_col <- compute_candidate_M(
    mz = out$mz[chosen_strong_idx],
    adduct_string = strong_adducts
  )

  is_spectral_row <- if ("candidate_library" %in% names(out)) {
    u_libs <- unique(out$candidate_library)
    u_libs_non_na <- as.character(u_libs[!is.na(u_libs)])
    spectral_libs <- u_libs_non_na[
      grepl("spectral", tolower(u_libs_non_na), fixed = TRUE)
    ]
    !is.na(fastmatch::fmatch(out$candidate_library, spectral_libs))
  } else {
    !is_ms1_row
  }

  m_feat <- fastmatch::fmatch(f_int, strong_f_int)
  has_strong <- !is.na(m_feat)
  cand_state_id <- state_id_by_u_add[m_add]
  strong_state_id <- strong_state_id_col[m_feat]

  exact_adduct_match <- has_strong &
    !is.na(cand_state_id) &
    (cand_state_id == strong_state_id)
  rm(cand_state_id, strong_state_id)

  idx_m <- which(
    !is_ms1_row &
      has_strong &
      !is.na(cand_adducts) &
      !exact_adduct_match
  )

  m_delta_rescued <- logical(n_total)
  if (length(idx_m) > 0L && "mz" %in% names(out)) {
    f_m <- f_int[idx_m]
    a_m <- m_add[idx_m]
    fa_m_key <- (f_m - 1L) * n_add_map + a_m
    u_fa_m_idx <- which(!duplicated(fa_m_key))
    fa_m_grp <- fastmatch::fmatch(fa_m_key, fa_m_key[u_fa_m_idx])

    u_idx_m <- idx_m[u_fa_m_idx]
    u_cand_M <- compute_candidate_M(
      mz = out$mz[u_idx_m],
      adduct_string = cand_adducts[u_idx_m]
    )
    u_strong_M <- strong_M_col[m_feat[u_idx_m]]
    mass_diff <- abs(u_cand_M - u_strong_M)
    ppm_window <- tolerance_ppm * 1e-6 * pmax(abs(u_cand_M), abs(u_strong_M))
    u_rescued <- !is.na(u_cand_M) &
      !is.na(u_strong_M) &
      (mass_diff <= ppm_window | mass_diff <= tolerance_dalton)
    u_rescued[is.na(u_rescued)] <- FALSE
    m_delta_rescued[idx_m] <- u_rescued[fa_m_grp]
    rm(
      f_m,
      a_m,
      fa_m_key,
      u_fa_m_idx,
      fa_m_grp,
      u_idx_m,
      u_cand_M,
      u_strong_M
    )
  }
  rm(f_int, m_add, m_feat, cand_adducts)

  keep_row <- is_ms1_row |
    !has_strong |
    is.na(out$candidate_adduct) |
    exact_adduct_match |
    m_delta_rescued

  if (all(is.na(out$candidate_adduct_match_mode))) {
    mm <- rep.int("incompatible", n_total)
    mm[!has_strong & !is_ms1_row] <- "unconstrained"
    mm[is_ms1_row] <- "ms1_reference"
    mm[exact_adduct_match] <- "exact_adduct"
    mm[m_delta_rescued] <- "m_delta_rescued"
    data.table::set(out, j = "candidate_adduct_match_mode", value = mm)
    rm(mm)
  } else {
    missing_mode_idx <- which(is.na(out$candidate_adduct_match_mode))
    if (length(missing_mode_idx) > 0L) {
      mm <- rep.int("incompatible", length(missing_mode_idx))
      mm[!has_strong[missing_mode_idx] & !is_ms1_row[missing_mode_idx]] <-
        "unconstrained"
      mm[is_ms1_row[missing_mode_idx]] <- "ms1_reference"
      mm[exact_adduct_match[missing_mode_idx]] <- "exact_adduct"
      mm[m_delta_rescued[missing_mode_idx]] <- "m_delta_rescued"
      out$candidate_adduct_match_mode[missing_mode_idx] <- mm
      rm(mm)
    }
  }
  rm(has_strong, exact_adduct_match)

  rescued_note_idx <- which(!is_ms1_row & m_delta_rescued)
  rm(m_delta_rescued)
  if (length(rescued_note_idx) > 0L) {
    cur_notes <- out$annotation_note[rescued_note_idx]
    na_notes <- is.na(cur_notes)
    if (any(na_notes)) {
      out$annotation_note[rescued_note_idx[na_notes]] <-
        "Adduct differs from strong MS1 state; retained by neutral-mass delta consistency"
    }
  }

  dropped_n <- n_total - sum(keep_row)
  dropped_spectral_n <- if (dropped_n > 0L) {
    sum(!keep_row & is_spectral_row)
  } else {
    0L
  }
  rm(is_ms1_row, is_spectral_row)

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

  out_filtered <- if (dropped_n == 0L) out else out[which(keep_row), ]
  rm(out, keep_row)
  attr(out_filtered, "adduct_semantics_audit") <- list(
    n_input = n_total,
    n_removed = dropped_n,
    n_removed_spectral = dropped_spectral_n,
    n_output = nrow(out_filtered)
  )
  out_filtered
}
