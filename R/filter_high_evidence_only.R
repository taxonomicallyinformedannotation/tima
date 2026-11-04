#' @title Filter high evidence only
#' `r lifecycle::badge("experimental")`
#'
#' @description This function filters annotation results to retain only
#'     high-evidence candidates based on multiple scoring criteria
#'     (biological, initial, chemical) and retention time accuracy.
#'
#' @include validations_utils.R
#' @include logs_utils.R
#' @include validations_params.R
#'
#' @param df Data frame containing annotation results with score columns
#' @param score_bio_min Numeric minimum biological score threshold (default:
#'     0.8). Range: 0-1
#' @param score_ini_min Numeric minimum initial score threshold (default: 0.9).
#'     Range: 0-1
#' @param score_final_min Numeric minimum final (chemical) score threshold
#'     (default: 0.75). Range: 0-1
#' @param error_rt_max Numeric maximum retention time error in minutes (default:
#'     0.05). Must be > 0
#' @param confidence_sirius_min Numeric minimum SIRIUS confidence score
#'     threshold (optional).
#' Range: 0-1. If provided, candidates WITH a evidence score below this
#'     threshold
#'     are filtered out, but candidates with NA/missing scores are retained.
#' @param similarity_spectral_min Numeric minimum spectral similarity threshold
#'     (optional).
#' Range: 0-1. If provided, candidates WITH a similarity score below this
#'     threshold
#'     are filtered out, but candidates with NA/missing scores are retained.
#' @param matched_peaks_min Numeric minimum count of matched peaks threshold
#'     (optional).
#' Must be >= 0 (typically a small integer). If provided, candidates WITH a
#'     matched
#' peak count below this threshold are filtered out, but candidates with
#'     NA/missing
#'     peak counts are retained.
#' @param context Optional character string to tag logs with a stage label
#'     (e.g.,
#'     "mini+filtered" or "full"). Defaults to NULL (no tag).
#'
#' @return Data frame containing only high-evidence annotations that meet
#'     at least one score threshold and pass RT error filtering
#'
#' @details
#' Columns used when present:
#' - score_biological
#' - candidate_score_pseudo_initial
#' - score_weighted_chemo (contains combined final score)
#' - candidate_structure_error_rt (assumed in minutes; NA means unknown and is
#'     allowed)
#' - candidate_score_sirius_confidence (optional, NA values allowed)
#' - candidate_similarity (optional, NA values allowed, 0 always filtered)
#' - candidate_matched_peaks (optional, NA values allowed)
#'
#' Missing score columns are treated as absent for that criterion and do not
#' cause errors; at least one of the three primary scores must satisfy its
#' threshold for a row to be retained.
#'
#' **NA value handling:**
#' - RT error: NA values (unknown RT) are allowed to pass
#' - SIRIUS confidence: NA values (no SIRIUS annotation) are allowed to pass
#' - Spectral similarity: NA values (no spectral match) are allowed to pass, but
#'     0 is ALWAYS filtered
#' - Matched peaks: NA values (no peak matching data) are allowed to pass
#'
#' **SIRIUS and Spectral filtering (OR logic):**
#' When both `confidence_sirius_min` and `similarity_spectral_min` are set,
#' candidates pass if EITHER the SIRIUS confidence OR spectral similarity meets
#' its threshold. This allows candidates with good SIRIUS but poor spectral
#'     match
#' (or vice versa) to pass. Spectral similarity = 0 is always filtered out as
#'     invalid.
#'
#' This means thresholds only apply when the corresponding value is present.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Filter to high-evidence annotations only
#' high_evi <- filter_high_evidence_only(
#'   df = annotations,
#'   score_bio_min = 0.8,
#'   score_ini_min = 0.9,
#'   score_final_min = 0.75,
#'   error_rt_max = 0.05,
#'   context = "final_filtered"
#' )
#'
#' # With SIRIUS confidence threshold
#' high_evi <- filter_high_evidence_only(
#'   df = annotations,
#'   confidence_sirius_min = 0.8,
#'   similarity_spectral_min = 0.7
#' )
#' }
filter_high_evidence_only <- function(
  df,
  score_bio_min = DEFAULT_HE_SCORE_BIO_MIN,
  score_ini_min = DEFAULT_HE_SCORE_INITIAL_MIN,
  score_final_min = DEFAULT_HE_SCORE_FINAL_MIN,
  error_rt_max = DEFAULT_HE_MAX_RT_ERROR_MIN,
  confidence_sirius_min = DEFAULT_HE_SCORE_SIRIUS_MIN,
  similarity_spectral_min = DEFAULT_HE_SCORE_SPECTRAL_MIN,
  matched_peaks_min = DEFAULT_HE_SCORE_MIN_PEAKS,
  context = NULL
) {
  ctx <- log_operation(
    "filter_high_evidence",
    n_input = nrow(df),
    context = context %||% "default"
  )

  # Validation ----
  validate_dataframe(df, param_name = "df", allow_empty = TRUE)

  if (nrow(df) == 0L) {
    log_warn("Empty data frame provided to filter")
    return(df)
  }

  validate_numeric_range(
    score_bio_min,
    param_name = "score_bio_min",
    min_value = 0,
    max_value = 1
  )
  validate_numeric_range(
    score_ini_min,
    param_name = "score_ini_min",
    min_value = 0,
    max_value = 1
  )
  validate_numeric_range(
    score_final_min,
    param_name = "score_final_min",
    min_value = 0,
    max_value = 1
  )
  if (
    !is.numeric(error_rt_max) ||
      length(error_rt_max) != 1L ||
      is.na(error_rt_max) ||
      error_rt_max <= 0
  ) {
    cli::cli_abort(
      "RT error threshold must be positive (minutes)",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!is.null(confidence_sirius_min)) {
    validate_numeric_range(
      confidence_sirius_min,
      param_name = "confidence_sirius_min",
      min_value = 0,
      max_value = 1
    )
  }
  if (!is.null(similarity_spectral_min)) {
    validate_numeric_range(
      similarity_spectral_min,
      param_name = "similarity_spectral_min",
      min_value = 0,
      max_value = 1
    )
  }
  if (!is.null(matched_peaks_min)) {
    if (
      !is.numeric(matched_peaks_min) ||
        length(matched_peaks_min) != 1L ||
        is.na(matched_peaks_min) ||
        matched_peaks_min < 0
    ) {
      cli::cli_abort(
        "matched peaks threshold must be a non-negative number",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }
  }

  # Prepare safe columns ----
  # IMPORTANT: We do NOT use coalesce() to convert missing values to -Inf
  # because score=NA (MS1-only hits) is a legitimate value, not missing.
  # Instead, we create columns with actual values or NA, and handle NA in
  # filtering.
  # Explicitly convert to numeric to prevent character comparison issues.

  n_before <- nrow(df)
  df_src <- tidytable::as_tidytable(df)
  rm(df)

  # If a rank-1 row was promoted via cluster consensus, let it inherit the
  # anchor's evidence-bearing fields before any high-evidence filtering is
  # applied. This preserves the parent decision for the child row while still
  # letting the child keep its own non-evidence metadata.
  has_consensus_cols <- all(
    c(
      "cluster_consensus_promoted_from_anchor",
      "cluster_consensus_group_id",
      "cluster_consensus_anchor_feature_id",
      "rank_final"
    ) %in%
      names(df_src)
  )
  prom_idx <- integer(0L)
  if (has_consensus_cols) {
    grp_col <- df_src$cluster_consensus_group_id
    prom_col <- df_src$cluster_consensus_promoted_from_anchor
    rk_col <- df_src$rank_final
    prom_idx <- which(
      !is.na(grp_col) &
        !is.na(prom_col) &
        prom_col &
        !is.na(rk_col) &
        rk_col == 1L
    )
    if (length(prom_idx) > 0L) {
      inherit_cols <- intersect(
        c(
          "score_biological",
          "candidate_score_pseudo_initial",
          "score_weighted_chemo",
          "candidate_structure_error_rt",
          "candidate_score_sirius_confidence",
          "candidate_similarity",
          "candidate_score_similarity",
          "candidate_score_similarity_forward",
          "candidate_score_similarity_reverse",
          "candidate_count_similarity_peaks_matched",
          "candidate_adduct_origin",
          "candidate_annotation_level",
          "candidate_evidence_tier",
          "adduct_support",
          "annotation_note"
        ),
        names(df_src)
      )
      anchor_idx <- which(
        !is.na(grp_col) &
          !is.na(rk_col) &
          rk_col == 1L &
          df_src$feature_id == df_src$cluster_consensus_anchor_feature_id
      )
      if (length(inherit_cols) > 0L && length(anchor_idx) > 0L) {
        anchor_u_idx <- anchor_idx[!duplicated(grp_col[anchor_idx])]
        m_anc <- fastmatch::fmatch(grp_col[prom_idx], grp_col[anchor_u_idx])
        has_anc <- which(!is.na(m_anc))
        if (length(has_anc) > 0L) {
          df_src <- data.table::copy(df_src)
          p_sub <- prom_idx[has_anc]
          a_sub <- anchor_u_idx[m_anc[has_anc]]
          for (col in inherit_cols) {
            cur_v <- df_src[[col]]
            anc_v <- cur_v[a_sub]
            use_anc <- !is.na(anc_v)
            if (any(use_anc)) {
              cur_v[p_sub[use_anc]] <- anc_v[use_anc]
              data.table::set(df_src, j = col, value = cur_v)
            }
          }
        }
      }
    }
  }

  .num_col <- function(col_name) {
    if (!col_name %in% names(df_src)) {
      return(NULL)
    }
    x <- df_src[[col_name]]
    if (is.numeric(x)) x else .as_numeric_dedup(x)
  }

  # Core filtering ----
  # At least one of the three score thresholds must be satisfied
  score_bio <- .num_col("score_biological")
  score_ini <- .num_col("candidate_score_pseudo_initial")
  score_fin <- .num_col("score_weighted_chemo")

  keep <- logical(n_before)
  if (!is.null(score_bio)) {
    keep <- keep | (!is.na(score_bio) & score_bio >= score_bio_min)
  }
  if (!is.null(score_ini)) {
    keep <- keep |
      (!is.na(score_ini) & score_ini > 0 & score_ini >= score_ini_min)
  }
  if (!is.null(score_fin)) {
    keep <- keep | (!is.na(score_fin) & score_fin >= score_final_min)
  }

  # RT error filter (minutes). Allow NA (unknown) values
  rt_err <- .num_col("candidate_structure_error_rt")
  if (!is.null(rt_err)) {
    keep <- keep & (is.na(rt_err) | abs(rt_err) <= error_rt_max)
  }

  # SIRIUS confidence and spectral similarity filtering
  # ALWAYS filter out spectral similarity = 0 (invalid MS2 data) if column exists
  csim <- .num_col("candidate_similarity")
  if (!is.null(csim)) {
    keep <- keep & (is.na(csim) | csim > 0)
  }

  # Apply threshold filters with OR logic if set
  # NA values are allowed (no SIRIUS/spectral data available)
  has_sirius <- !is.null(confidence_sirius_min) &&
    "candidate_score_sirius_confidence" %in% names(df_src)
  has_spectral <- !is.null(similarity_spectral_min) && !is.null(score_ini)

  if (has_sirius && has_spectral) {
    sir_v <- .num_col("candidate_score_sirius_confidence")
    fwd_v <- .num_col("candidate_score_similarity_forward")
    rev_v <- .num_col("candidate_score_similarity_reverse")
    pass_or <- (!is.na(sir_v) & sir_v >= confidence_sirius_min) |
      (!is.na(score_ini) & score_ini >= similarity_spectral_min) |
      (is.na(sir_v) & is.na(score_ini))
    if (!is.null(fwd_v)) {
      pass_or <- pass_or |
        (!is.na(fwd_v) & fwd_v >= 2 * similarity_spectral_min)
    }
    if (!is.null(rev_v)) {
      pass_or <- pass_or |
        (!is.na(rev_v) & rev_v >= 2 * similarity_spectral_min)
    }
    keep <- keep & pass_or
  } else if (has_sirius) {
    sir_v <- .num_col("candidate_score_sirius_confidence")
    keep <- keep & (is.na(sir_v) | sir_v >= confidence_sirius_min)
  } else if (has_spectral) {
    keep <- keep & (is.na(score_ini) | score_ini >= similarity_spectral_min)
  }

  # Matched peaks filtering
  pks_v <- .num_col("candidate_count_similarity_peaks_matched")
  if (!is.null(pks_v)) {
    if (!is.null(matched_peaks_min)) {
      keep <- keep & (is.na(pks_v) | (pks_v > 0 & pks_v >= matched_peaks_min))
    } else {
      keep <- keep & (is.na(pks_v) | pks_v > 0)
    }
  }

  keep_idx <- which(keep)

  # Enforce parent-following semantics for cluster-consensus-promoted children.
  if (length(prom_idx) > 0L) {
    grp_col <- df_src$cluster_consensus_group_id
    rk_col <- df_src$rank_final
    anchor_all_idx <- which(
      !is.na(grp_col) &
        !is.na(rk_col) &
        rk_col == 1L &
        df_src$feature_id == df_src$cluster_consensus_anchor_feature_id
    )
    passing_groups <- if (length(anchor_all_idx) > 0L) {
      unique(grp_col[anchor_all_idx[keep[anchor_all_idx]]])
    } else {
      character(0L)
    }
    prom_pass <- !is.na(fastmatch::fmatch(grp_col[prom_idx], passing_groups))
    keep_child_row_ids <- prom_idx[prom_pass]
    drop_child_row_ids <- prom_idx[!prom_pass]

    if (length(drop_child_row_ids) > 0L) {
      keep_idx <- keep_idx[is.na(fastmatch::fmatch(
        keep_idx,
        drop_child_row_ids
      ))]
    }
    if (length(keep_child_row_ids) > 0L) {
      add_back_ids <- keep_child_row_ids[is.na(fastmatch::fmatch(
        keep_child_row_ids,
        keep_idx
      ))]
      if (length(add_back_ids) > 0L) {
        keep_idx <- c(keep_idx, add_back_ids)
      }
    }
  }

  df_filtered <- df_src[keep_idx, ]
  rm(df_src)

  n_after <- nrow(df_filtered)
  n_removed <- n_before - n_after
  percent_removed <- round(100 * n_removed / n_before, 1)

  # Build a tag for log lines if context is provided
  tag <- if (!is.null(context) && nzchar(context)) {
    paste0("[", context, "] ")
  } else {
    ""
  }

  log_info(
    "%s Removed %d low-evidence candidates (%s%% of %d total)",
    tag,
    n_removed,
    percent_removed,
    n_before
  )
  log_info(
    "%s %d high-evidence candidates remaining (%s%%)",
    tag,
    n_after,
    round(100 * n_after / n_before, 1)
  )

  log_complete(ctx, n_filtered = n_after, n_removed = n_removed)

  df_filtered
}

# Backwards-compatible alias used by existing callers/tests.
filter_high_evidence_only <- filter_high_evidence_only
