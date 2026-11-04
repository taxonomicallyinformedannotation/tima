.has_unique_feature_ik <- function(df) {
  if (isTRUE(attr(df, ".unique_feature_ik"))) {
    return(TRUE)
  }
  n <- nrow(df)
  if (n <= 1L) {
    return(TRUE)
  }
  f <- df$feature_id
  k <- df$candidate_structure_inchikey_connectivity_layer
  if (anyNA(f) || anyNA(k)) {
    return(FALSE)
  }
  u_f <- unique(f)
  u_k <- unique(k)
  f_id <- fastmatch::fmatch(f, u_f)
  k_id <- fastmatch::fmatch(k, u_k)
  n_k <- length(u_k)
  if (as.double(length(u_f)) * n_k < .Machine$integer.max) {
    anyDuplicated((f_id - 1L) * n_k + k_id) == 0L
  } else {
    pk <- as.double(f_id - 1L) * n_k + k_id
    all(fastmatch::fmatch(pk, pk) == seq_along(pk))
  }
}

#' @title Extract narrow biological score table
#'
#' @description Extracts only the narrow scoring columns from the full
#'     weight_bio output, avoiding wide table materialization.
#'
#' @include weight_bio.R
#' @include validations_utils.R
#' @include weights_utils.R
#' @include logs_utils.R
#'
#' @param weight_bio_result [data.frame] Output from weight_bio()
#'
#' @return Data frame with columns:
#'     - feature_id
#'     - candidate_structure_inchikey_connectivity_layer
#'     - score_biological
#'     - candidate_score_pseudo_initial
#'     - score_weighted_bio
#'
#' @keywords internal
extract_bio_scores <- function(weight_bio_result) {
  validate_dataframe(weight_bio_result, param_name = "weight_bio_result")

  if (nrow(weight_bio_result) == 0L) {
    return(
      tidytable::tidytable(
        feature_id = character(),
        candidate_structure_inchikey_connectivity_layer = character(),
        score_biological = numeric(),
        candidate_score_pseudo_initial = numeric(),
        score_weighted_bio = numeric()
      )
    )
  }

  need_cols <- c(
    "feature_id",
    "candidate_structure_inchikey_connectivity_layer",
    "score_biological",
    "candidate_score_pseudo_initial",
    "score_weighted_bio"
  )
  out <- if (all(need_cols %in% names(weight_bio_result))) {
    tidytable::as_tidytable(as.list(weight_bio_result)[need_cols])
  } else {
    weight_bio_result |>
      tidytable::select(
        feature_id,
        candidate_structure_inchikey_connectivity_layer,
        score_biological,
        candidate_score_pseudo_initial,
        score_weighted_bio
      )
  }
  if (isTRUE(attr(weight_bio_result, ".unique_feature_ik"))) {
    attr(out, ".unique_feature_ik") <- TRUE
  }
  out
}

#' @title Combine weighted scores from biological and chemical sources
#'
#' @description Combines biological and chemical scores computed separately
#'     into a single weighted score. Operates on narrow scoring tables to avoid
#'     cartesian explosions from joining wide candidate tables.
#'
#' @include weights_utils.R
#' @include validations_utils.R
#' @include validations_params.R
#'
#' @param score_bio_table [data.frame] Narrow table with biological scores
#' @param score_chemo_table [data.frame] Narrow table with chemical scores
#' @param weight_biological [numeric] Weight for biological score (0-1)
#' @param weight_chemical [numeric] Weight for chemical score (0-1)
#' @param weight_spectral [numeric] Weight for spectral score (0-1)
#'
#' @return Data frame with narrow scoring columns only
#'
#' @keywords internal
combine_weighted_scores <- function(
  score_bio_table,
  score_chemo_table,
  weight_biological,
  weight_chemical,
  weight_spectral = 0.3
) {
  # Input Validation
  validate_dataframe(score_bio_table, param_name = "score_bio_table")
  validate_dataframe(score_chemo_table, param_name = "score_chemo_table")

  ctx <- log_operation(
    "combine_weighted_scores",
    n_bio = nrow(score_bio_table),
    n_chemo = nrow(score_chemo_table)
  )

  if (nrow(score_bio_table) == 0L || nrow(score_chemo_table) == 0L) {
    log_warn("Empty scoring tables provided")
    return(
      tidytable::tidytable(
        feature_id = character(),
        candidate_structure_inchikey_connectivity_layer = character(),
        score_initial = numeric(),
        score_biological = numeric(),
        score_chemical = numeric(),
        score_final = numeric()
      )
    )
  }

  # Fast path when both narrow tables are already row-aligned with unique keys
  aligned <- nrow(score_bio_table) == nrow(score_chemo_table) &&
    identical(score_bio_table$feature_id, score_chemo_table$feature_id) &&
    identical(
      score_bio_table$candidate_structure_inchikey_connectivity_layer,
      score_chemo_table$candidate_structure_inchikey_connectivity_layer
    ) &&
    .has_unique_feature_ik(score_bio_table)

  if (aligned) {
    score_final <- compute_weighted_sum(
      score_chemo_table$score_chemical,
      score_bio_table$score_biological,
      score_bio_table$candidate_score_pseudo_initial,
      weights = c(weight_chemical, weight_biological, weight_spectral)
    )
    result <- tidytable::tidytable(
      feature_id = score_bio_table$feature_id,
      candidate_structure_inchikey_connectivity_layer = score_bio_table$candidate_structure_inchikey_connectivity_layer,
      score_initial = score_bio_table$candidate_score_pseudo_initial,
      score_biological = score_bio_table$score_biological,
      score_chemical = score_chemo_table$score_chemical,
      score_final = score_final
    )
    attr(result, ".aligned_unique_feature_ik") <- TRUE
    log_complete(ctx, n_combined = nrow(result))
    return(result)
  }

  # Key join: ONLY on feature_id + inchikey, nothing else
  # This prevents cartesian products
  result <- score_bio_table |>
    tidytable::inner_join(
      y = score_chemo_table,
      by = c(
        "feature_id",
        "candidate_structure_inchikey_connectivity_layer"
      )
    ) |>
    tidytable::mutate(
      # Combine ALL THREE evidence sources:
      # - score_biological: organism/taxonomy evidence (from bio)
      # - score_chemical: structure/classifier evidence (from chemo)
      # - candidate_score_pseudo_initial: spectral evidence (MS2 match, or NA for MS1-only)
      #
      # All three are weighted according to user preference.
      # Missing values (e.g. no spectral score for MS1-only hits) count as 0
      # and the sum is divided by the total weight (see
      # compute_weighted_components()). Same order as in weight_chemo().
      score_final = compute_weighted_sum(
        score_chemical,
        score_biological,
        candidate_score_pseudo_initial,
        weights = c(weight_chemical, weight_biological, weight_spectral)
      )
    ) |>
    tidytable::select(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      score_initial = candidate_score_pseudo_initial,
      score_biological,
      score_chemical,
      score_final
    )

  log_complete(ctx, n_combined = nrow(result))

  result
}

#' @title Expand narrow combined scores with minimal metadata for filtering
#'
#' @description Takes narrow combined score table and expands it with necessary
#'     columns from original wide tables for clean_chemo's filtering/ranking logic.
#'     Uses semi-join strategy to avoid cartesian products. Keeps original score
#'     columns AND adds combined scores as new columns.
#'
#' @param combined_scores Narrow combined score table from combine_weighted_scores()
#' @param wide_bio_table Full output from weight_bio() (wide metadata)
#' @param wide_chemo_table Full output from weight_chemo() (wide metadata)
#'
#' @return Expanded table suitable for clean_chemo filtering/ranking with all
#'     original score columns + combined scores
#' @keywords internal
expand_combined_scores_for_filtering <- function(
  combined_scores,
  wide_bio_table,
  wide_chemo_table
) {
  validate_dataframe(combined_scores, param_name = "combined_scores")
  validate_dataframe(wide_bio_table, param_name = "wide_bio_table")
  validate_dataframe(wide_chemo_table, param_name = "wide_chemo_table")

  chemo_cols_to_add <- setdiff(
    names(wide_chemo_table),
    names(wide_bio_table)
  )

  # Fast path when all three tables are row-aligned with unique keys
  aligned <- nrow(wide_bio_table) == nrow(wide_chemo_table) &&
    nrow(wide_bio_table) == nrow(combined_scores) &&
    identical(wide_bio_table$feature_id, wide_chemo_table$feature_id) &&
    identical(wide_bio_table$feature_id, combined_scores$feature_id) &&
    identical(
      wide_bio_table$candidate_structure_inchikey_connectivity_layer,
      wide_chemo_table$candidate_structure_inchikey_connectivity_layer
    ) &&
    identical(
      wide_bio_table$candidate_structure_inchikey_connectivity_layer,
      combined_scores$candidate_structure_inchikey_connectivity_layer
    ) &&
    (isTRUE(attr(combined_scores, ".aligned_unique_feature_ik")) ||
      .has_unique_feature_ik(wide_bio_table))

  if (aligned) {
    result <- tidytable::as_tidytable(as.list(
      select_clean_chemo_working_columns(wide_bio_table)
    ))
    for (col in chemo_cols_to_add) {
      data.table::set(result, j = col, value = wide_chemo_table[[col]])
    }
    if ("score_final" %in% names(combined_scores)) {
      data.table::set(
        result,
        j = "score_weighted_chemo",
        value = combined_scores$score_final
      )
    }
    return(result)
  }

  # Get the candidate keys we need from combined_scores
  candidates_needed <- combined_scores |>
    tidytable::select(
      feature_id,
      candidate_structure_inchikey_connectivity_layer
    )

  # OPTIMIZATION: Combine chemo column addition and score_final into a
  # single left_join instead of two separate joins. Each left_join on a
  # millions-row table creates a full copy, so merging the join inputs
  # into one reduces intermediate memory by ~50% for this step.
  # score_final comes from combined_scores (not the wide tables), so we
  # extract it here and merge it into the chemo lookup table.
  chemo_lookup <- tidytable::select(
    wide_chemo_table,
    feature_id,
    candidate_structure_inchikey_connectivity_layer,
    tidyselect::all_of(chemo_cols_to_add)
  )
  # Merge score_final from combined_scores into the single lookup table
  score_final_col <- if ("score_final" %in% names(combined_scores)) {
    tidytable::select(
      combined_scores,
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      score_final
    )
  } else {
    NULL
  }
  if (!is.null(score_final_col)) {
    chemo_lookup <- chemo_lookup |>
      tidytable::left_join(
        y = score_final_col,
        by = c("feature_id", "candidate_structure_inchikey_connectivity_layer")
      )
    rm(score_final_col)
  }

  # Semi-join to filter to only relevant rows, then single left_join
  result <- wide_bio_table |>
    tidytable::semi_join(
      candidates_needed,
      by = c("feature_id", "candidate_structure_inchikey_connectivity_layer")
    ) |>
    select_clean_chemo_working_columns() |>
    tidytable::left_join(
      y = chemo_lookup |>
        tidytable::semi_join(
          candidates_needed,
          by = c(
            "feature_id",
            "candidate_structure_inchikey_connectivity_layer"
          )
        ),
      by = c("feature_id", "candidate_structure_inchikey_connectivity_layer")
    )

  # Replace score_weighted_chemo with score_final for ranking
  if ("score_final" %in% names(result)) {
    result$score_weighted_chemo <- result$score_final
    result$score_final <- NULL
  }

  rm(
    chemo_lookup,
    candidates_needed,
    wide_bio_table,
    wide_chemo_table,
    combined_scores
  )

  result
}
