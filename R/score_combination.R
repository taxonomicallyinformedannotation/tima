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

  weight_bio_result |>
    tidytable::select(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      score_biological,
      candidate_score_pseudo_initial,
      score_weighted_bio
    )
}

#' @title Combine weighted scores from biological and chemical sources
#'
#' @description Combines biological and chemical scores computed separately
#'     into a single weighted score. Operates on narrow scoring tables to avoid
#'     cartesian explosions from joining wide candidate tables.
#'
#' @include weights_utils.R
#' @include validations_utils.R
#'
#' @param score_bio_table [data.frame] Narrow table with biological scores
#' @param score_chemo_table [data.frame] Narrow table with chemical scores
#' @param weight_biological [numeric] Weight for biological score (0-1)
#' @param weight_chemical [numeric] Weight for chemical score (0-1)
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
      # Missing spectral data (MS1-only hits) is handled gracefully via weight normalization:
      # When spectral weight is missing (NA), total_weight_available is reduced accordingly.
      score_final = compute_weighted_sum(
        score_biological,
        score_chemical,
        candidate_score_pseudo_initial,
        weights = c(weight_biological, weight_chemical, weight_spectral)
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

  # Get the candidate keys we need from combined_scores
  candidates_needed <- combined_scores |>
    tidytable::select(
      feature_id,
      candidate_structure_inchikey_connectivity_layer
    )

  # Semi-join against wide bio table to filter to only relevant rows
  # Keep ALL columns including original scores - ranking logic needs them
  bio_for_filtering <- wide_bio_table |>
    tidytable::semi_join(
      candidates_needed,
      by = c("feature_id", "candidate_structure_inchikey_connectivity_layer")
    )

  # Semi-join against wide chemo table - also keep all columns
  chemo_for_filtering <- wide_chemo_table |>
    tidytable::semi_join(
      candidates_needed,
      by = c("feature_id", "candidate_structure_inchikey_connectivity_layer")
    )

  # Start with bio table (has all metadata and scores)
  result <- bio_for_filtering

  # Add chemo-specific columns that aren't in bio
  # (score_weighted_chemo, score_chemical, etc)
  chemo_cols_to_add <- setdiff(
    names(chemo_for_filtering),
    names(bio_for_filtering)
  )

  if (length(chemo_cols_to_add) > 0L) {
    chemo_subset <- chemo_for_filtering |>
      tidytable::select(
        feature_id,
        candidate_structure_inchikey_connectivity_layer,
        tidyselect::all_of(chemo_cols_to_add)
      )

    result <- result |>
      tidytable::left_join(
        chemo_subset,
        by = c("feature_id", "candidate_structure_inchikey_connectivity_layer")
      )
  }

  # Add combined scores (score_final) as new column
  # But REPLACE score_weighted_chemo with score_final so ranking logic uses it
  combined_scores_only <- combined_scores |>
    tidytable::select(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      score_final
    )

  result <- result |>
    tidytable::left_join(
      combined_scores_only,
      by = c("feature_id", "candidate_structure_inchikey_connectivity_layer")
    ) |>
    tidytable::mutate(
      # Replace the old score_weighted_chemo with our new combined score
      # so that ranking logic uses the refactored combined scores
      score_weighted_chemo = score_final
    ) |>
    tidytable::select(-score_final)

  result
}
