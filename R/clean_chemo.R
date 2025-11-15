#' @title Clean chemo
#'
#' @description This function cleans chemically weighted annotation results by
#'     filtering to top candidates, optionally removing MS1-only annotations
#'     below score thresholds, and preparing final results for export. Can
#'     include compound names and apply high-confidence filters.
#'
#' @include filter_high_confidence_only.R
#' @include minimize_results.R
#' @include summarize_results.R
#'
#' @param annot_table_wei_chemo Data frame with chemically weighted annotations
#' @param components_table Data frame with molecular network components
#' @param features_table Data frame with feature metadata
#' @param structure_organism_pairs_table Data frame with structure-organism pairs
#' @param candidates_final Integer number of final candidates to keep per feature
#' @param best_percentile Numeric percentile threshold (0-1) for selecting top
#'     candidates within each feature (default: 0.9, keeps candidates with scores
#'     >= 90% of the maximum score for that feature). Used for both filtered and
#'     mini outputs to ensure consistent row counts.
#' @param minimal_ms1_bio Numeric minimal biological score for MS1 annotations (0-1)
#' @param minimal_ms1_chemo Numeric minimal chemical score for MS1 annotations (0-1)
#' @param minimal_ms1_condition Character condition: "OR" or "AND" for MS1 filtering
#' @param compounds_names Logical whether to include compound names (can be large)
#' @param high_confidence Logical whether to filter for high confidence only
#' @param remove_ties Logical whether to remove tied scores
#' @param summarize Logical whether to summarize to 1 row per feature
#'
#' @return Data frame with cleaned, filtered chemically weighted annotations
#'
#' @seealso weight_chemo
#'
#' @examples NULL
clean_chemo <- function(
  annot_table_wei_chemo = get(
    "annot_table_wei_chemo",
    envir = parent.frame()
  ),
  components_table = get("components_table", envir = parent.frame()),
  features_table = get("features_table", envir = parent.frame()),
  structure_organism_pairs_table = get(
    "structure_organism_pairs_table",
    envir = parent.frame()
  ),
  candidates_final = get("candidates_final", envir = parent.frame()),
  best_percentile = get("best_percentile", envir = parent.frame()),
  minimal_ms1_bio = get("minimal_ms1_bio", envir = parent.frame()),
  minimal_ms1_chemo = get("minimal_ms1_chemo", envir = parent.frame()),
  minimal_ms1_condition = get(
    "minimal_ms1_condition",
    envir = parent.frame()
  ),
  compounds_names = get("compounds_names", envir = parent.frame()),
  high_confidence = get("high_confidence", envir = parent.frame()),
  remove_ties = get("remove_ties", envir = parent.frame()),
  summarize = get("summarize", envir = parent.frame())
) {
  # ============================================================================
  # Input Validation
  # ============================================================================

  # Validate data frame
  if (!is.data.frame(annot_table_wei_chemo)) {
    stop("annot_table_wei_chemo must be a data frame")
  }

  # Early exit for empty input
  if (nrow(annot_table_wei_chemo) == 0L) {
    logger::log_warn("Empty annotation table provided")
    return(annot_table_wei_chemo)
  }

  # Validate numeric parameters (combined checks)
  if (!is.numeric(candidates_final) || candidates_final < 1) {
    stop("candidates_final must be a positive integer, got: ", candidates_final)
  }

  if (
    !is.numeric(best_percentile) || best_percentile < 0 || best_percentile > 1
  ) {
    stop("best_percentile must be between 0 and 1, got: ", best_percentile)
  }

  if (
    !is.numeric(minimal_ms1_bio) || minimal_ms1_bio < 0 || minimal_ms1_bio > 1
  ) {
    stop("minimal_ms1_bio must be between 0 and 1, got: ", minimal_ms1_bio)
  }

  if (
    !is.numeric(minimal_ms1_chemo) ||
      minimal_ms1_chemo < 0 ||
      minimal_ms1_chemo > 1
  ) {
    stop("minimal_ms1_chemo must be between 0 and 1, got: ", minimal_ms1_chemo)
  }

  # Validate condition
  if (!minimal_ms1_condition %in% c("OR", "AND")) {
    stop(
      "minimal_ms1_condition must be 'OR' or 'AND', got: ",
      minimal_ms1_condition
    )
  }

  # Validate logical parameters
  logical_params <- list(
    compounds_names = compounds_names,
    high_confidence = high_confidence,
    remove_ties = remove_ties,
    summarize = summarize
  )

  is_valid_logical <- sapply(logical_params, is.logical)
  if (!all(is_valid_logical)) {
    invalid_params <- names(logical_params)[!is_valid_logical]
    stop(
      "Parameter(s) must be logical (TRUE/FALSE): ",
      paste(invalid_params, collapse = ", ")
    )
  }

  # ============================================================================
  # Log Processing Parameters
  # ============================================================================

  logger::log_info("Cleaning chemically weighted annotations")
  logger::log_debug("Keeping top {candidates_final} candidates per feature")
  logger::log_debug(
    "Using best_percentile: {best_percentile} for consistent filtering"
  )
  logger::log_debug(
    "Options - High confidence: {high_confidence}, ",
    "Remove ties: {remove_ties}, Summarize: {summarize}"
  )
  logger::log_info(
    "Filtering top {candidates_final} candidates and keeping only MS1 ",
    "candidates with minimum {minimal_ms1_bio} biological score ",
    "{minimal_ms1_condition} {minimal_ms1_chemo} chemical score"
  )

  # ============================================================================
  # Filter MS1 Annotations Based on Scores
  # ============================================================================

  ## Those lines are to keep ms1 annotation
  ## Only if a good biological OR chemical consistency score is obtained
  if (minimal_ms1_condition == "OR") {
    df1 <- annot_table_wei_chemo |>
      tidytable::filter(
        (!is.na(candidate_score_similarity) |
          !is.na(candidate_score_sirius_csi)) |
          (score_biological >= minimal_ms1_bio |
            score_chemical >= minimal_ms1_chemo)
      )
  }
  if (minimal_ms1_condition == "AND") {
    df1 <- annot_table_wei_chemo |>
      tidytable::filter(
        (!is.na(candidate_score_similarity) |
          !is.na(candidate_score_sirius_csi)) |
          (score_biological >= minimal_ms1_bio &
            score_chemical >= minimal_ms1_chemo)
      )
  }

  df1 <- df1 |>
    tidytable::arrange(
      score_weighted_chemo |>
        tidytable::desc()
    ) |>
    tidytable::distinct(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    ) |>
    tidytable::mutate(
      rank_initial = tidytable::dense_rank(-candidate_score_pseudo_initial),
      rank_final = tidytable::dense_rank(-score_weighted_chemo),
      .by = c(feature_id)
    )

  results_mini <- df1 |>
    minimize_results(
      features_table = features_table,
      best_percentile = best_percentile
    )

  # Check if candidate columns exist before selecting them
  if (
    all(c("candidates_evaluated", "candidates_best") %in% names(results_mini))
  ) {
    results_candidates <- results_mini |>
      tidytable::distinct(feature_id, candidates_evaluated, candidates_best)
  } else {
    # Create empty results_candidates if columns don't exist
    results_candidates <- tidytable::tidytable(
      feature_id = character(0),
      candidates_evaluated = integer(0),
      candidates_best = integer(0)
    )
  }

  if (high_confidence) {
    df1 <- df1 |>
      filter_high_confidence_only()
  }

  if (compounds_names == FALSE) {
    df1 <- df1 |>
      tidytable::select(-candidate_structure_name)
  }

  # Apply same percentile filtering as minimize_results for consistency
  # This ensures filtered and mini outputs have the same number of rows
  # logger::log_trace(
  #  "Applying ",
  #  best_percentile,
  #  " percentile filter to match minimize_results"
  #)
  df1_filtered <- df1 |>
    tidytable::group_by(feature_id) |>
    tidytable::filter(
      score_weighted_chemo >= best_percentile * max(score_weighted_chemo, na.rm = TRUE)
    ) |>
    tidytable::ungroup() |>
    tidytable::filter(rank_final <= candidates_final)

  # logger::log_trace("Processing full results")
  results_full <- df1 |>
    summarize_results(
      features_table = features_table,
      components_table = components_table,
      structure_organism_pairs_table = structure_organism_pairs_table,
      annot_table_wei_chemo = annot_table_wei_chemo,
      remove_ties = remove_ties,
      summarize = summarize
    )
  # logger::log_trace("Processing filtered results")
  results_filtered <- df1_filtered |>
    summarize_results(
      features_table = features_table,
      components_table = components_table,
      structure_organism_pairs_table = structure_organism_pairs_table,
      annot_table_wei_chemo = annot_table_wei_chemo,
      remove_ties = remove_ties,
      summarize = summarize
    )
  if (
    results_candidates |>
      nrow() >
      0L
  ) {
    results_full <- results_full |>
      tidytable::left_join(results_candidates)
    results_filtered <- results_filtered |>
      tidytable::left_join(results_candidates)
  }
  rm(
    annot_table_wei_chemo,
    features_table,
    components_table,
    structure_organism_pairs_table
  )

  return(
    list(
      "full" = results_full,
      "filtered" = results_filtered,
      "mini" = results_mini
    )
  )
}
