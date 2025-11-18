#' Validate Inputs for clean_chemo
#'
#' @description Internal helper to validate all input parameters for clean_chemo.
#'     Checks data types, ranges, and logical consistency.
#'
#' @param annot_table_wei_chemo Data frame with annotations
#' @param candidates_final Integer >= 1
#' @param best_percentile Numeric (0-1)
#' @param minimal_ms1_bio Numeric (0-1)
#' @param minimal_ms1_chemo Numeric (0-1)
#' @param minimal_ms1_condition Character: "OR" or "AND"
#' @param compounds_names Logical
#' @param high_confidence Logical
#' @param remove_ties Logical
#' @param summarize Logical
#'
#' @return NULL (stops execution if validation fails)
#' @keywords internal
validate_clean_chemo_inputs <- function(
  annot_table_wei_chemo,
  candidates_final,
  best_percentile,
  minimal_ms1_bio,
  minimal_ms1_chemo,
  minimal_ms1_condition,
  compounds_names,
  high_confidence,
  remove_ties,
  summarize
) {
  # Validate data frame
  if (!is.data.frame(annot_table_wei_chemo)) {
    stop("annot_table_wei_chemo must be a data frame", call. = FALSE)
  }

  # Validate numeric parameters (combined checks)
  if (!is.numeric(candidates_final) || candidates_final < 1) {
    stop(
      "candidates_final must be a positive integer, got: ",
      candidates_final,
      call. = FALSE
    )
  }

  if (!is.numeric(best_percentile) || best_percentile < 0 || best_percentile > 1) {
    stop(
      "best_percentile must be between 0 and 1, got: ",
      best_percentile,
      call. = FALSE
    )
  }

  if (!is.numeric(minimal_ms1_bio) || minimal_ms1_bio < 0 || minimal_ms1_bio > 1) {
    stop(
      "minimal_ms1_bio must be between 0 and 1, got: ",
      minimal_ms1_bio,
      call. = FALSE
    )
  }

  if (
    !is.numeric(minimal_ms1_chemo) ||
      minimal_ms1_chemo < 0 ||
      minimal_ms1_chemo > 1
  ) {
    stop(
      "minimal_ms1_chemo must be between 0 and 1, got: ",
      minimal_ms1_chemo,
      call. = FALSE
    )
  }

  # Validate condition
  if (!minimal_ms1_condition %in% c("OR", "AND")) {
    stop(
      "minimal_ms1_condition must be 'OR' or 'AND', got: ",
      minimal_ms1_condition,
      call. = FALSE
    )
  }

  # Validate logical parameters
  logical_params <- list(
    compounds_names = compounds_names,
    high_confidence = high_confidence,
    remove_ties = remove_ties,
    summarize = summarize
  )

  is_valid_logical <- vapply(logical_params, is.logical, logical(1))
  if (!all(is_valid_logical)) {
    invalid_params <- names(logical_params)[!is_valid_logical]
    stop(
      "Parameter(s) must be logical (TRUE/FALSE): ",
      paste(invalid_params, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Filter MS1 Annotations by Score Thresholds
#'
#' @description Internal helper to filter MS1-only annotations based on
#'     biological and chemical score thresholds with OR/AND logic.
#'
#' @param annot_table_wei_chemo Data frame with annotations
#' @param minimal_ms1_bio Numeric minimum biological score
#' @param minimal_ms1_chemo Numeric minimum chemical score
#' @param minimal_ms1_condition Character "OR" or "AND"
#'
#' @return Filtered data frame
#' @keywords internal
filter_ms1_annotations <- function(
  annot_table_wei_chemo,
  minimal_ms1_bio,
  minimal_ms1_chemo,
  minimal_ms1_condition
) {
  # Keep annotations with MS2 data OR MS1 meeting score thresholds
  has_ms2 <- quote(
    !is.na(candidate_score_similarity) | !is.na(candidate_score_sirius_csi)
  )

  if (minimal_ms1_condition == "OR") {
    ms1_condition <- quote(
      score_biological >= minimal_ms1_bio | score_chemical >= minimal_ms1_chemo
    )
  } else {
    # "AND"
    ms1_condition <- quote(
      score_biological >= minimal_ms1_bio & score_chemical >= minimal_ms1_chemo
    )
  }

  annot_table_wei_chemo |>
    tidytable::filter(!!has_ms2 | !!ms1_condition)
}

#' Rank and Deduplicate Annotations
#'
#' @description Internal helper to rank candidates and keep the best
#'     structure per feature.
#'
#' @param df Data frame with filtered annotations
#'
#' @return Data frame with ranking columns added
#' @keywords internal
rank_and_deduplicate <- function(df) {
  df |>
    tidytable::arrange(tidytable::desc(score_weighted_chemo)) |>
    tidytable::distinct(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    ) |>
    tidytable::mutate(
      rank_initial = tidytable::dense_rank(-candidate_score_pseudo_initial),
      rank_final = tidytable::dense_rank(-score_weighted_chemo),
      .by = feature_id
    )
}

#' Apply Percentile Filter
#'
#' @description Internal helper to filter candidates by percentile threshold
#'     within each feature.
#'
#' @param df Data frame with ranked annotations
#' @param best_percentile Numeric percentile threshold (0-1)
#'
#' @return Filtered data frame
#' @keywords internal
apply_percentile_filter <- function(df, best_percentile) {
  df |>
    tidytable::group_by(feature_id) |>
    tidytable::filter(
      score_weighted_chemo >=
        best_percentile * max(score_weighted_chemo, na.rm = TRUE)
    ) |>
    tidytable::ungroup()
}

#' Count Evaluated and Best Candidates
#'
#' @description Internal helper to count candidates before and after
#'     percentile filtering.
#'
#' @param df_ranked Data frame with all ranked candidates
#' @param df_percentile Data frame with percentile-filtered candidates
#'
#' @return Data frame with feature_id, candidates_evaluated, candidates_best
#' @keywords internal
count_candidates <- function(df_ranked, df_percentile) {
  candidates_evaluated <- df_ranked |>
    tidytable::group_by(feature_id) |>
    tidytable::add_count(name = "candidates_evaluated") |>
    tidytable::ungroup() |>
    tidytable::distinct(feature_id, candidates_evaluated)

  candidates_best <- df_percentile |>
    tidytable::group_by(feature_id) |>
    tidytable::add_count(name = "candidates_best") |>
    tidytable::ungroup() |>
    tidytable::distinct(feature_id, candidates_best)

  tidytable::left_join(
    candidates_evaluated,
    candidates_best,
    by = "feature_id"
  )
}

#' Extract Weighted Taxonomy Scores
#'
#' @description Internal helper to extract level weights from parent environment
#'     (matching weight_chemo parameters).
#'
#' @return Named list of weights
#' @keywords internal
get_taxonomy_weights <- function() {
  list(
    w_cla_kin = get("score_chemical_cla_kingdom", envir = parent.frame(n = 2)),
    w_cla_sup = get("score_chemical_cla_superclass", envir = parent.frame(n = 2)),
    w_cla_cla = get("score_chemical_cla_class", envir = parent.frame(n = 2)),
    w_cla_par = get("score_chemical_cla_parent", envir = parent.frame(n = 2)),
    w_npc_pat = get("score_chemical_npc_pathway", envir = parent.frame(n = 2)),
    w_npc_sup = get("score_chemical_npc_superclass", envir = parent.frame(n = 2)),
    w_npc_cla = get("score_chemical_npc_class", envir = parent.frame(n = 2))
  )
}

#' Compute Weighted ClassyFire Taxonomy
#'
#' @description Internal helper to compute weighted scores for all ClassyFire
#'     levels and select the best one.
#'
#' @param df_pred_tax Data frame with predicted taxonomy scores
#' @param weights List of taxonomy weights
#'
#' @return Data frame with selected ClassyFire label and score
#' @keywords internal
compute_classyfire_taxonomy <- function(df_pred_tax, weights) {
  df_pred_tax |>
    tidytable::mutate(
      # Compute weighted score for each level
      ws_kin = as.numeric(feature_pred_tax_cla_01kin_score) * weights$w_cla_kin,
      ws_sup = as.numeric(feature_pred_tax_cla_02sup_score) * weights$w_cla_sup,
      ws_cla = as.numeric(feature_pred_tax_cla_03cla_score) * weights$w_cla_cla,
      ws_par = as.numeric(feature_pred_tax_cla_04dirpar_score) * weights$w_cla_par,
      # Find which level has max weighted score
      max_ws = pmax(ws_kin, ws_sup, ws_cla, ws_par, na.rm = TRUE),
      # Pick label and score from that level
      label_classyfire_predicted = tidytable::case_when(
        !is.na(ws_par) & ws_par == max_ws ~ feature_pred_tax_cla_04dirpar_val,
        !is.na(ws_cla) & ws_cla == max_ws ~ feature_pred_tax_cla_03cla_val,
        !is.na(ws_sup) & ws_sup == max_ws ~ feature_pred_tax_cla_02sup_val,
        !is.na(ws_kin) & ws_kin == max_ws ~ feature_pred_tax_cla_01kin_val,
        TRUE ~ NA_character_
      ),
      score_classyfire = tidytable::case_when(
        !is.na(ws_par) & ws_par == max_ws ~
          as.numeric(feature_pred_tax_cla_04dirpar_score),
        !is.na(ws_cla) & ws_cla == max_ws ~
          as.numeric(feature_pred_tax_cla_03cla_score),
        !is.na(ws_sup) & ws_sup == max_ws ~
          as.numeric(feature_pred_tax_cla_02sup_score),
        !is.na(ws_kin) & ws_kin == max_ws ~
          as.numeric(feature_pred_tax_cla_01kin_score),
        TRUE ~ NA_real_
      )
    ) |>
    tidytable::filter(label_classyfire_predicted != "empty") |>
    tidytable::select(
      feature_id,
      label_classyfire_predicted,
      score_classyfire
    ) |>
    tidytable::distinct()
}

#' Compute Weighted NPClassifier Taxonomy
#'
#' @description Internal helper to compute weighted scores for all NPClassifier
#'     levels and select the best one.
#'
#' @param df_pred_tax Data frame with predicted taxonomy scores
#' @param weights List of taxonomy weights
#'
#' @return Data frame with selected NPClassifier label and score
#' @keywords internal
compute_npclassifier_taxonomy <- function(df_pred_tax, weights) {
  df_pred_tax |>
    tidytable::mutate(
      # Compute weighted score for each level
      ws_pat = as.numeric(feature_pred_tax_npc_01pat_score) * weights$w_npc_pat,
      ws_sup = as.numeric(feature_pred_tax_npc_02sup_score) * weights$w_npc_sup,
      ws_cla = as.numeric(feature_pred_tax_npc_03cla_score) * weights$w_npc_cla,
      # Find which level has max weighted score
      max_ws = pmax(ws_pat, ws_sup, ws_cla, na.rm = TRUE),
      # Pick label and score from that level
      label_npclassifier_predicted = tidytable::case_when(
        !is.na(ws_cla) & ws_cla == max_ws ~ feature_pred_tax_npc_03cla_val,
        !is.na(ws_sup) & ws_sup == max_ws ~ feature_pred_tax_npc_02sup_val,
        !is.na(ws_pat) & ws_pat == max_ws ~ feature_pred_tax_npc_01pat_val,
        TRUE ~ NA_character_
      ),
      score_npclassifier = tidytable::case_when(
        !is.na(ws_cla) & ws_cla == max_ws ~
          as.numeric(feature_pred_tax_npc_03cla_score),
        !is.na(ws_sup) & ws_sup == max_ws ~
          as.numeric(feature_pred_tax_npc_02sup_score),
        !is.na(ws_pat) & ws_pat == max_ws ~
          as.numeric(feature_pred_tax_npc_01pat_score),
        TRUE ~ NA_real_
      )
    ) |>
    tidytable::filter(label_npclassifier_predicted != "empty") |>
    tidytable::select(
      feature_id,
      label_npclassifier_predicted,
      score_npclassifier
    ) |>
    tidytable::distinct()
}

#' Remove Compound Names from Results
#'
#' @description Internal helper to optionally remove compound names from
#'     all result tiers.
#'
#' @param results_list Named list with full, filtered, mini data frames
#' @param compounds_names Logical, if FALSE remove names
#'
#' @return Modified results list
#' @keywords internal
remove_compound_names <- function(results_list, compounds_names) {
  if (!compounds_names) {
    results_list$mini <- results_list$mini |>
      tidytable::select(-tidyselect::any_of("candidate_structure_name"))
    results_list$filtered <- results_list$filtered |>
      tidytable::select(-tidyselect::any_of("candidate_structure_name"))
    results_list$full <- results_list$full |>
      tidytable::select(-tidyselect::any_of("candidate_structure_name"))
  }
  results_list
}

#' @title Clean Chemical Annotations
#'
#' @description Cleans and filters chemically weighted annotation results through
#'     a multi-tier pipeline. Applies MS1 score thresholds, percentile filtering,
#'     ranking, and optional high-confidence filtering. Returns three-tier output:
#'     full (comprehensive), filtered (top candidates), and mini (one row per feature).
#'
#' @include filter_high_confidence_only.R
#' @include summarize_results.R
#'
#' @param annot_table_wei_chemo Data frame with chemically weighted annotations.
#'     Required columns: feature_id, candidate_structure_inchikey_connectivity_layer,
#'     score_weighted_chemo, score_biological, score_chemical, candidate_score_pseudo_initial
#' @param components_table Data frame with molecular network component assignments.
#'     Required columns: feature_id, component_id
#' @param features_table Data frame with feature metadata (RT, m/z, etc.).
#'     Required columns: feature_id
#' @param structure_organism_pairs_table Data frame linking structures to organisms.
#'     Required columns: structure_inchikey_connectivity_layer
#' @param candidates_final Integer, number of top candidates to retain per feature (>= 1)
#' @param best_percentile Numeric (0-1), percentile threshold for score filtering.
#'     Candidates with scores >= percentile * max_score are kept. Default: 0.9 (90th percentile)
#' @param minimal_ms1_bio Numeric (0-1), minimum biological score for MS1-only annotations
#' @param minimal_ms1_chemo Numeric (0-1), minimum chemical score for MS1-only annotations
#' @param minimal_ms1_condition Character, logical operator for MS1 filtering: "OR" or "AND".
#'     "OR" = keep if bio >= threshold OR chem >= threshold.
#'     "AND" = keep if bio >= threshold AND chem >= threshold
#' @param compounds_names Logical, include compound names in output (may increase size)
#' @param high_confidence Logical, apply strict high-confidence filters
#' @param remove_ties Logical, remove tied scores (keep only highest-ranked)
#' @param summarize Logical, collapse results to one row per feature
#'
#' @return Named list with three data frames:
#'   \describe{
#'     \item{full}{All annotations (optionally high-confidence filtered)}
#'     \item{filtered}{Top candidates meeting percentile + rank thresholds}
#'     \item{mini}{One row per feature with best compound/taxonomy}
#'   }
#'
#' @seealso \code{\link{weight_chemo}}, \code{\link{filter_high_confidence_only}},
#'     \code{\link{summarize_results}}
#'
#' @examples
#' \dontrun{
#' results <- clean_chemo(
#'   annot_table_wei_chemo = annotations,
#'   features_table = features,
#'   components_table = components,
#'   structure_organism_pairs_table = sop_table,
#'   candidates_final = 10,
#'   best_percentile = 0.9,
#'   minimal_ms1_bio = 0.5,
#'   minimal_ms1_chemo = 0.5,
#'   minimal_ms1_condition = "OR",
#'   compounds_names = TRUE,
#'   high_confidence = FALSE,
#'   remove_ties = FALSE,
#'   summarize = FALSE
#' )
#' }
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

  validate_clean_chemo_inputs(
    annot_table_wei_chemo = annot_table_wei_chemo,
    candidates_final = candidates_final,
    best_percentile = best_percentile,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    minimal_ms1_condition = minimal_ms1_condition,
    compounds_names = compounds_names,
    high_confidence = high_confidence,
    remove_ties = remove_ties,
    summarize = summarize
  )

  # Early exit for empty input
  if (nrow(annot_table_wei_chemo) == 0L) {
    logger::log_warn("Empty annotation table provided")
    return(annot_table_wei_chemo)
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
  # Core Filtering Pipeline - Single Source of Truth
  # ============================================================================

  # Step 1: Filter MS1 annotations based on score thresholds
  df_base <- filter_ms1_annotations(
    annot_table_wei_chemo = annot_table_wei_chemo,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    minimal_ms1_condition = minimal_ms1_condition
  )

  # Step 2: Rank and deduplicate - keep best structure per feature
  df_ranked <- rank_and_deduplicate(df_base)

  # ============================================================================
  # Apply Percentile Filter - Centralized in clean_chemo
  # ============================================================================

  # Apply percentile filtering HERE (not in minimize_results)
  # This is done BEFORE high-confidence filter to get accurate counts
  df_percentile <- apply_percentile_filter(df_ranked, best_percentile)

  # ============================================================================
  # Count Candidates BEFORE High-Confidence Filter
  # ============================================================================

  # Count evaluated candidates (all in df_ranked)
  # Count best candidates (those passing percentile in df_percentile)
  results_candidates <- count_candidates(df_ranked, df_percentile)

  # ============================================================================
  # Three-Tier Output Generation
  # ============================================================================

  # ============================================================================
  # Apply High-Confidence Filter for Filtered and Full Tiers
  # ============================================================================

  # Apply filter_high_confidence_only for filtered tier
  df_filtered <- df_percentile |>
    filter_high_confidence_only(context = "filtered") |>
    tidytable::filter(rank_final <= candidates_final)

  results_filtered <- df_filtered |>
    summarize_results(
      features_table = features_table,
      components_table = components_table,
      structure_organism_pairs_table = structure_organism_pairs_table,
      annot_table_wei_chemo = annot_table_wei_chemo,
      remove_ties = remove_ties,
      summarize = summarize
    ) |>
    tidytable::left_join(results_candidates, by = "feature_id") |>
    tidytable::mutate(
      # Set candidates_best to NA when no inchikey
      candidates_best = tidytable::if_else(
        is.na(candidate_structure_inchikey_connectivity_layer),
        NA_integer_,
        candidates_best
      )
    )

  # Tier 1: MINI - Extract from df_percentile BEFORE high-confidence filter
  # This ensures we get predicted taxonomy for features without inchikey

  # Extract best compound per feature
  df_compounds_mini <- df_percentile |>
    tidytable::arrange(tidytable::desc(score_weighted_chemo)) |>
    tidytable::distinct(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    ) |>
    tidytable::select(
      feature_id,
      label_compound = candidate_structure_name,
      adduct = candidate_adduct,
      smiles_no_stereo = candidate_structure_smiles_no_stereo,
      inchikey_connectivity_layer = candidate_structure_inchikey_connectivity_layer,
      library = candidate_library,
      error_mz = candidate_structure_error_mz,
      error_rt = candidate_structure_error_rt,
      organism_closest = candidate_structure_organism_occurrence_closest,
      score = score_weighted_chemo
    )

  # Extract structure-based taxonomy (when inchikey exists - use structure columns)
  df_structure_tax <- df_percentile |>
    tidytable::select(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      tidyselect::starts_with("candidate_structure_tax_cla"),
      tidyselect::starts_with("candidate_structure_tax_npc")
    ) |>
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::mutate(
      # Get best ClassyFire from structure (most specific level)
      label_classyfire_structure = tidytable::coalesce(
        candidate_structure_tax_cla_04dirpar,
        candidate_structure_tax_cla_03cla,
        candidate_structure_tax_cla_02sup,
        candidate_structure_tax_cla_01kin
      ),
      # Get best NPClassifier from structure (most specific level)
      label_npclassifier_structure = tidytable::coalesce(
        candidate_structure_tax_npc_03cla,
        candidate_structure_tax_npc_02sup,
        candidate_structure_tax_npc_01pat
      ),
      has_inchikey = !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::select(
      feature_id,
      has_inchikey,
      label_classyfire_structure,
      label_npclassifier_structure
    )

  # Extract predicted taxonomy with scores (when NO inchikey - use predicted columns)
  df_pred_tax <- df_percentile |>
    tidytable::select(tidytable::contains(c("feature_id", "feature_pred"))) |>
    tidytable::mutate(tidytable::across(
      tidytable::contains("score"),
      as.numeric
    ))

  if (nrow(df_pred_tax) > 0L) {
    # Bring level weights from parent env (same as weight_chemo parameters)
    weights <- get_taxonomy_weights()

    # For ClassyFire: compute weighted scores for ALL levels, pick single best
    df_cla <- compute_classyfire_taxonomy(df_pred_tax, weights)

    # For NPClassifier: compute weighted scores for ALL levels, pick single best
    df_npc <- compute_npclassifier_taxonomy(df_pred_tax, weights)

    # Combine ClassyFire and NPClassifier
    df_pred_tax <- df_cla |>
      tidytable::left_join(df_npc, by = "feature_id")
  } else {
    df_pred_tax <- tidytable::tidytable(
      feature_id = character(0),
      label_classyfire_predicted = character(0),
      label_npclassifier_predicted = character(0),
      score_classyfire = numeric(0),
      score_npclassifier = numeric(0)
    )
  }

  # Combine: use structure taxonomy when inchikey exists, predicted when not
  df_classes_mini <- df_structure_tax |>
    tidytable::left_join(df_pred_tax, by = "feature_id") |>
    tidytable::mutate(
      label_classyfire = tidytable::if_else(
        has_inchikey,
        label_classyfire_structure,
        label_classyfire_predicted
      ),
      label_npclassifier = tidytable::if_else(
        has_inchikey,
        label_npclassifier_structure,
        label_npclassifier_predicted
      ),
      # Keep scores only when using predicted (no inchikey)
      score_classyfire = tidytable::if_else(
        has_inchikey,
        NA_real_,
        score_classyfire
      ),
      score_npclassifier = tidytable::if_else(
        has_inchikey,
        NA_real_,
        score_npclassifier
      )
    ) |>
    tidytable::select(
      feature_id,
      has_inchikey,
      label_classyfire,
      label_npclassifier,
      score_classyfire,
      score_npclassifier
    ) |>
    tidytable::distinct() |>
    tidytable::filter(
      score_classyfire != 0 |
        score_npclassifier != 0 |
        is.na(score_classyfire) |
        is.na(score_npclassifier)
    )

  # Combine mini results and adjust candidates_best based on inchikey presence
  results_mini <- features_table |>
    tidytable::left_join(df_classes_mini, by = "feature_id") |>
    tidytable::left_join(results_candidates, by = "feature_id") |>
    tidytable::left_join(
      df_filtered |>
        tidytable::select(
          feature_id,
          label_compound = candidate_structure_name,
          adduct = candidate_adduct,
          smiles_no_stereo = candidate_structure_smiles_no_stereo,
          inchikey_connectivity_layer = candidate_structure_inchikey_connectivity_layer,
          library = candidate_library,
          error_mz = candidate_structure_error_mz,
          error_rt = candidate_structure_error_rt,
          organism_closest = candidate_structure_organism_occurrence_closest,
          score = score_weighted_chemo
        ),
      by = "feature_id"
    ) |>
    tidytable::mutate(
      # Set candidates to NA when no inchikey
      candidates_evaluated = tidytable::if_else(
        is.na(inchikey_connectivity_layer),
        NA_integer_,
        candidates_evaluated
      ),
      candidates_best = tidytable::if_else(
        is.na(inchikey_connectivity_layer),
        NA_integer_,
        candidates_best
      ),
      # Average taxonomy score when no inchikey (across available classifiers)
      .pred_denom = as.numeric(!is.na(score_classyfire)) +
        as.numeric(!is.na(score_npclassifier)),
      .pred_sum = tidytable::coalesce(score_classyfire, 0) +
        tidytable::coalesce(score_npclassifier, 0),
      .pred_avg = tidytable::if_else(
        .pred_denom > 0,
        .pred_sum / .pred_denom,
        NA_real_
      ),
      # Replace compound score with averaged taxonomy score when there is no inchikey
      score = tidytable::if_else(
        is.na(inchikey_connectivity_layer),
        .pred_avg,
        score
      )
    ) |>
    tidytable::select(
      tidytable::any_of(
        c(
          "feature_id",
          "rt",
          "mz",
          "label_classyfire",
          "label_npclassifier",
          "label_compound",
          "adduct",
          "smiles_no_stereo",
          "inchikey_connectivity_layer",
          "library",
          "error_mz",
          "error_rt",
          "organism_closest",
          "score",
          "candidates_evaluated",
          "candidates_best"
        )
      )
    ) |>
    tidytable::distinct() |>
    tidytable::mutate(
      label_classyfire = tidytable::if_else(
        condition = !is.na(score),
        true = label_classyfire,
        false = NA_character_
      ),
      label_npclassifier = tidytable::if_else(
        condition = !is.na(score),
        true = label_npclassifier,
        false = NA_character_
      )
    )

  # Tier 3: FULL - Optionally apply high-confidence filter
  df_full <- if (high_confidence) {
    df_ranked |> filter_high_confidence_only(context = "full")
  } else {
    df_ranked
  }

  results_full <- df_full |>
    summarize_results(
      features_table = features_table,
      components_table = components_table,
      structure_organism_pairs_table = structure_organism_pairs_table,
      annot_table_wei_chemo = annot_table_wei_chemo,
      remove_ties = remove_ties,
      summarize = summarize
    ) |>
    tidytable::left_join(results_candidates, by = "feature_id") |>
    tidytable::mutate(
      # Set candidates_best to NA when no inchikey
      candidates_best = tidytable::if_else(
        is.na(candidate_structure_inchikey_connectivity_layer),
        NA_integer_,
        candidates_best
      )
    )

  # ============================================================================
  # Optionally Remove Compound Names (After All Processing)
  # ============================================================================

  # Build results list
  results_list <- list(
    full = results_full,
    filtered = results_filtered,
    mini = results_mini
  )

  # Remove compound names from outputs if requested
  results_list <- remove_compound_names(results_list, compounds_names)

  # Clean up intermediate objects
  rm(
    annot_table_wei_chemo,
    df_base,
    df_ranked,
    df_percentile,
    df_filtered,
    df_full,
    results_candidates,
    features_table,
    components_table,
    structure_organism_pairs_table
  )

  return(results_list)
}
