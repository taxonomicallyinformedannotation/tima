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
  # Core Filtering Pipeline - Single Source of Truth
  # ============================================================================

  # Step 1: Filter MS1 annotations based on score thresholds
  # Keep MS1 only if good biological OR/AND chemical consistency score obtained
  df_base <- if (minimal_ms1_condition == "OR") {
    annot_table_wei_chemo |>
      tidytable::filter(
        (!is.na(candidate_score_similarity) |
          !is.na(candidate_score_sirius_csi)) |
          (score_biological >= minimal_ms1_bio |
            score_chemical >= minimal_ms1_chemo)
      )
  } else {
    # minimal_ms1_condition == "AND"
    annot_table_wei_chemo |>
      tidytable::filter(
        (!is.na(candidate_score_similarity) |
          !is.na(candidate_score_sirius_csi)) |
          (score_biological >= minimal_ms1_bio &
            score_chemical >= minimal_ms1_chemo)
      )
  }

  # Step 2: Rank and deduplicate - keep best structure per feature
  df_ranked <- df_base |>
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

  # ============================================================================
  # Apply Percentile Filter - Centralized in clean_chemo
  # ============================================================================

  # Apply percentile filtering HERE (not in minimize_results)
  # This is done BEFORE high-confidence filter to get accurate counts
  ## COMMENT: Actually not sure it is the best idea
  df_percentile <- df_ranked |>
    tidytable::group_by(feature_id) |>
    tidytable::filter(
      score_weighted_chemo >=
        best_percentile * max(score_weighted_chemo, na.rm = TRUE)
    ) |>
    tidytable::ungroup()

  # ============================================================================
  # Count Candidates BEFORE High-Confidence Filter
  # ============================================================================

  # Count evaluated candidates (all in df_ranked)
  # Count best candidates (those passing percentile in df_percentile)
  results_candidates <- df_ranked |>
    tidytable::group_by(feature_id) |>
    tidytable::add_count(name = "candidates_evaluated") |>
    tidytable::ungroup() |>
    tidytable::distinct(feature_id, candidates_evaluated) |>
    tidytable::left_join(
      df_percentile |>
        tidytable::group_by(feature_id) |>
        tidytable::add_count(name = "candidates_best") |>
        tidytable::ungroup() |>
        tidytable::distinct(feature_id, candidates_best),
      by = "feature_id"
    )

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
    w_cla_kin <- get("score_chemical_cla_kingdom", envir = parent.frame())
    w_cla_sup <- get("score_chemical_cla_superclass", envir = parent.frame())
    w_cla_cla <- get("score_chemical_cla_class", envir = parent.frame())
    w_cla_par <- get("score_chemical_cla_parent", envir = parent.frame())
    w_npc_pat <- get("score_chemical_npc_pathway", envir = parent.frame())
    w_npc_sup <- get("score_chemical_npc_superclass", envir = parent.frame())
    w_npc_cla <- get("score_chemical_npc_class", envir = parent.frame())

    # For ClassyFire: compute weighted scores for ALL levels, pick single best
    df_cla <- df_pred_tax |>
      tidytable::mutate(
        # Compute weighted score for each level
        ws_kin = as.numeric(feature_pred_tax_cla_01kin_score) * w_cla_kin,
        ws_sup = as.numeric(feature_pred_tax_cla_02sup_score) * w_cla_sup,
        ws_cla = as.numeric(feature_pred_tax_cla_03cla_score) * w_cla_cla,
        ws_par = as.numeric(feature_pred_tax_cla_04dirpar_score) * w_cla_par,
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

    # For NPClassifier: compute weighted scores for ALL levels, pick single best
    df_npc <- df_pred_tax |>
      tidytable::mutate(
        # Compute weighted score for each level
        ws_pat = as.numeric(feature_pred_tax_npc_01pat_score) * w_npc_pat,
        ws_sup = as.numeric(feature_pred_tax_npc_02sup_score) * w_npc_sup,
        ws_cla = as.numeric(feature_pred_tax_npc_03cla_score) * w_npc_cla,
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

  # Remove compound names from outputs if requested
  # Done AFTER all processing to avoid "column doesn't exist" errors
  if (!compounds_names) {
    results_mini <- results_mini |>
      tidytable::select(-tidyselect::any_of("candidate_structure_name"))
    results_filtered <- results_filtered |>
      tidytable::select(-tidyselect::any_of("candidate_structure_name"))
    results_full <- results_full |>
      tidytable::select(-tidyselect::any_of("candidate_structure_name"))
  }

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

  return(
    list(
      "full" = results_full,
      "filtered" = results_filtered,
      "mini" = results_mini
    )
  )
}
