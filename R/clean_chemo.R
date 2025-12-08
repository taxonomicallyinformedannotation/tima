# Helper Functions ----

#' Validate data frame with feature_id column
#' @keywords internal
#' @noRd
.validate_features_dataframe <- function(tbl) {
  if (!is.data.frame(tbl)) {
    stop(
      "features_table and components_table must be data frames",
      call. = FALSE
    )
  }
  if (!"feature_id" %in% names(tbl)) {
    stop(
      "features_table/components_table must contain feature_id column",
      call. = FALSE
    )
  }
}

# Exported Functions ----

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
#' @param max_per_score Integer
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
  summarize,
  max_per_score,
  score_chemical_cla_kingdom,
  score_chemical_cla_superclass,
  score_chemical_cla_class,
  score_chemical_cla_parent,
  score_chemical_npc_pathway,
  score_chemical_npc_superclass,
  score_chemical_npc_class
) {
  # Validate data frame
  if (!is.data.frame(annot_table_wei_chemo)) {
    stop("annot_table_wei_chemo must be a data frame", call. = FALSE)
  }

  # Basic required columns in annotation table
  required_cols <- c(
    "feature_id",
    "candidate_structure_inchikey_connectivity_layer",
    "score_biological",
    "score_chemical",
    "score_weighted_chemo",
    "candidate_score_pseudo_initial"
  )
  missing_cols <- setdiff(required_cols, names(annot_table_wei_chemo))
  if (length(missing_cols) > 0) {
    stop(
      "annot_table_wei_chemo missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Validate numeric parameters
  if (!is.numeric(candidates_final) || candidates_final < 1) {
    stop(
      "candidates_final must be a positive integer, got: ",
      candidates_final,
      call. = FALSE
    )
  }

  if (
    !is.numeric(best_percentile) || best_percentile < 0 || best_percentile > 1
  ) {
    stop(
      "best_percentile must be between 0 and 1, got: ",
      best_percentile,
      call. = FALSE
    )
  }

  if (
    !is.numeric(minimal_ms1_bio) || minimal_ms1_bio < 0 || minimal_ms1_bio > 1
  ) {
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

  # Validate max_per_score
  if (!is.numeric(max_per_score) || max_per_score < 1) {
    stop(
      "max_per_score must be a positive integer, got: ",
      max_per_score,
      call. = FALSE
    )
  }

  # Validate taxonomy weights if provided
  weights <- c(
    score_chemical_cla_kingdom,
    score_chemical_cla_superclass,
    score_chemical_cla_class,
    score_chemical_cla_parent,
    score_chemical_npc_pathway,
    score_chemical_npc_superclass,
    score_chemical_npc_class
  )
  if (
    any(!vapply(weights, function(x) is.null(x) || is.numeric(x), logical(1)))
  ) {
    stop(
      "taxonomy weight parameters must be numeric when provided",
      call. = FALSE
    )
  }
  weights_num <- weights[!vapply(weights, is.null, logical(1))]
  if (length(weights_num) > 0) {
    if (any(weights_num < 0 | weights_num > 1)) {
      stop("taxonomy weights must be within [0,1]", call. = FALSE)
    }
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
  # Ensure score columns are numeric
  annot_table_wei_chemo <- annot_table_wei_chemo |>
    tidytable::mutate(
      score_biological = as.numeric(score_biological),
      score_chemical = as.numeric(score_chemical)
    )

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
    tidytable::mutate(
      score_weighted_chemo = as.numeric(score_weighted_chemo),
      candidate_score_pseudo_initial = as.numeric(
        candidate_score_pseudo_initial
      )
    ) |>
    tidytable::arrange(tidytable::desc(x = score_weighted_chemo)) |>
    tidytable::distinct(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    ) |>
    tidytable::mutate(
      rank_initial = tidytable::dense_rank(x = -candidate_score_pseudo_initial),
      rank_final = tidytable::dense_rank(x = -score_weighted_chemo),
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
    tidytable::mutate(
      score_weighted_chemo = as.numeric(score_weighted_chemo)
    ) |>
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
    x = candidates_evaluated,
    y = candidates_best
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
      ws_par = as.numeric(feature_pred_tax_cla_04dirpar_score) *
        weights$w_cla_par,
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

#' Sample Candidates Per Group with RT Error Priority
#'
#' @description Internal helper to sample candidates per (feature_id, rank_final)
#'     group, prioritizing those with non-NA candidate_structure_error_rt values.
#'
#' @param df Data frame with ranked candidates
#' @param max_per_score Integer, maximum candidates to keep per group
#' @param seed Integer, random seed for reproducibility
#'
#' @return List with two elements:
#'   \describe{
#'     \item{df}{Filtered data frame with sampled candidates}
#'     \item{n_sampled_features}{Number of features affected by sampling}
#'   }
#' @keywords internal
sample_candidates_per_group <- function(df, max_per_score, seed = 42L) {
  if (nrow(df) == 0L) {
    return(list(df = df, n_sampled_features = 0L))
  }

  # Add group sizes per (feature_id, candidate_adduct, rank_final)
  df <- df |>
    tidytable::mutate(
      .n_per_group = tidytable::n(),
      .by = c(feature_id, candidate_adduct, rank_final)
    )

  # Count features that need sampling
  n_sampled_features <- df |>
    tidytable::filter(.n_per_group > max_per_score) |>
    tidytable::distinct(feature_id) |>
    nrow()

  # Groups that do NOT need sampling
  df_keep_all <- df |>
    tidytable::filter(.n_per_group <= max_per_score)

  # Groups that DO need sampling - prioritize RT error candidates
  has_rt_col <- "candidate_structure_error_rt" %in% names(df)

  df_needs_sampling <- df |>
    tidytable::filter(.n_per_group > max_per_score)

  if (nrow(df_needs_sampling) > 0L && has_rt_col) {
    # Add priority flag: TRUE for candidates with RT error
    df_needs_sampling <- df_needs_sampling |>
      tidytable::mutate(.rt_priority = !is.na(candidate_structure_error_rt))

    set.seed(seed)
    df_sampled <- df_needs_sampling |>
      tidytable::arrange(tidytable::desc(.rt_priority)) |>
      tidytable::slice_head(
        n = max_per_score,
        by = c(feature_id, candidate_adduct, rank_final)
      ) |>
      tidytable::mutate(
        annotation_note = paste0(
          "Sampled ",
          max_per_score,
          " of ",
          .n_per_group,
          " candidates with same score"
        )
      ) |>
      tidytable::select(-.rt_priority)
  } else if (nrow(df_needs_sampling) > 0L) {
    # No RT column, just sample randomly
    set.seed(seed)
    df_sampled <- df_needs_sampling |>
      tidytable::slice_sample(
        n = max_per_score,
        by = c(feature_id, candidate_adduct, rank_final)
      ) |>
      tidytable::mutate(
        annotation_note = paste0(
          "Sampled ",
          max_per_score,
          " of ",
          .n_per_group,
          " candidates with same score"
        )
      )
  } else {
    df_sampled <- tidytable::tidytable()
  }

  # Combine back
  df_result <- tidytable::bind_rows(df_keep_all, df_sampled) |>
    tidytable::select(-.n_per_group)

  # Extract annotation_note as a separate lookup table
  # annotation_note is per (feature_id, adduct, rank_final) group
  annotation_notes_lookup <- df_result |>
    tidytable::filter(!is.na(annotation_note)) |>
    tidytable::distinct(
      feature_id,
      candidate_adduct,
      rank_final,
      annotation_note
    )

  # Remove annotation_note from main data
  df_result <- df_result |>
    tidytable::select(-tidyselect::any_of("annotation_note"))

  list(
    df = df_result,
    n_sampled_features = n_sampled_features,
    annotation_notes = annotation_notes_lookup
  )
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
      tidytable::select(
        -tidyselect::any_of(x = "candidate_structure_name")
      )
    results_list$filtered <- results_list$filtered |>
      tidytable::select(
        -tidyselect::any_of(x = "candidate_structure_name")
      )
    results_list$full <- results_list$full |>
      tidytable::select(
        -tidyselect::any_of(x = "candidate_structure_name")
      )
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
#' @param max_per_score Integer, max candidates to keep per feature per score.
#'   If more exist, they are randomly sampled and a note is added. Default 7.
#' @param score_chemical_cla_kingdom Numeric (0-1), score for ClassyFire kingdom level
#' @param score_chemical_cla_superclass Numeric (0-1), score for ClassyFire superclass level
#' @param score_chemical_cla_class Numeric (0-1), score for ClassyFire class level
#' @param score_chemical_cla_parent Numeric (0-1), score for ClassyFire direct parent level
#' @param score_chemical_npc_pathway Numeric (0-1), score for NPClassifier pathway level
#' @param score_chemical_npc_superclass Numeric (0-1), score for NPClassifier superclass level
#' @param score_chemical_npc_class Numeric (0-1), score for NPClassifier class level
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
  annot_table_wei_chemo,
  components_table,
  features_table,
  structure_organism_pairs_table,
  candidates_final,
  best_percentile,
  minimal_ms1_bio,
  minimal_ms1_chemo,
  minimal_ms1_condition,
  compounds_names,
  high_confidence,
  remove_ties,
  summarize,
  # Explicit taxonomy weights (0-1)
  score_chemical_cla_kingdom = 0.1,
  score_chemical_cla_superclass = 0.2,
  score_chemical_cla_class = 0.3,
  score_chemical_cla_parent = 0.4,
  score_chemical_npc_pathway = 0.3,
  score_chemical_npc_superclass = 0.2,
  score_chemical_npc_class = 0.1,
  max_per_score = 7L
) {
  # Input Validation ----

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
    summarize = summarize,
    max_per_score = max_per_score,
    score_chemical_cla_kingdom = score_chemical_cla_kingdom,
    score_chemical_cla_superclass = score_chemical_cla_superclass,
    score_chemical_cla_class = score_chemical_cla_class,
    score_chemical_cla_parent = score_chemical_cla_parent,
    score_chemical_npc_pathway = score_chemical_npc_pathway,
    score_chemical_npc_superclass = score_chemical_npc_superclass,
    score_chemical_npc_class = score_chemical_npc_class
  )

  # Early exit for empty input
  if (nrow(annot_table_wei_chemo) == 0L) {
    log_warn("Empty annotation table provided")
    return(annot_table_wei_chemo)
  }

  # Validate features and components schema minimally
  purrr::walk(
    .x = list(features_table, components_table),
    .f = .validate_features_dataframe
  )
  if (!is.data.frame(structure_organism_pairs_table)) {
    stop("structure_organism_pairs_table must be a data frame", call. = FALSE)
  }

  # Ensure Score Columns are Numeric ----

  # Convert score columns to numeric to prevent "non-numeric argument" errors
  # These columns may be character if loaded from TSV files
  score_columns <- c(
    "score_biological",
    "score_chemical",
    "score_weighted_chemo",
    "candidate_score_pseudo_initial",
    "candidate_score_similarity",
    "candidate_score_sirius_csi"
  )

  cols_to_convert <- intersect(score_columns, names(annot_table_wei_chemo))
  if (length(cols_to_convert) > 0L) {
    annot_table_wei_chemo <- annot_table_wei_chemo |>
      tidytable::mutate(
        tidytable::across(
          .cols = tidyselect::all_of(cols_to_convert),
          .fns = as.numeric
        )
      )
  }

  # Log Processing Parameters ----

  log_info("Cleaning chemically weighted annotations")
  log_debug("Keeping top %d candidates per feature", candidates_final)
  log_debug(
    "Using best_percentile: {best_percentile} for consistent filtering"
  )
  log_debug(
    "Options - High confidence: %s, Remove ties: %s, Summarize: %s",
    high_confidence,
    remove_ties,
    summarize
  )
  log_info(
    "Filtering top %d candidates and keeping only MS1 candidates with minimum %s biological score %s %s chemical score",
    candidates_final,
    minimal_ms1_bio,
    minimal_ms1_condition,
    minimal_ms1_chemo
  )
  log_debug(
    "Sampling max_per_score = %d candidates per (feature_id, rank_final) after filters",
    max_per_score
  )

  # Core Filtering Pipeline ----

  # Step 1: Filter MS1 annotations based on score thresholds
  df_base <- filter_ms1_annotations(
    annot_table_wei_chemo = annot_table_wei_chemo,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    minimal_ms1_condition = minimal_ms1_condition
  )

  # Step 2: Rank and deduplicate ----
  df_ranked <- rank_and_deduplicate(df_base)

  ## Sampling ----
  # Sample candidates per (feature_id, rank_final) group
  # Prioritize candidates with non-NA candidate_structure_error_rt
  sampling_result <- sample_candidates_per_group(
    df = df_ranked,
    max_per_score = max_per_score,
    seed = 42L
  )

  n_sampled_features <- sampling_result$n_sampled_features
  annotation_notes_lookup <- sampling_result$annotation_notes
  df_ranked <- sampling_result$df

  if (n_sampled_features > 0L) {
    log_info(
      "Sampling candidates for %d features with more than %d candidates per score",
      n_sampled_features,
      max_per_score
    )
  }

  # Apply Percentile Filter -----
  # This is done BEFORE high-confidence filter to get accurate counts
  df_percentile <- apply_percentile_filter(df_ranked, best_percentile)

  # Count Candidates BEFORE High-Confidence Filter ----

  results_candidates <- count_candidates(df_ranked, df_percentile)

  # Three-Tier Output Generation ----

  # Apply High-Confidence Filter for Filtered and Full Tiers ----

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
      summarize = summarize,
      annotation_notes_lookup = annotation_notes_lookup
    ) |>
    tidytable::left_join(y = results_candidates) |>
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
  # (downstream mini assembly uses filtered tier instead)
  df_structure_tax <- df_percentile |>
    tidytable::select(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      tidyselect::starts_with(match = "candidate_structure_tax_cla"),
      tidyselect::starts_with(match = "candidate_structure_tax_npc")
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
    tidytable::select(
      tidytable::contains(match = c("feature_id", "feature_pred"))
    ) |>
    tidytable::mutate(
      tidytable::across(
        tidytable::contains(match = "score"),
        as.numeric
      )
    )

  if (nrow(df_pred_tax) > 0L) {
    # Build weights list from explicit parameters
    weights <- list(
      w_cla_kin = score_chemical_cla_kingdom,
      w_cla_sup = score_chemical_cla_superclass,
      w_cla_cla = score_chemical_cla_class,
      w_cla_par = score_chemical_cla_parent,
      w_npc_pat = score_chemical_npc_pathway,
      w_npc_sup = score_chemical_npc_superclass,
      w_npc_cla = score_chemical_npc_class
    )

    # For ClassyFire: compute weighted scores for ALL levels, pick single best
    df_cla <- compute_classyfire_taxonomy(df_pred_tax, weights)

    # For NPClassifier: compute weighted scores for ALL levels, pick single best
    df_npc <- compute_npclassifier_taxonomy(df_pred_tax, weights)

    # Combine ClassyFire and NPClassifier
    df_pred_tax <- df_cla |>
      tidytable::left_join(y = df_npc)
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
    tidytable::left_join(y = df_pred_tax) |>
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
    tidytable::left_join(y = df_classes_mini) |>
    tidytable::left_join(y = results_filtered) |>
    tidytable::left_join(
      y = df_filtered |>
        tidytable::select(
          feature_id,
          candidate_structure_name,
          candidate_adduct,
          candidate_structure_smiles_no_stereo,
          candidate_structure_inchikey_connectivity_layer,
          candidate_library,
          candidate_structure_error_mz,
          candidate_structure_error_rt,
          candidate_structure_organism_occurrence_closest,
          score_weighted_chemo
        )
    ) |>
    tidytable::rename(
      label_compound = candidate_structure_name,
      adduct = candidate_adduct,
      smiles_no_stereo = candidate_structure_smiles_no_stereo,
      inchikey_connectivity_layer = candidate_structure_inchikey_connectivity_layer,
      library = candidate_library,
      error_mz = candidate_structure_error_mz,
      error_rt = candidate_structure_error_rt,
      organism_closest = candidate_structure_organism_occurrence_closest,
      score = score_weighted_chemo
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
        x = c(
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
          "candidates_best",
          "note" = "annotation_note"
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
    df_ranked |>
      filter_high_confidence_only(context = "full")
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
      summarize = summarize,
      annotation_notes_lookup = annotation_notes_lookup
    ) |>
    tidytable::left_join(y = results_candidates) |>
    tidytable::mutate(
      # Set candidates_best to NA when no inchikey
      candidates_best = tidytable::if_else(
        is.na(candidate_structure_inchikey_connectivity_layer),
        NA_integer_,
        candidates_best
      )
    )

  # Optionally Remove Compound Names (After All Processing) ----

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
