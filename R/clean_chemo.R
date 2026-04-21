# Helper Functions ----

#' Validate data frame with feature_id column
#' @keywords internal
#' @noRd
.validate_features_dataframe <- function(tbl) {
  if (!is.data.frame(tbl)) {
    tima_abort(
      problem = "features_table and components_table must be data frames",
      class = c("tima_validation_error", "tima_error")
    )
  }
  if (!"feature_id" %in% names(tbl)) {
    tima_abort(
      problem = "features_table/components_table must contain feature_id column",
      class = c("tima_validation_error", "tima_error")
    )
  }
}

# Exported Functions ----

#' Validate Inputs for clean_chemo
#'
#' @description Internal helper to validate all input parameters for
#'     clean_chemo.
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
    tima_abort(
      problem = "annot_table_wei_chemo must be a data frame",
      class = c("tima_validation_error", "tima_error")
    )
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
    tima_abort(
      problem = paste0(
        "annot_table_wei_chemo missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Validate numeric parameters
  if (!is.numeric(candidates_final) || candidates_final < 1) {
    tima_abort(
      problem = paste0(
        "candidates_final must be a positive integer, got: ",
        candidates_final
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (
    !is.numeric(best_percentile) || best_percentile < 0 || best_percentile > 1
  ) {
    tima_abort(
      problem = paste0(
        "best_percentile must be between 0 and 1, got: ",
        best_percentile
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (
    !is.numeric(minimal_ms1_bio) || minimal_ms1_bio < 0 || minimal_ms1_bio > 1
  ) {
    tima_abort(
      problem = paste0(
        "minimal_ms1_bio must be between 0 and 1, got: ",
        minimal_ms1_bio
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (
    !is.numeric(minimal_ms1_chemo) ||
      minimal_ms1_chemo < 0 ||
      minimal_ms1_chemo > 1
  ) {
    tima_abort(
      problem = paste0(
        "minimal_ms1_chemo must be between 0 and 1, got: ",
        minimal_ms1_chemo
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Validate condition
  if (!minimal_ms1_condition %in% c("OR", "AND")) {
    tima_abort(
      problem = paste0(
        "minimal_ms1_condition must be 'OR' or 'AND', got: ",
        minimal_ms1_condition
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Validate logical parameters
  logical_params <- list(
    compounds_names = compounds_names,
    high_confidence = high_confidence,
    remove_ties = remove_ties,
    summarize = summarize
  )

  is_valid_logical <- vapply(X = logical_params, FUN = is.logical, logical(1))
  if (!all(is_valid_logical)) {
    invalid_params <- names(logical_params)[!is_valid_logical]
    tima_abort(
      problem = paste0(
        "Parameter(s) must be logical (TRUE/FALSE): ",
        paste(invalid_params, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Validate max_per_score
  if (!is.numeric(max_per_score) || max_per_score < 1) {
    tima_abort(
      problem = paste0(
        "max_per_score must be a positive integer, got: ",
        max_per_score
      ),
      class = c("tima_validation_error", "tima_error")
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
    !all(vapply(
      X = weights,
      FUN = function(x) is.null(x) || is.numeric(x),
      logical(1)
    ))
  ) {
    tima_abort(
      problem = "taxonomy weight parameters must be numeric when provided",
      class = c("tima_validation_error", "tima_error")
    )
  }
  weights_num <- weights[!vapply(X = weights, FUN = is.null, logical(1))]
  if (length(weights_num) > 0) {
    if (any(weights_num < 0 | weights_num > 1)) {
      tima_abort(
        problem = "taxonomy weights must be within [0,1]",
        class = c("tima_validation_error", "tima_error")
      )
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

  # Keep annotations with MS2 data OR MS1 meeting score thresholds.
  # "has MS2 data" = has a direct spectral similarity score, a raw SIRIUS CSI
  # score, or a SIRIUS confidence score (top-1 per feature from v5/v6).
  # Build the expression dynamically so it gracefully handles data frames that
  # do not contain the confidence column (e.g., older fixture files or pure
  # spectral annotation runs).
  has_confidence_col <- "candidate_score_sirius_confidence" %in%
    names(annot_table_wei_chemo)

  if (has_confidence_col) {
    has_ms2 <- quote(
      !is.na(candidate_score_similarity) |
        !is.na(candidate_score_sirius_csi) |
        (!is.na(candidate_score_sirius_confidence) &
          as.numeric(candidate_score_sirius_confidence) > 0)
    )
  } else {
    has_ms2 <- quote(
      !is.na(candidate_score_similarity) | !is.na(candidate_score_sirius_csi)
    )
  }

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
      cla_kin_valid = !is.na(feature_pred_tax_cla_01kin_val) &
        feature_pred_tax_cla_01kin_val != "notClassified",
      cla_sup_valid = !is.na(feature_pred_tax_cla_02sup_val) &
        feature_pred_tax_cla_02sup_val != "notClassified",
      cla_cla_valid = !is.na(feature_pred_tax_cla_03cla_val) &
        feature_pred_tax_cla_03cla_val != "notClassified",
      cla_par_valid = !is.na(feature_pred_tax_cla_04dirpar_val) &
        feature_pred_tax_cla_04dirpar_val != "notClassified",
      # Compute weighted score for each level
      ws_kin = tidytable::if_else(
        cla_kin_valid,
        as.numeric(feature_pred_tax_cla_01kin_score) * weights$w_cla_kin,
        NA_real_
      ),
      ws_sup = tidytable::if_else(
        cla_sup_valid,
        as.numeric(feature_pred_tax_cla_02sup_score) * weights$w_cla_sup,
        NA_real_
      ),
      ws_cla = tidytable::if_else(
        cla_cla_valid,
        as.numeric(feature_pred_tax_cla_03cla_score) * weights$w_cla_cla,
        NA_real_
      ),
      ws_par = tidytable::if_else(
        cla_par_valid,
        as.numeric(feature_pred_tax_cla_04dirpar_score) * weights$w_cla_par,
        NA_real_
      ),
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
    tidytable::filter(
      !is.na(label_classyfire_predicted),
      label_classyfire_predicted != "notClassified"
    ) |>
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
      npc_pat_valid = !is.na(feature_pred_tax_npc_01pat_val) &
        feature_pred_tax_npc_01pat_val != "notClassified",
      npc_sup_valid = !is.na(feature_pred_tax_npc_02sup_val) &
        feature_pred_tax_npc_02sup_val != "notClassified",
      npc_cla_valid = !is.na(feature_pred_tax_npc_03cla_val) &
        feature_pred_tax_npc_03cla_val != "notClassified",
      # Compute weighted score for each level
      ws_pat = tidytable::if_else(
        npc_pat_valid,
        as.numeric(feature_pred_tax_npc_01pat_score) * weights$w_npc_pat,
        NA_real_
      ),
      ws_sup = tidytable::if_else(
        npc_sup_valid,
        as.numeric(feature_pred_tax_npc_02sup_score) * weights$w_npc_sup,
        NA_real_
      ),
      ws_cla = tidytable::if_else(
        npc_cla_valid,
        as.numeric(feature_pred_tax_npc_03cla_score) * weights$w_npc_cla,
        NA_real_
      ),
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
    tidytable::filter(
      !is.na(label_npclassifier_predicted),
      label_npclassifier_predicted != "notClassified"
    ) |>
    tidytable::select(
      feature_id,
      label_npclassifier_predicted,
      score_npclassifier
    ) |>
    tidytable::distinct()
}

#' Sample Candidates Per Group with RT Error Priority
#'
#' @description Internal helper to sample candidates per (feature_id,
#'     rank_final)
#' group, prioritizing those with non-NA candidate_structure_error_rt values.
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
  annotation_notes_lookup <- tidytable::tidytable()

  if ("annotation_note" %in% names(df_result)) {
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
  }

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

#' Coerce score columns to numeric
#'
#' @description Internal helper that normalizes score-like columns loaded as
#'     character values (e.g., from TSV/CSV inputs) before numeric operations.
#'
#' @param annot_table_wei_chemo Data frame with annotation scores
#'
#' @return Data frame with existing score columns coerced to numeric
#' @keywords internal
coerce_score_columns <- function(annot_table_wei_chemo) {
  score_columns <- c(
    "score_biological",
    "score_chemical",
    "score_weighted_chemo",
    "candidate_score_pseudo_initial",
    "candidate_score_similarity",
    "candidate_score_similarity_forward",
    "candidate_score_similarity_reverse",
    "candidate_score_sirius_csi",
    "candidate_score_sirius_confidence"
  )

  cols_to_convert <- intersect(score_columns, names(annot_table_wei_chemo))
  if (length(cols_to_convert) == 0L) {
    return(annot_table_wei_chemo)
  }

  annot_table_wei_chemo |>
    tidytable::mutate(
      tidytable::across(
        .cols = tidyselect::all_of(cols_to_convert),
        .fns = as.numeric
      )
    )
}

#' Prepare ranked candidate tables for downstream tiers
#'
#' @param annot_table_wei_chemo Data frame with normalized score columns
#' @param minimal_ms1_bio Numeric threshold for biological score
#' @param minimal_ms1_chemo Numeric threshold for chemical score
#' @param minimal_ms1_condition Character filter mode ("OR" or "AND")
#' @param best_percentile Numeric percentile threshold
#' @param max_per_score Integer max candidates per score group
#'
#' @return Named list with ranked/percentile tables and candidate counts
#' @keywords internal
prepare_ranked_candidates <- function(
  annot_table_wei_chemo,
  minimal_ms1_bio,
  minimal_ms1_chemo,
  minimal_ms1_condition,
  best_percentile,
  max_per_score
) {
  df_base <- filter_ms1_annotations(
    annot_table_wei_chemo = annot_table_wei_chemo,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    minimal_ms1_condition = minimal_ms1_condition
  )

  df_ranked <- rank_and_deduplicate(df_base)

  sampling_result <- sample_candidates_per_group(
    df = df_ranked,
    max_per_score = max_per_score,
    seed = 42L
  )

  df_ranked <- sampling_result$df
  df_percentile <- apply_percentile_filter(
    df = df_ranked,
    best_percentile = best_percentile
  )

  list(
    df_ranked = df_ranked,
    df_percentile = df_percentile,
    results_candidates = count_candidates(df_ranked, df_percentile),
    n_sampled_features = sampling_result$n_sampled_features,
    annotation_notes_lookup = sampling_result$annotation_notes
  )
}

#' Build mini-tier taxonomy table
#'
#' @param df_percentile Percentile-filtered candidate table
#' @param score_chemical_cla_kingdom ClassyFire kingdom weight
#' @param score_chemical_cla_superclass ClassyFire superclass weight
#' @param score_chemical_cla_class ClassyFire class weight
#' @param score_chemical_cla_parent ClassyFire direct parent weight
#' @param score_chemical_npc_pathway NPClassifier pathway weight
#' @param score_chemical_npc_superclass NPClassifier superclass weight
#' @param score_chemical_npc_class NPClassifier class weight
#'
#' @return Mini-tier taxonomy table keyed by feature_id
#' @keywords internal
build_mini_taxonomy_table <- function(
  df_percentile,
  score_chemical_cla_kingdom,
  score_chemical_cla_superclass,
  score_chemical_cla_class,
  score_chemical_cla_parent,
  score_chemical_npc_pathway,
  score_chemical_npc_superclass,
  score_chemical_npc_class
) {
  normalize_tax_label <- function(x) {
    vals <- trimws(as.character(x))
    vals[
      vals %in% c("", "notClassified", "empty", "N/A", "null")
    ] <- NA_character_
    vals
  }

  # Prepare all candidates with their structure taxonomy scored by
  # score_weighted_chemo so they compete on equal footing with predicted labels.
  df_all <- df_percentile |>
    tidytable::mutate(
      score_weighted_chemo = as.numeric(score_weighted_chemo),
      has_inchikey = !is.na(candidate_structure_inchikey_connectivity_layer),
      tidytable::across(
        .cols = tidyselect::starts_with(match = "candidate_structure_tax_"),
        .fns = normalize_tax_label
      ),
      .label_cla_str = tidytable::coalesce(
        candidate_structure_tax_cla_04dirpar,
        candidate_structure_tax_cla_03cla,
        candidate_structure_tax_cla_02sup,
        candidate_structure_tax_cla_01kin
      ),
      .label_npc_str = tidytable::coalesce(
        candidate_structure_tax_npc_03cla,
        candidate_structure_tax_npc_02sup,
        candidate_structure_tax_npc_01pat
      )
    )

  # has_inchikey flag at feature level
  df_has_ik <- df_all |>
    tidytable::summarize(
      has_inchikey = any(has_inchikey, na.rm = TRUE),
      .by = feature_id
    )

  # Best structure label per feature (highest score_weighted_chemo wins)
  df_str_cla <- df_all |>
    tidytable::filter(!is.na(.label_cla_str)) |>
    tidytable::arrange(tidytable::desc(score_weighted_chemo)) |>
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::select(
      feature_id,
      label_classyfire_str = .label_cla_str,
      score_cla_str = score_weighted_chemo
    )

  df_str_npc <- df_all |>
    tidytable::filter(!is.na(.label_npc_str)) |>
    tidytable::arrange(tidytable::desc(score_weighted_chemo)) |>
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::select(
      feature_id,
      label_npclassifier_str = .label_npc_str,
      score_npc_str = score_weighted_chemo
    )

  # Best predicted label per feature (highest prediction score wins)
  df_pred_tax <- df_percentile |>
    tidytable::select(
      tidyselect::contains(match = c("feature_id", "feature_pred"))
    ) |>
    tidytable::mutate(
      tidytable::across(
        tidyselect::contains(match = "score"),
        as.numeric
      )
    )

  if (nrow(df_pred_tax) > 0L) {
    weights <- list(
      w_cla_kin = score_chemical_cla_kingdom,
      w_cla_sup = score_chemical_cla_superclass,
      w_cla_cla = score_chemical_cla_class,
      w_cla_par = score_chemical_cla_parent,
      w_npc_pat = score_chemical_npc_pathway,
      w_npc_sup = score_chemical_npc_superclass,
      w_npc_cla = score_chemical_npc_class
    )

    df_pred_cla <- compute_classyfire_taxonomy(df_pred_tax, weights) |>
      tidytable::arrange(tidytable::desc(score_classyfire)) |>
      tidytable::distinct(feature_id, .keep_all = TRUE)

    df_pred_npc <- compute_npclassifier_taxonomy(df_pred_tax, weights) |>
      tidytable::arrange(tidytable::desc(score_npclassifier)) |>
      tidytable::distinct(feature_id, .keep_all = TRUE)
  } else {
    df_pred_cla <- tidytable::tidytable(
      feature_id = character(0),
      label_classyfire_predicted = character(0),
      score_classyfire = numeric(0)
    )
    df_pred_npc <- tidytable::tidytable(
      feature_id = character(0),
      label_npclassifier_predicted = character(0),
      score_npclassifier = numeric(0)
    )
  }

  # pmax across both pools: structure label wins when its score_weighted_chemo
  # >= prediction score, otherwise the predicted label wins.
  df_has_ik |>
    tidytable::left_join(y = df_str_cla) |>
    tidytable::left_join(y = df_str_npc) |>
    tidytable::left_join(y = df_pred_cla) |>
    tidytable::left_join(y = df_pred_npc) |>
    tidytable::mutate(
      label_classyfire = tidytable::case_when(
        !is.na(score_cla_str) &
          !is.na(score_classyfire) &
          score_cla_str >= score_classyfire ~ label_classyfire_str,
        !is.na(score_classyfire) ~ label_classyfire_predicted,
        !is.na(label_classyfire_str) ~ label_classyfire_str,
        TRUE ~ NA_character_
      ),
      # No uncertainty score when label comes from a confirmed InChIKey structure
      score_classyfire = tidytable::case_when(
        !is.na(score_cla_str) &
          !is.na(score_classyfire) &
          score_cla_str >= score_classyfire ~ NA_real_,
        !is.na(score_classyfire) ~ score_classyfire,
        TRUE ~ NA_real_
      ),
      label_npclassifier = tidytable::case_when(
        !is.na(score_npc_str) &
          !is.na(score_npclassifier) &
          score_npc_str >= score_npclassifier ~ label_npclassifier_str,
        !is.na(score_npclassifier) ~ label_npclassifier_predicted,
        !is.na(label_npclassifier_str) ~ label_npclassifier_str,
        TRUE ~ NA_character_
      ),
      score_npclassifier = tidytable::case_when(
        !is.na(score_npc_str) &
          !is.na(score_npclassifier) &
          score_npc_str >= score_npclassifier ~ NA_real_,
        !is.na(score_npclassifier) ~ score_npclassifier,
        TRUE ~ NA_real_
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
    tidytable::distinct()
}

#' Build mini-tier result table
#'
#' @param features_table Feature metadata table
#' @param df_classes_mini Mini taxonomy table keyed by feature_id
#' @param results_filtered Filtered-tier summarized results
#' @param df_filtered Filtered candidate table prior to summarization
#' @param xrefs_table Optional cross-reference table
#'
#' @return Mini-tier output table
#' @keywords internal
build_mini_results_table <- function(
  features_table,
  df_classes_mini,
  results_filtered,
  df_filtered,
  xrefs_table = NULL
) {
  results_mini <- features_table |>
    tidytable::left_join(y = df_classes_mini) |>
    tidytable::left_join(y = results_filtered) |>
    tidytable::left_join(
      y = df_filtered |>
        tidytable::select(
          tidyselect::any_of(c(
            "feature_id",
            "candidate_structure_name",
            "candidate_adduct",
            "candidate_structure_smiles_no_stereo",
            "candidate_structure_inchikey_connectivity_layer",
            "candidate_library",
            "candidate_structure_error_mz",
            "candidate_structure_error_rt",
            "candidate_structure_organism_occurrence_closest",
            "candidate_structure_tag",
            "score_weighted_chemo"
          ))
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
      .pred_denom = as.numeric(!is.na(score_classyfire)) +
        as.numeric(!is.na(score_npclassifier)),
      .pred_sum = tidytable::coalesce(score_classyfire, 0) +
        tidytable::coalesce(score_npclassifier, 0),
      .pred_avg = tidytable::if_else(
        .pred_denom > 0,
        .pred_sum / .pred_denom,
        NA_real_
      ),
      score = tidytable::if_else(
        is.na(inchikey_connectivity_layer),
        .pred_avg,
        score
      )
    ) |>
    tidytable::select(
      tidyselect::any_of(
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
          "tag" = "candidate_structure_tag",
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

  if (!is.null(xrefs_table) && nrow(xrefs_table) > 0L) {
    results_mini <- results_mini |>
      tidytable::rename(
        candidate_structure_inchikey_connectivity_layer = inchikey_connectivity_layer
      ) |>
      .add_xrefs_to_df(xrefs = xrefs_table) |>
      tidytable::rename(
        inchikey_connectivity_layer = candidate_structure_inchikey_connectivity_layer
      )

    mini_id_cols <- grep(
      pattern = "^candidate_structure_id_",
      x = names(results_mini),
      value = TRUE
    )

    if (length(mini_id_cols) > 0L) {
      results_mini <- results_mini |>
        tidytable::rename_with(
          ~ sub("^candidate_structure_", "", .x),
          .cols = tidyselect::all_of(mini_id_cols)
        )
    }
  }

  results_mini
}

#' @title Clean Chemical Annotations
#'
#' @description Cleans and filters chemically weighted annotation results
#'     through
#' a multi-tier pipeline. Applies MS1 score thresholds, percentile filtering,
#' ranking, and optional high-confidence filtering. Returns three-tier output:
#' full (comprehensive), filtered (top candidates), and mini (one row per
#'     feature).
#'
#' @include add_xrefs_to_annotations.R
#' @include filter_high_confidence_only.R
#' @include summarize_results.R
#'
#' @param annot_table_wei_chemo Data frame with chemically weighted annotations.
#' Required columns: feature_id,
#'     candidate_structure_inchikey_connectivity_layer,
#' score_weighted_chemo, score_biological, score_chemical,
#'     candidate_score_pseudo_initial
#' @param components_table Data frame with molecular network component
#'     assignments.
#'     Required columns: feature_id, component_id
#' @param features_table Data frame with feature metadata (RT, m/z, etc.).
#'     Required columns: feature_id
#' @param structure_organism_pairs_table Data frame linking structures to
#'     organisms.
#'     Required columns: structure_inchikey_connectivity_layer
#' @param candidates_final Integer, number of top candidates to retain per
#'     feature (>= 1)
#' @param best_percentile Numeric (0-1), percentile threshold for score
#'     filtering.
#' Candidates with scores >= percentile * max_score are kept. Default: 0.9 (90th
#'     percentile)
#' @param minimal_ms1_bio Numeric (0-1), minimum biological score for MS1-only
#'     annotations
#' @param minimal_ms1_chemo Numeric (0-1), minimum chemical score for MS1-only
#'     annotations
#' @param minimal_ms1_condition Character, logical operator for MS1 filtering:
#'     "OR" or "AND".
#'     "OR" = keep if bio >= threshold OR chem >= threshold.
#'     "AND" = keep if bio >= threshold AND chem >= threshold
#' @param compounds_names Logical, include compound names in output (may
#'     increase size)
#' @param high_confidence Logical, apply strict high-confidence filters
#' @param remove_ties Logical, remove tied scores (keep only highest-ranked)
#' @param summarize Logical, collapse results to one row per feature
#' @param max_per_score Integer, max candidates to keep per feature per score.
#'   If more exist, they are randomly sampled and a note is added. Default 7.
#' @param score_chemical_cla_kingdom Numeric (0-1), score for ClassyFire kingdom
#'     level
#' @param score_chemical_cla_superclass Numeric (0-1), score for ClassyFire
#'     superclass level
#' @param score_chemical_cla_class Numeric (0-1), score for ClassyFire class
#'     level
#' @param score_chemical_cla_parent Numeric (0-1), score for ClassyFire direct
#'     parent level
#' @param score_chemical_npc_pathway Numeric (0-1), score for NPClassifier
#'     pathway level
#' @param score_chemical_npc_superclass Numeric (0-1), score for NPClassifier
#'     superclass level
#' @param score_chemical_npc_class Numeric (0-1), score for NPClassifier class
#'     level
#' @param xrefs_table Optional data frame with columns inchikey/prefix/id from
#'   get_compounds_xrefs(), used to add candidate_structure_id_* columns before
#'   summarization.
#'
#' @return Named list with three data frames:
#'   \describe{
#'     \item{full}{All annotations (optionally high-confidence filtered)}
#'     \item{filtered}{Top candidates meeting percentile + rank thresholds}
#'     \item{mini}{One row per feature with best compound/taxonomy}
#'   }
#'
#' @seealso \code{\link{weight_chemo}},
#'     \code{\link{filter_high_confidence_only}},
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
  score_chemical_cla_kingdom = 0.2,
  score_chemical_cla_superclass = 0.4,
  score_chemical_cla_class = 0.6,
  score_chemical_cla_parent = 0.8,
  score_chemical_npc_pathway = 0.25,
  score_chemical_npc_superclass = 0.5,
  score_chemical_npc_class = 0.75,
  max_per_score = 7L,
  xrefs_table = NULL
) {
  # Initialize logging context
  ctx <- log_operation(
    "clean_chemo",
    n_annotations = nrow(annot_table_wei_chemo),
    candidates_final = candidates_final,
    high_confidence = high_confidence
  )

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
    log_complete(ctx, n_final = 0, note = "Empty annotation table")
    return(annot_table_wei_chemo)
  }

  # Validate features and components schema minimally
  purrr::walk(
    .x = list(features_table, components_table),
    .f = .validate_features_dataframe
  )
  if (!is.data.frame(structure_organism_pairs_table)) {
    tima_abort(
      problem = "structure_organism_pairs_table must be a data frame",
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Ensure Score Columns are Numeric ----

  annot_table_wei_chemo <- coerce_score_columns(annot_table_wei_chemo)

  # Precompute feature-level consensus metadata once to avoid repeated
  # full-table coercion inside summarize_results for each output tier.
  feature_consensus_table <- .build_feature_consensus_table(
    annot_table_wei_chemo = annot_table_wei_chemo,
    model = columns_model()
  )

  # Core Filtering Pipeline ----
  log_metadata(
    ctx,
    phase = "filtering",
    best_percentile = best_percentile,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    max_per_score = max_per_score
  )

  candidate_tables <- prepare_ranked_candidates(
    annot_table_wei_chemo = annot_table_wei_chemo,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    minimal_ms1_condition = minimal_ms1_condition,
    best_percentile = best_percentile,
    max_per_score = max_per_score
  )

  df_ranked <- candidate_tables$df_ranked
  df_percentile <- candidate_tables$df_percentile
  results_candidates <- candidate_tables$results_candidates
  annotation_notes_lookup <- candidate_tables$annotation_notes_lookup
  organism_lookup <- .build_organism_lookup(
    structure_organism_pairs_table = structure_organism_pairs_table,
    df = df_ranked
  )

  if (candidate_tables$n_sampled_features > 0L) {
    log_info(
      "Sampling candidates for %d features with more than %d candidates per score",
      candidate_tables$n_sampled_features,
      max_per_score
    )
  }

  # Three-Tier Output Generation ----

  # Apply High-Confidence Filter for Filtered and Full Tiers ----

  # Apply filter_high_confidence_only for filtered tier
  df_filtered <- df_percentile |>
    filter_high_confidence_only(context = "filtered") |>
    tidytable::filter(rank_final <= candidates_final)

  if (!is.null(xrefs_table) && nrow(xrefs_table) > 0L) {
    df_filtered <- .add_xrefs_to_df(df_filtered, xrefs_table)
  }

  results_filtered <- df_filtered |>
    summarize_results(
      features_table = features_table,
      components_table = components_table,
      structure_organism_pairs_table = structure_organism_pairs_table,
      annot_table_wei_chemo = annot_table_wei_chemo,
      remove_ties = remove_ties,
      summarize = summarize,
      annotation_notes_lookup = annotation_notes_lookup,
      feature_consensus_table = feature_consensus_table,
      organism_lookup = organism_lookup
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

  df_classes_mini <- build_mini_taxonomy_table(
    df_percentile = df_percentile,
    score_chemical_cla_kingdom = score_chemical_cla_kingdom,
    score_chemical_cla_superclass = score_chemical_cla_superclass,
    score_chemical_cla_class = score_chemical_cla_class,
    score_chemical_cla_parent = score_chemical_cla_parent,
    score_chemical_npc_pathway = score_chemical_npc_pathway,
    score_chemical_npc_superclass = score_chemical_npc_superclass,
    score_chemical_npc_class = score_chemical_npc_class
  )

  # Combine mini results and adjust candidates_best based on inchikey presence
  results_mini <- build_mini_results_table(
    features_table = features_table,
    df_classes_mini = df_classes_mini,
    results_filtered = results_filtered,
    df_filtered = df_filtered,
    xrefs_table = xrefs_table
  )

  rm(df_classes_mini, df_filtered, df_percentile)
  invisible(gc(verbose = FALSE))

  # Tier 3: FULL - Optionally apply high-confidence filter
  df_full <- if (high_confidence) {
    df_ranked |>
      filter_high_confidence_only(context = "full")
  } else {
    df_ranked
  }

  if (!is.null(xrefs_table) && nrow(xrefs_table) > 0L) {
    df_full <- .add_xrefs_to_df(df_full, xrefs_table)
  }

  results_full <- df_full |>
    summarize_results(
      features_table = features_table,
      components_table = components_table,
      structure_organism_pairs_table = structure_organism_pairs_table,
      annot_table_wei_chemo = annot_table_wei_chemo,
      remove_ties = remove_ties,
      summarize = summarize,
      annotation_notes_lookup = annotation_notes_lookup,
      feature_consensus_table = feature_consensus_table,
      organism_lookup = organism_lookup
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

  rm(df_full, df_ranked)
  invisible(gc(verbose = FALSE))

  # Optionally Remove Compound Names (After All Processing) ----

  # Build results list
  results_list <- list(
    full = results_full,
    filtered = results_filtered,
    mini = results_mini
  )

  # Remove compound names from outputs if requested
  results_list <- remove_compound_names(results_list, compounds_names)

  log_complete(
    ctx,
    n_final_full = nrow(results_full),
    n_final_filtered = nrow(results_filtered),
    n_final_mini = nrow(results_mini),
    n_features = tidytable::n_distinct(results_filtered$feature_id)
  )

  # Clean up intermediate objects
  rm(
    annot_table_wei_chemo,
    results_candidates,
    features_table,
    components_table,
    structure_organism_pairs_table,
    candidate_tables,
    feature_consensus_table,
    organism_lookup
  )

  results_list
}
