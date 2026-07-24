## Helper Functions ----

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
#' @param high_evidence Logical
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
  high_evidence,
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
    high_evidence = high_evidence,
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
#'     When a feature has at least one MS2-annotated candidate, all MS1-only
#'     candidates for that feature are dropped: MS2 evidence supersedes MS1.
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
  has_csi_col <- "candidate_score_sirius_csi" %in%
    names(annot_table_wei_chemo)

  if (has_confidence_col && has_csi_col) {
    has_ms2 <- quote(
      !is.na(candidate_score_similarity) |
        !is.na(candidate_score_sirius_csi) |
        (!is.na(candidate_score_sirius_confidence) &
          as.numeric(candidate_score_sirius_confidence) > 0)
    )
  } else if (has_confidence_col && !has_csi_col) {
    has_ms2 <- quote(
      !is.na(candidate_score_similarity) |
        (!is.na(candidate_score_sirius_confidence) &
          as.numeric(candidate_score_sirius_confidence) > 0)
    )
  } else if (!has_confidence_col && has_csi_col) {
    has_ms2 <- quote(
      !is.na(candidate_score_similarity) | !is.na(candidate_score_sirius_csi)
    )
  } else {
    has_ms2 <- quote(
      !is.na(candidate_score_similarity)
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

  # Apply per-row pre-filter (keep MS2 rows OR MS1 rows that meet
  # score thresholds). This removes low-quality MS1 candidates that are not
  # backed by any biological/chemical evidence.
  result <- annot_table_wei_chemo |>
    tidytable::filter(!!has_ms2 | !!ms1_condition)

  result
}

rank_and_deduplicate <- function(df) {
  # === Ranking Pipeline ===
  # This function assigns rank_initial and rank_final based on different evidence.
  # 
  # rank_initial: Based on spectral score only (candidate_score_pseudo_initial)
  #   - For MS1-only filtering (MS1-only candidates must meet minimum thresholds)
  #   - Used to separate MS1-only vs MS2 evidence
  #
  # rank_final: Based on combined score (score_final = bio+chem+spec)
  #   - PRIMARY ranking used for candidate selection
  #   - Combines all evidence into ONE clear score
  #   - Mathematically sound: highest combined score = best candidate

  # === Coerce all score columns to numeric ===
  score_cols <- c(
    "score_final",
    "score_final_coverage",
    "candidate_score_pseudo_initial",
    "candidate_score_similarity",
    "candidate_score_similarity_forward",
    "candidate_score_similarity_reverse",
    "score_biological",
    "score_chemical"
  )

  for (col in score_cols) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    } else {
      df[[col]] <- rep(NA_real_, nrow(df))
    }
  }

  # Ensure match mode is character
  if (!("candidate_adduct_match_mode" %in% names(df))) {
    df[["candidate_adduct_match_mode"]] <- NA_character_
  }

  df <- tidytable::as_tidytable(df)

  # === Compute rank_initial (spectral score ranking) ===
  # For MS1-only filtering: ranks by spectral evidence only
  # dense_rank: ties get same rank, ranks are consecutive (1, 1, 2, 3, 3, 4, ...)
  df_initial <- df |>
    tidytable::group_by(feature_id) |>
    tidytable::mutate(
      rank_initial = tidytable::dense_rank(-candidate_score_pseudo_initial)
    ) |>
    tidytable::ungroup() |>
    tidytable::select(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      rank_initial
    )

  # === Compute rank_final (combined evidence ranking) ===
  # PRIMARY ranking using score_final (which combines bio+chem+spectral)
  # This is what determines which candidates are "best"
  df_ranked_final <- rank_candidates(as.data.frame(df))

  # Extract rank_final for joining
  df_final <- tidytable::as_tidytable(df_ranked_final) |>
    tidytable::select(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      rank_final
    )

  # === Join both ranks back to original data ===
  df_result <- df |>
    tidytable::left_join(
      df_initial,
      by = c("feature_id", "candidate_structure_inchikey_connectivity_layer")
    ) |>
    tidytable::left_join(
      df_final,
      by = c("feature_id", "candidate_structure_inchikey_connectivity_layer")
    )

  df_result
}

#' Apply Percentile Filter
#'
#' @description Internal helper to filter candidates by percentile threshold
#'     within each feature while preserving the top-ranked candidate and any
#'     cluster-consensus-promoted rows that remain the best coherent option.
#'
#' @param df Data frame with ranked annotations
#' @param best_percentile Numeric percentile threshold (0-1)
#'
#' @return Filtered data frame
#' @keywords internal
apply_percentile_filter <- function(df, best_percentile) {
  has_rank_col <- "rank_final" %in% names(df)
  has_consensus_col <- "cluster_consensus_promoted_from_anchor" %in% names(df)

  df |>
    tidytable::mutate(
      score_weighted_chemo = as.numeric(score_weighted_chemo)
    ) |>
    tidytable::group_by(feature_id) |>
    tidytable::mutate(
      .score_max = suppressWarnings(max(score_weighted_chemo, na.rm = TRUE)),
      .keep_by_percentile = score_weighted_chemo >=
        best_percentile * .score_max,
      .keep_by_rank = if (has_rank_col) {
        rank_final == 1L
      } else {
        FALSE
      },
      .keep_by_consensus = if (has_consensus_col) {
        cluster_consensus_promoted_from_anchor %in% TRUE
      } else {
        FALSE
      }
    ) |>
    tidytable::filter(
      .keep_by_percentile | .keep_by_rank | .keep_by_consensus
    ) |>
    tidytable::ungroup() |>
    tidytable::select(
      -tidyselect::any_of(c(
        ".score_max",
        ".keep_by_percentile",
        ".keep_by_rank",
        ".keep_by_consensus"
      ))
    )
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
  # Normalize weight inputs: ensure numeric scalar defaults to 0 when NULL or non-numeric
  w_cla_kin <- if (!is.null(weights$w_cla_kin)) {
    as.numeric(weights$w_cla_kin)
  } else {
    0
  }
  w_cla_sup <- if (!is.null(weights$w_cla_sup)) {
    as.numeric(weights$w_cla_sup)
  } else {
    0
  }
  w_cla_cla <- if (!is.null(weights$w_cla_cla)) {
    as.numeric(weights$w_cla_cla)
  } else {
    0
  }
  w_cla_par <- if (!is.null(weights$w_cla_par)) {
    as.numeric(weights$w_cla_par)
  } else {
    0
  }

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
        as.numeric(feature_pred_tax_cla_01kin_score) * w_cla_kin,
        NA_real_
      ),
      ws_sup = tidytable::if_else(
        cla_sup_valid,
        as.numeric(feature_pred_tax_cla_02sup_score) * w_cla_sup,
        NA_real_
      ),
      ws_cla = tidytable::if_else(
        cla_cla_valid,
        as.numeric(feature_pred_tax_cla_03cla_score) * w_cla_cla,
        NA_real_
      ),
      ws_par = tidytable::if_else(
        cla_par_valid,
        as.numeric(feature_pred_tax_cla_04dirpar_score) * w_cla_par,
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
  # Normalize weight inputs: ensure numeric scalar defaults to 0 when NULL or non-numeric
  w_npc_pat <- if (!is.null(weights$w_npc_pat)) {
    as.numeric(weights$w_npc_pat)
  } else {
    0
  }
  w_npc_sup <- if (!is.null(weights$w_npc_sup)) {
    as.numeric(weights$w_npc_sup)
  } else {
    0
  }
  w_npc_cla <- if (!is.null(weights$w_npc_cla)) {
    as.numeric(weights$w_npc_cla)
  } else {
    0
  }

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
        as.numeric(feature_pred_tax_npc_01pat_score) * w_npc_pat,
        NA_real_
      ),
      ws_sup = tidytable::if_else(
        npc_sup_valid,
        as.numeric(feature_pred_tax_npc_02sup_score) * w_npc_sup,
        NA_real_
      ),
      ws_cla = tidytable::if_else(
        npc_cla_valid,
        as.numeric(feature_pred_tax_npc_03cla_score) * w_npc_cla,
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

#' Compute neutral mass (M) per candidate row
#'
#' @description Vectorised, silent wrapper around
#'     \code{\link{calculate_mass_of_m}} used to build a cross-adduct
#'     propagation key. Returns \code{NA_real_} for any row whose adduct or
#'     m/z cannot be parsed / converted, so callers can safely \code{round()}
#'     the result.
#'
#' @param mz Numeric vector of feature m/z values
#' @param adduct_string Character vector of adduct strings, same length as mz
#'
#' @return Numeric vector of neutral masses, NA where conversion failed
#' @keywords internal
compute_candidate_M <- function(mz, adduct_string) {
  n <- length(mz)
  if (n == 0L) {
    return(numeric(0))
  }
  mz_num <- as.numeric(mz)
  out <- rep(NA_real_, n)

  usable <- !is.na(mz_num) &
    is.finite(mz_num) &
    mz_num > 0 &
    !is.na(adduct_string) &
    nzchar(adduct_string)
  if (!any(usable)) {
    return(out)
  }

  # Group by unique adduct string: parse once per unique string, then apply
  # the neutral-mass formula as vectorised arithmetic. This avoids O(N)
  # calls into the validator-heavy `calculate_mass_of_m` and scales with
  # the (typically small) number of distinct adducts rather than rows.
  # Was: looped over unique adducts and rescanned all rows with
  # which(usable & adduct_string == add) on each iteration.
  # Now: split usable row indices by adduct once, then reuse each index group.
  usable_idx <- which(usable)
  adduct_groups <- split(usable_idx, adduct_string[usable_idx])
  for (add in names(adduct_groups)) {
    parsed <- tryCatch(
      suppressWarnings(parse_adduct(add)),
      error = function(.err) {
        invisible(.err)
        NULL
      }
    )
    if (is.null(parsed) || all(parsed == 0)) {
      next
    }
    n_charges <- parsed[["n_charges"]]
    n_mer <- parsed[["n_mer"]]
    n_iso <- parsed[["n_iso"]]
    mass_mods <- parsed[["los_add_clu"]]
    charge_sign <- parsed[["charge"]]
    if (n_mer == 0L || n_charges == 0L) {
      next
    }

    idx <- adduct_groups[[add]]
    if (!length(idx)) {
      next
    }
    mz_vec <- mz_num[idx]
    iso_shift <- n_iso * ISOTOPE_MASS_SHIFT_DALTONS
    z_signed <- n_charges * charge_sign
    m_vec <- (n_charges *
      (mz_vec - iso_shift) -
      mass_mods +
      z_signed * ELECTRON_MASS_DALTONS) /
      n_mer
    m_vec[!is.finite(m_vec) | m_vec == 0] <- NA_real_
    valid <- !is.na(m_vec)
    out[idx[valid]] <- m_vec[valid]
  }

  out
}
