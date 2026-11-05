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
#' @include errors_utils.R
#' @include parse_adduct.R
#' @include rank_candidates.R
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
  annot_table_wei_chemo <- tidytable::as_tidytable(annot_table_wei_chemo)
  n <- nrow(annot_table_wei_chemo)
  if (n == 0L) {
    return(annot_table_wei_chemo)
  }

  sb <- as.numeric(annot_table_wei_chemo$score_biological)
  sc <- as.numeric(annot_table_wei_chemo$score_chemical)
  data.table::set(annot_table_wei_chemo, j = "score_biological", value = sb)
  data.table::set(annot_table_wei_chemo, j = "score_chemical", value = sc)

  has_ms2 <- if (
    "candidate_score_similarity" %in% names(annot_table_wei_chemo)
  ) {
    sim <- suppressWarnings(
      .as_numeric_dedup(annot_table_wei_chemo$candidate_score_similarity)
    )
    !is.na(sim) & sim > 0
  } else {
    rep.int(FALSE, n)
  }
  if ("candidate_score_sirius_csi" %in% names(annot_table_wei_chemo)) {
    has_ms2 <- has_ms2 |
      !is.na(annot_table_wei_chemo$candidate_score_sirius_csi)
  }
  if ("candidate_score_sirius_confidence" %in% names(annot_table_wei_chemo)) {
    conf <- suppressWarnings(
      as.numeric(annot_table_wei_chemo$candidate_score_sirius_confidence)
    )
    has_ms2 <- has_ms2 | (!is.na(conf) & conf > 0)
  }

  bio_ok <- !is.na(sb) & (sb >= minimal_ms1_bio)
  chemo_ok <- !is.na(sc) & (sc >= minimal_ms1_chemo)
  ms1_ok <- if (minimal_ms1_condition == "OR") {
    bio_ok | chemo_ok
  } else {
    bio_ok & chemo_ok
  }

  keep <- has_ms2 | ms1_ok
  if (all(keep)) {
    return(annot_table_wei_chemo)
  }
  annot_table_wei_chemo[keep, , drop = FALSE]
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

  # Handle empty input
  if (nrow(df) == 0L) {
    df <- tidytable::as_tidytable(df)
    if (!("rank_initial" %in% names(df))) {
      df$rank_initial <- integer()
    }
    if (!("rank_final" %in% names(df))) {
      df$rank_final <- integer()
    }
    return(df)
  }

  # === Coerce all score columns to numeric ===
  score_cols <- c(
    "score_final",
    "candidate_score_pseudo_initial",
    "candidate_score_similarity",
    "candidate_score_similarity_forward",
    "candidate_score_similarity_reverse",
    "score_biological",
    "score_chemical"
  )

  for (col in score_cols) {
    if (col %in% names(df)) {
      df[[col]] <- .as_numeric_dedup(df[[col]])
    } else {
      df[[col]] <- rep(NA_real_, nrow(df))
    }
  }

  # Ensure match mode is character
  if (!("candidate_adduct_match_mode" %in% names(df))) {
    df[["candidate_adduct_match_mode"]] <- rep(NA_character_, nrow(df))
  }

  df <- tidytable::as_tidytable(df)

  # Compute rank_final (min_rank on combined score) and deduplicate per
  # (feature_id, candidate_structure_inchikey_connectivity_layer), keeping each
  # structure's best-scoring row in original row order.
  df_result <- rank_candidates(df)

  # Compute rank_initial (dense_rank on spectral score) per feature_id
  fids <- df_result$feature_id
  u_fids <- unique(fids)
  fid_id <- fastmatch::fmatch(fids, u_fids)
  si <- suppressWarnings(as.numeric(df_result$candidate_score_pseudo_initial))
  n_res <- nrow(df_result)

  valid <- !is.na(si)
  rk_init <- rep.int(NA_integer_, n_res)
  if (any(valid)) {
    v_idx <- which(valid)
    g <- fid_id[v_idx]
    v <- -si[v_idx]
    ord <- order(g, v, method = "radix")
    g_o <- g[ord]
    v_o <- v[ord]
    m <- length(ord)
    if (m == 1L) {
      rk_init[v_idx[ord]] <- 1L
    } else {
      new_g <- c(TRUE, g_o[-1L] != g_o[-m])
      new_v <- new_g | c(TRUE, v_o[-1L] != v_o[-m])
      cs <- cumsum(as.integer(new_v))
      g_start_cs <- cummax(ifelse(new_g, cs, 1L))
      rk_init[v_idx[ord]] <- cs - g_start_cs + 1L
    }
  }
  data.table::set(df_result, j = "rank_initial", value = rk_init)

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
  if (nrow(df) == 0L) {
    return(df)
  }
  df <- tidytable::as_tidytable(df)
  swc <- as.numeric(df$score_weighted_chemo)
  data.table::set(df, j = "score_weighted_chemo", value = swc)

  fids <- df$feature_id
  u_fids <- unique(fids)
  fid_id <- fastmatch::fmatch(fids, u_fids)

  valid <- !is.na(swc)
  score_max <- rep.int(-Inf, length(u_fids))
  if (any(valid)) {
    v_idx <- which(valid)
    g <- fid_id[v_idx]
    v <- -swc[v_idx]
    ord <- order(g, v, method = "radix")
    first_in_g <- ord[!duplicated(g[ord])]
    score_max[fid_id[v_idx[first_in_g]]] <- swc[v_idx[first_in_g]]
  }

  row_max <- score_max[fid_id]
  keep <- (!is.na(swc) & swc >= best_percentile * row_max)
  if ("rank_final" %in% names(df)) {
    rf <- df$rank_final
    keep <- keep | (!is.na(rf) & rf == 1L)
  }
  if ("cluster_consensus_promoted_from_anchor" %in% names(df)) {
    cc <- df$cluster_consensus_promoted_from_anchor
    keep <- keep | (!is.na(cc) & cc)
  }

  df[keep, , drop = FALSE]
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
  fids_eval <- df_ranked$feature_id
  u_fids <- unique(fids_eval)
  if (length(u_fids) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      candidates_evaluated = integer(),
      candidates_best = integer()
    ))
  }
  n_eval <- tabulate(
    fastmatch::fmatch(fids_eval, u_fids),
    nbins = length(u_fids)
  )
  fids_best <- df_percentile$feature_id
  idx_best <- fastmatch::fmatch(fids_best, u_fids, nomatch = 0L)
  n_best <- tabulate(idx_best[idx_best > 0L], nbins = length(u_fids))
  n_best[n_best == 0L] <- NA_integer_

  tidytable::tidytable(
    feature_id = u_fids,
    candidates_evaluated = n_eval,
    candidates_best = n_best
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
  n <- nrow(df_pred_tax)
  if (n == 0L) {
    return(tidytable::tidytable(
      feature_id = character(0L),
      label_classyfire_predicted = character(0L),
      score_classyfire = numeric(0L)
    ))
  }

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

  v_kin <- df_pred_tax$feature_pred_tax_cla_01kin_val
  v_sup <- df_pred_tax$feature_pred_tax_cla_02sup_val
  v_cla <- df_pred_tax$feature_pred_tax_cla_03cla_val
  v_par <- df_pred_tax$feature_pred_tax_cla_04dirpar_val

  s_kin <- .as_numeric_dedup(df_pred_tax$feature_pred_tax_cla_01kin_score)
  s_sup <- .as_numeric_dedup(df_pred_tax$feature_pred_tax_cla_02sup_score)
  s_cla <- .as_numeric_dedup(df_pred_tax$feature_pred_tax_cla_03cla_score)
  s_par <- .as_numeric_dedup(df_pred_tax$feature_pred_tax_cla_04dirpar_score)

  ws_kin <- s_kin * w_cla_kin
  ws_kin[is.na(v_kin) | v_kin == "notClassified"] <- NA_real_
  ws_sup <- s_sup * w_cla_sup
  ws_sup[is.na(v_sup) | v_sup == "notClassified"] <- NA_real_
  ws_cla <- s_cla * w_cla_cla
  ws_cla[is.na(v_cla) | v_cla == "notClassified"] <- NA_real_
  ws_par <- s_par * w_cla_par
  ws_par[is.na(v_par) | v_par == "notClassified"] <- NA_real_

  max_ws <- pmax(ws_kin, ws_sup, ws_cla, ws_par, na.rm = TRUE)

  label <- rep(NA_character_, n)
  score <- rep(NA_real_, n)

  m_kin <- !is.na(ws_kin) & ws_kin == max_ws
  label[m_kin] <- v_kin[m_kin]
  score[m_kin] <- s_kin[m_kin]

  m_sup <- !is.na(ws_sup) & ws_sup == max_ws
  label[m_sup] <- v_sup[m_sup]
  score[m_sup] <- s_sup[m_sup]

  m_cla <- !is.na(ws_cla) & ws_cla == max_ws
  label[m_cla] <- v_cla[m_cla]
  score[m_cla] <- s_cla[m_cla]

  m_par <- !is.na(ws_par) & ws_par == max_ws
  label[m_par] <- v_par[m_par]
  score[m_par] <- s_par[m_par]

  keep <- !is.na(label) & label != "notClassified"
  tidytable::tidytable(
    feature_id = df_pred_tax$feature_id[keep],
    label_classyfire_predicted = label[keep],
    score_classyfire = score[keep]
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
  n <- nrow(df_pred_tax)
  if (n == 0L) {
    return(tidytable::tidytable(
      feature_id = character(0L),
      label_npclassifier_predicted = character(0L),
      score_npclassifier = numeric(0L)
    ))
  }

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

  v_pat <- df_pred_tax$feature_pred_tax_npc_01pat_val
  v_sup <- df_pred_tax$feature_pred_tax_npc_02sup_val
  v_cla <- df_pred_tax$feature_pred_tax_npc_03cla_val

  s_pat <- .as_numeric_dedup(df_pred_tax$feature_pred_tax_npc_01pat_score)
  s_sup <- .as_numeric_dedup(df_pred_tax$feature_pred_tax_npc_02sup_score)
  s_cla <- .as_numeric_dedup(df_pred_tax$feature_pred_tax_npc_03cla_score)

  ws_pat <- s_pat * w_npc_pat
  ws_pat[is.na(v_pat) | v_pat == "notClassified"] <- NA_real_
  ws_sup <- s_sup * w_npc_sup
  ws_sup[is.na(v_sup) | v_sup == "notClassified"] <- NA_real_
  ws_cla <- s_cla * w_npc_cla
  ws_cla[is.na(v_cla) | v_cla == "notClassified"] <- NA_real_

  max_ws <- pmax(ws_pat, ws_sup, ws_cla, na.rm = TRUE)

  label <- rep(NA_character_, n)
  score <- rep(NA_real_, n)

  m_pat <- !is.na(ws_pat) & ws_pat == max_ws
  label[m_pat] <- v_pat[m_pat]
  score[m_pat] <- s_pat[m_pat]

  m_sup <- !is.na(ws_sup) & ws_sup == max_ws
  label[m_sup] <- v_sup[m_sup]
  score[m_sup] <- s_sup[m_sup]

  m_cla <- !is.na(ws_cla) & ws_cla == max_ws
  label[m_cla] <- v_cla[m_cla]
  score[m_cla] <- s_cla[m_cla]

  keep <- !is.na(label) & label != "notClassified"
  tidytable::tidytable(
    feature_id = df_pred_tax$feature_id[keep],
    label_npclassifier_predicted = label[keep],
    score_npclassifier = score[keep]
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
  mz_num <- .as_numeric_dedup(mz)
  out <- rep(NA_real_, n)

  usable_idx <- which(
    !is.na(mz_num) &
      is.finite(mz_num) &
      mz_num > 0 &
      !is.na(adduct_string) &
      nzchar(adduct_string)
  )
  if (length(usable_idx) == 0L) {
    return(out)
  }

  add_sub <- adduct_string[usable_idx]
  u_add <- unique(add_sub)
  add_id <- fastmatch::fmatch(add_sub, u_add)

  n_u <- length(u_add)
  u_n_charges <- rep(NA_real_, n_u)
  u_n_mer <- rep(NA_real_, n_u)
  u_iso_shift <- rep(NA_real_, n_u)
  u_mass_mods <- rep(NA_real_, n_u)
  u_z_elec <- rep(NA_real_, n_u)

  for (ui in seq_len(n_u)) {
    parsed <- tryCatch(
      suppressWarnings(parse_adduct(u_add[[ui]])),
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
    if (n_mer == 0L || n_charges == 0L) {
      next
    }
    u_n_charges[[ui]] <- n_charges
    u_n_mer[[ui]] <- n_mer
    u_iso_shift[[ui]] <- parsed[["n_iso"]] * ISOTOPE_MASS_SHIFT_DALTONS
    u_mass_mods[[ui]] <- parsed[["los_add_clu"]]
    u_z_elec[[ui]] <- (n_charges * parsed[["charge"]]) * ELECTRON_MASS_DALTONS
  }

  nc <- u_n_charges[add_id]
  valid_add <- !is.na(nc)
  if (!any(valid_add)) {
    return(out)
  }

  v_idx <- usable_idx[valid_add]
  v_aid <- add_id[valid_add]
  m_vec <- (u_n_charges[v_aid] *
    (mz_num[v_idx] - u_iso_shift[v_aid]) -
    u_mass_mods[v_aid] +
    u_z_elec[v_aid]) /
    u_n_mer[v_aid]
  m_vec[!is.finite(m_vec) | m_vec == 0] <- NA_real_
  out[v_idx] <- m_vec
  out
}
