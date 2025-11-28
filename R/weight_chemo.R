#' @title Weight annotations by chemical consistency
#'
#' @description Weights biologically weighted annotations according to their
#'     chemical consistency by comparing chemical taxonomy (ClassyFire,
#'     NPClassifier) across molecular network neighbors. Higher chemical
#'     consistency within network components results in higher chemical scores.
#'     Internal helper for weight_annotations().
#'
#' @details The weights are automatically normalized by dividing by their sum,
#'     so they do NOT need to sum to 1. For example, weights of (1, 1, 1)
#'     produce the same result as (0.33, 0.33, 0.33).
#'
#' @include validations_utils.R
#' @include weights_utils.R
#'
#' @param annot_table_wei_bio_clean Data frame with cleaned biologically weighted annotations
#' @param weight_spectral Weight for spectral similarity score (any positive number)
#' @param weight_biological Weight for biological source score (any positive number)
#' @param weight_chemical Weight for chemical consistency score (any positive number)
#' @param score_chemical_cla_kingdom Score for ClassyFire kingdom match (0-1)
#' @param score_chemical_cla_superclass Score for ClassyFire superclass match (0-1)
#' @param score_chemical_cla_class Score for ClassyFire class match (0-1)
#' @param score_chemical_cla_parent Score for ClassyFire parent match (0-1, highest)
#' @param score_chemical_npc_pathway Score for NPC pathway match (0-1)
#' @param score_chemical_npc_superclass Score for NPC superclass match (0-1)
#' @param score_chemical_npc_class Score for NPC class match (0-1, highest)
#'
#' @return Data frame with chemically weighted annotations including chemical
#'     consistency scores and final weighted scores
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal use only - called by weight_annotations()
#' weighted <- weight_chemo(
#'   annot_table_wei_bio_clean = bio_weighted,
#'   weight_spectral = 1,
#'   weight_biological = 1,
#'   weight_chemical = 1,
#'   score_chemical_cla_kingdom = 0.1,
#'   score_chemical_cla_superclass = 0.2,
#'   score_chemical_cla_class = 0.3,
#'   score_chemical_cla_parent = 0.4,
#'   score_chemical_npc_pathway = 0.1,
#'   score_chemical_npc_superclass = 0.2,
#'   score_chemical_npc_class = 0.3
#' )
#' }
weight_chemo <- function(
  annot_table_wei_bio_clean,
  weight_spectral,
  weight_biological,
  weight_chemical,
  score_chemical_cla_kingdom,
  score_chemical_cla_superclass,
  score_chemical_cla_class,
  score_chemical_cla_parent,
  score_chemical_npc_pathway,
  score_chemical_npc_superclass,
  score_chemical_npc_class
) {
  # Input Validation ----
  validate_dataframe(
    annot_table_wei_bio_clean,
    param_name = "annot_table_wei_bio_clean"
  )

  # Validate weights
  validate_weights(
    c(
      weight_spectral,
      weight_biological,
      weight_chemical
    )
  )

  # Validate all chemical score parameters (0-1 range)
  validate_numeric_range(
    score_chemical_cla_kingdom,
    0,
    1,
    param_name = "score_chemical_cla_kingdom"
  )
  validate_numeric_range(
    score_chemical_cla_superclass,
    0,
    1,
    param_name = "score_chemical_cla_superclass"
  )
  validate_numeric_range(
    score_chemical_cla_class,
    0,
    1,
    param_name = "score_chemical_cla_class"
  )
  validate_numeric_range(
    score_chemical_cla_parent,
    0,
    1,
    param_name = "score_chemical_cla_parent"
  )
  validate_numeric_range(
    score_chemical_npc_pathway,
    0,
    1,
    param_name = "score_chemical_npc_pathway"
  )
  validate_numeric_range(
    score_chemical_npc_superclass,
    0,
    1,
    param_name = "score_chemical_npc_superclass"
  )
  validate_numeric_range(
    score_chemical_npc_class,
    0,
    1,
    param_name = "score_chemical_npc_class"
  )

  # Early Exit ----
  n_annotations <- nrow(annot_table_wei_bio_clean)
  if (n_annotations == 0L) {
    log_warn(
      "Empty annotation table provided, skipping chemical weighting"
    )
    return(annot_table_wei_bio_clean)
  }

  # Validate weights sum to 1 (with tolerance for floating-point precision)
  weight_sum <- weight_spectral + weight_biological + weight_chemical
  if (abs(weight_sum - 1.0) > 0.01) {
    stop("Weights must sum to 1.0, got: ", round(weight_sum, 4))
  }

  # Validate all weights are non-negative
  if (weight_spectral < 0 || weight_biological < 0 || weight_chemical < 0) {
    stop("All weights must be non-negative")
  }

  # Validate chemical scores
  chem_scores <- list(
    cla_kingdom = score_chemical_cla_kingdom,
    cla_superclass = score_chemical_cla_superclass,
    cla_class = score_chemical_cla_class,
    cla_parent = score_chemical_cla_parent,
    npc_pathway = score_chemical_npc_pathway,
    npc_superclass = score_chemical_npc_superclass,
    npc_class = score_chemical_npc_class
  )

  for (level in names(chem_scores)) {
    score <- chem_scores[[level]]
    if (!is.numeric(score) || score < 0 || score > 1) {
      stop("score_chemical_", level, " must be between 0 and 1, got: ", score)
    }
  }

  log_info(
    "Weighting %d annotations by chemical consistency",
    n_annotations
  )
  log_debug(
    "Weights - Spectral: %f, Biological: %f, Chemical: %f",
    weight_spectral,
    weight_biological,
    weight_chemical
  )

  # Reduce to unique candidate/feature taxonomy combinations to score per-level
  df2 <- annot_table_wei_bio_clean |>
    tidytable::distinct(
      candidate_structure_tax_cla_01kin,
      candidate_structure_tax_npc_01pat,
      candidate_structure_tax_cla_02sup,
      candidate_structure_tax_npc_02sup,
      candidate_structure_tax_cla_03cla,
      candidate_structure_tax_npc_03cla,
      candidate_structure_tax_cla_04dirpar,
      feature_pred_tax_cla_01kin_val,
      feature_pred_tax_cla_01kin_score,
      feature_pred_tax_npc_01pat_val,
      feature_pred_tax_npc_01pat_score,
      feature_pred_tax_cla_02sup_val,
      feature_pred_tax_cla_02sup_score,
      feature_pred_tax_npc_02sup_val,
      feature_pred_tax_npc_02sup_score,
      feature_pred_tax_cla_03cla_val,
      feature_pred_tax_cla_03cla_score,
      feature_pred_tax_npc_03cla_val,
      feature_pred_tax_npc_03cla_score,
      feature_pred_tax_cla_04dirpar_val,
      feature_pred_tax_cla_04dirpar_score
    )

  # Helper: compute per-level chemical score when candidate taxonomy matches
  score_per_level_chemo <- function(
    df,
    candidates,
    features_val,
    features_score,
    level_weight,
    score_name
  ) {
    cand_sym <- rlang::sym(x = candidates)
    fval_sym <- rlang::sym(x = features_val)
    fsc_sym <- rlang::sym(x = features_score)

    df |>
      tidytable::distinct(!!cand_sym, !!fval_sym, !!fsc_sym) |>
      tidytable::filter(
        !is.na(!!cand_sym),
        !is.na(!!fval_sym)
      ) |>
      # candidate label appears in feature predicted label path
      tidytable::filter(stringi::stri_detect_regex(
        str = !!fval_sym,
        pattern = !!cand_sym
      )) |>
      # assign the configured per-level weight when matched
      tidytable::mutate(
        !!rlang::sym(x = score_name) := as.numeric(level_weight)
      ) |>
      tidytable::select(!!cand_sym, !!rlang::sym(x = score_name)) |>
      tidytable::distinct()
  }

  # Compute per-level matches for ClassyFire and NPClassifier
  step_cla_kin <- df2 |>
    score_per_level_chemo(
      candidates = "candidate_structure_tax_cla_01kin",
      features_val = "feature_pred_tax_cla_01kin_val",
      features_score = "feature_pred_tax_cla_01kin_score",
      level_weight = score_chemical_cla_kingdom,
      score_name = "score_chemical_1"
    )
  step_npc_pat <- df2 |>
    score_per_level_chemo(
      candidates = "candidate_structure_tax_npc_01pat",
      features_val = "feature_pred_tax_npc_01pat_val",
      features_score = "feature_pred_tax_npc_01pat_score",
      level_weight = score_chemical_npc_pathway,
      score_name = "score_chemical_2"
    )
  step_cla_sup <- df2 |>
    score_per_level_chemo(
      candidates = "candidate_structure_tax_cla_02sup",
      features_val = "feature_pred_tax_cla_02sup_val",
      features_score = "feature_pred_tax_cla_02sup_score",
      level_weight = score_chemical_cla_superclass,
      score_name = "score_chemical_3"
    )
  step_npc_sup <- df2 |>
    score_per_level_chemo(
      candidates = "candidate_structure_tax_npc_02sup",
      features_val = "feature_pred_tax_npc_02sup_val",
      features_score = "feature_pred_tax_npc_02sup_score",
      level_weight = score_chemical_npc_superclass,
      score_name = "score_chemical_4"
    )
  step_cla_cla <- df2 |>
    score_per_level_chemo(
      candidates = "candidate_structure_tax_cla_03cla",
      features_val = "feature_pred_tax_cla_03cla_val",
      features_score = "feature_pred_tax_cla_03cla_score",
      level_weight = score_chemical_cla_class,
      score_name = "score_chemical_5"
    )
  step_npc_cla <- df2 |>
    score_per_level_chemo(
      candidates = "candidate_structure_tax_npc_03cla",
      features_val = "feature_pred_tax_npc_03cla_val",
      features_score = "feature_pred_tax_npc_03cla_score",
      level_weight = score_chemical_npc_class,
      score_name = "score_chemical_6"
    )
  step_cla_par <- df2 |>
    score_per_level_chemo(
      candidates = "candidate_structure_tax_cla_04dirpar",
      features_val = "feature_pred_tax_cla_04dirpar_val",
      features_score = "feature_pred_tax_cla_04dirpar_score",
      level_weight = score_chemical_cla_parent,
      score_name = "score_chemical_7"
    )

  # Merge scores and keep the best chemical score across levels
  supp_tables <- list(
    step_cla_kin,
    step_npc_pat,
    step_cla_sup,
    step_npc_sup,
    step_cla_cla,
    step_npc_cla,
    step_cla_par
  )
  rm(
    step_cla_kin,
    step_npc_pat,
    step_cla_sup,
    step_npc_sup,
    step_cla_cla,
    step_npc_cla,
    step_cla_par
  )

  annot_table_wei_chemo_init <- purrr::reduce(
    .x = supp_tables,
    .init = df2,
    .f = tidytable::left_join
  ) |>
    tidytable::mutate(
      score_chemical = pmax(
        score_chemical_1,
        score_chemical_2,
        score_chemical_3,
        score_chemical_4,
        score_chemical_5,
        score_chemical_6,
        score_chemical_7,
        0,
        na.rm = TRUE
      )
    ) |>
    tidytable::select(-tidyselect::contains(match = "score_chemical_"))
  rm(df2, supp_tables)

  annot_table_wei_chemo_interim <- annot_table_wei_chemo_init |>
    tidytable::right_join(y = annot_table_wei_bio_clean)
  rm(annot_table_wei_chemo_init, annot_table_wei_bio_clean)

  annot_table_wei_chemo <- annot_table_wei_chemo_interim |>
    tidytable::mutate(
      score_weighted_chemo = compute_weighted_sum(
        score_chemical,
        score_biological,
        candidate_score_pseudo_initial,
        weights = c(weight_chemical, weight_biological, weight_spectral)
      )
    )

  rm(annot_table_wei_chemo_interim)

  return(annot_table_wei_chemo)
}
