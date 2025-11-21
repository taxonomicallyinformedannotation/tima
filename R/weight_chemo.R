#' @title Weight chemo
#'
#' @description This function weights biologically weighted annotations according
#'     to their chemical consistency by comparing chemical taxonomy (ClassyFire,
#'     NPClassifier) across molecular network neighbors. Higher chemical
#'     consistency within network components results in higher chemical scores.
#'
#' @param annot_table_wei_bio_clean Data frame with cleaned biologically weighted annotations
#' @param weight_spectral Numeric weight for spectral similarity score (0-1)
#' @param weight_biological Numeric weight for biological source score (0-1)
#' @param weight_chemical Numeric weight for chemical consistency score (0-1)
#' @param score_chemical_cla_kingdom Numeric score for ClassyFire kingdom match
#' @param score_chemical_cla_superclass Numeric score for ClassyFire superclass match
#' @param score_chemical_cla_class Numeric score for ClassyFire class match
#' @param score_chemical_cla_parent Numeric score for ClassyFire parent match (highest)
#' @param score_chemical_npc_pathway Numeric score for NPC pathway match
#' @param score_chemical_npc_superclass Numeric score for NPC superclass match
#' @param score_chemical_npc_class Numeric score for NPC class match (highest)
#'
#' @return Data frame containing chemically weighted annotations with
#'     chemical consistency scores and final weighted scores
#'
#' @examples NULL
weight_chemo <- function(
  annot_table_wei_bio_clean = get(
    "annot_table_wei_bio_clean",
    envir = parent.frame()
  ),
  weight_spectral = get("weight_spectral", envir = parent.frame()),
  weight_biological = get("weight_biological", envir = parent.frame()),
  weight_chemical = get("weight_chemical", envir = parent.frame()),
  score_chemical_cla_kingdom = get(
    "score_chemical_cla_kingdom",
    envir = parent.frame()
  ),
  score_chemical_cla_superclass = get(
    "score_chemical_cla_superclass",
    envir = parent.frame()
  ),
  score_chemical_cla_class = get(
    "score_chemical_cla_class",
    envir = parent.frame()
  ),
  score_chemical_cla_parent = get(
    "score_chemical_cla_parent",
    envir = parent.frame()
  ),
  score_chemical_npc_pathway = get(
    "score_chemical_npc_pathway",
    envir = parent.frame()
  ),
  score_chemical_npc_superclass = get(
    "score_chemical_npc_superclass",
    envir = parent.frame()
  ),
  score_chemical_npc_class = get(
    "score_chemical_npc_class",
    envir = parent.frame()
  )
) {
  # Validate data frame
  if (!is.data.frame(annot_table_wei_bio_clean)) {
    stop("annot_table_wei_bio_clean must be a data frame")
  }

  n_annotations <- nrow(annot_table_wei_bio_clean)
  if (n_annotations == 0L) {
    logger::log_warn(
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

  logger::log_info(
    "Weighting {n_annotations} annotations by chemical consistency"
  )
  logger::log_debug(
    "Weights - Spectral: ",
    weight_spectral,
    ", Biological: ",
    weight_biological,
    ", Chemical: ",
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
      score_weighted_chemo = (1 /
        (weight_chemical + weight_biological + weight_spectral)) *
        weight_chemical *
        score_chemical +
        (1 / (weight_chemical + weight_biological + weight_spectral)) *
          weight_biological *
          score_biological +
        (1 / (weight_chemical + weight_biological + weight_spectral)) *
          weight_spectral *
          candidate_score_pseudo_initial
    )

  rm(annot_table_wei_chemo_interim)

  return(annot_table_wei_chemo)
}
