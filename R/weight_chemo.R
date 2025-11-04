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

  if (nrow(annot_table_wei_bio_clean) == 0L) {
    logger::log_warn("Empty annotation table provided")
    return(annot_table_wei_bio_clean)
  }

  # Validate weights sum to 1
  weight_sum <- weight_spectral + weight_biological + weight_chemical
  if (abs(weight_sum - 1.0) > 0.001) {
    stop("Weights must sum to 1.0, got: ", weight_sum)
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

  logger::log_info("Weighting annotations by chemical consistency")
  logger::log_debug(
    "Weights - Spectral: ",
    weight_spectral,
    ", Biological: ",
    weight_biological,
    ", Chemical: ",
    weight_chemical
  )
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

  logger::log_trace("Calculating chemical score at all levels ...")
  score_per_level_chemo <-
    function(
      df,
      candidates,
      features_val,
      features_score,
      score,
      score_name
    ) {
      score <- df |>
        tidytable::distinct(
          !!as.name(candidates),
          !!as.name(features_val),
          !!as.name(features_score)
        ) |>
        tidytable::filter(stringi::stri_detect_regex(
          pattern = !!as.name(candidates),
          str = !!as.name(features_val)
        )) |>
        tidytable::mutate(!!as.name(score_name) := !!as.name(score) * 1)
    }
  logger::log_trace("... (classyfire) kingdom")
  step_cla_kin <- df2 |>
    score_per_level_chemo(
      candidates = "candidate_structure_tax_cla_01kin",
      features_val = "feature_pred_tax_cla_01kin_val",
      features_score = "feature_pred_tax_cla_01kin_score",
      score = "score_chemical_cla_kingdom",
      score_name = "score_chemical_1"
    )
  logger::log_trace("... (NPC) pathway")
  step_npc_pat <- df2 |>
    score_per_level_chemo(
      candidates = "candidate_structure_tax_npc_01pat",
      features_val = "feature_pred_tax_npc_01pat_val",
      features_score = "feature_pred_tax_npc_01pat_score",
      score = "score_chemical_npc_pathway",
      score_name = "score_chemical_2"
    )
  logger::log_trace("... (classyfire) superclass")
  step_cla_sup <- df2 |>
    score_per_level_chemo(
      candidates = "candidate_structure_tax_cla_02sup",
      features_val = "feature_pred_tax_cla_02sup_val",
      features_score = "feature_pred_tax_cla_02sup_score",
      score = "score_chemical_cla_superclass",
      score_name = "score_chemical_3"
    )
  logger::log_trace("... (NPC) superclass")
  step_npc_sup <- df2 |>
    score_per_level_chemo(
      candidates = "candidate_structure_tax_npc_02sup",
      features_val = "feature_pred_tax_npc_02sup_val",
      features_score = "feature_pred_tax_npc_02sup_score",
      score = "score_chemical_npc_superclass",
      score_name = "score_chemical_4"
    )
  logger::log_trace("... (classyfire) class")
  step_cla_cla <- df2 |>
    score_per_level_chemo(
      candidates = "candidate_structure_tax_cla_03cla",
      features_val = "feature_pred_tax_cla_03cla_val",
      features_score = "feature_pred_tax_cla_03cla_score",
      score = "score_chemical_cla_class",
      score_name = "score_chemical_5"
    )
  logger::log_trace("... (NPC) class")
  step_npc_cla <- df2 |>
    score_per_level_chemo(
      candidates = "candidate_structure_tax_npc_03cla",
      features_val = "feature_pred_tax_npc_03cla_val",
      features_score = "feature_pred_tax_npc_03cla_score",
      score = "score_chemical_npc_class",
      score_name = "score_chemical_6"
    )
  logger::log_trace("... (classyfire) parent")
  step_cla_par <- df2 |>
    score_per_level_chemo(
      candidates = "candidate_structure_tax_cla_04dirpar",
      features_val = "feature_pred_tax_cla_04dirpar_val",
      features_score = "feature_pred_tax_cla_04dirpar_score",
      score = "score_chemical_cla_parent",
      score_name = "score_chemical_7"
    )

  logger::log_trace("... keeping best chemical score")
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
    .f = function(x, y) {
      tidytable::left_join(x, y)
    }
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
    tidytable::select(-tidyselect::contains("score_chemical_"))
  rm(df2, supp_tables)

  annot_table_wei_chemo_interim <- annot_table_wei_chemo_init |>
    tidytable::right_join(annot_table_wei_bio_clean)
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
