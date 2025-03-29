#' @title Weight chemo
#'
#' @description This function weights the biologically weighted annotations
#' according their chemical consistency
#'
#' @param annot_table_wei_bio_clean Table containing the biologically
#' weighted annotation
#' @param weight_spectral Weight for the spectral score
#' @param weight_biological Weight for the biological score
#' @param weight_chemical Weight for the chemical consistency score
#' @param score_chemical_cla_kingdom Score for a `Classyfire kingdom` match
#' (should be lower than ` Classyfire superclass`)
#' @param score_chemical_cla_superclass Score for a `Classyfire superclass`
#'  match
#'  (should be lower than `Classyfire class`)
#' @param score_chemical_cla_class Score for a `Classyfire class` match
#' (should be lower than `Classyfire parent`)
#' @param score_chemical_cla_parent Score for a `Classyfire parent` match
#'  (should be the highest)
#' @param score_chemical_npc_pathway Score for a `pathway` match
#' (should be lower than `superclass`)
#' @param score_chemical_npc_superclass Score for a `superclass` match
#' (should be lower than `class`)
#' @param score_chemical_npc_class Score for a `class` match
#' (should be the highest)
#'
#' @return A table containing the chemically weighted annotation
#'
#' @examples NULL
weight_chemo <-
  function(
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
