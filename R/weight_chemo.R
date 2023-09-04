utils::globalVariables(
  c(
    "annot_table_wei_bio_clean",
    "candidate_structure_1_cla_kingdom",
    "candidate_structure_1_npc_pathway",
    "candidate_structure_2_cla_superclass",
    "candidate_structure_2_npc_superclass",
    "candidate_structure_3_cla_class",
    "candidate_structure_3_npc_class",
    "candidate_structure_4_cla_parent",
    "consensus_structure_cla_cla",
    "consensus_structure_cla_kin",
    "consensus_structure_cla_par",
    "consensus_structure_cla_sup",
    "consensus_structure_npc_cla",
    "consensus_structure_npc_pat",
    "consensus_structure_npc_sup",
    "consistency_score_chemical_1_cla_kingdom",
    "consistency_score_chemical_1_npc_pathway",
    "consistency_score_chemical_2_cla_superclass",
    "consistency_score_chemical_2_npc_superclass",
    "consistency_score_chemical_3_cla_class",
    "consistency_score_chemical_3_npc_class",
    "consistency_score_chemical_4_cla_parent",
    "feature_id",
    "rank_final",
    "score_biological",
    "score_chemical",
    "score_chemical_cla_class",
    "score_chemical_cla_kingdom",
    "score_chemical_cla_parent",
    "score_chemical_cla_superclass",
    "score_chemical_npc_class",
    "score_chemical_npc_pathway",
    "score_chemical_npc_superclass",
    "score_input",
    "score_pondered_chemo",
    "weight_biological",
    "weight_chemical",
    "weight_spectral"
  )
)

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
#' @export
#'
#' @examples NULL
weight_chemo <-
  function(annot_table_wei_bio_clean = get("annot_table_wei_bio_clean",
             envir = parent.frame()
           ),
           weight_spectral = get("weight_spectral",
             envir = parent.frame()
           ),
           weight_biological = get("weight_biological",
             envir = parent.frame()
           ),
           weight_chemical = get("weight_chemical",
             envir = parent.frame()
           ),
           score_chemical_cla_kingdom = get("score_chemical_cla_kingdom",
             envir = parent.frame()
           ),
           score_chemical_cla_superclass = get("score_chemical_cla_superclass",
             envir = parent.frame()
           ),
           score_chemical_cla_class = get("score_chemical_cla_class",
             envir = parent.frame()
           ),
           score_chemical_cla_parent = get("score_chemical_cla_parent",
             envir = parent.frame()
           ),
           score_chemical_npc_pathway = get("score_chemical_npc_pathway",
             envir = parent.frame()
           ),
           score_chemical_npc_superclass = get("score_chemical_npc_superclass",
             envir = parent.frame()
           ),
           score_chemical_npc_class = get("score_chemical_npc_class",
             envir = parent.frame()
           )) {
    df2 <- annot_table_wei_bio_clean |>
      tidytable::distinct(
        candidate_structure_1_cla_kingdom,
        candidate_structure_1_npc_pathway,
        candidate_structure_2_cla_superclass,
        candidate_structure_2_npc_superclass,
        candidate_structure_3_cla_class,
        candidate_structure_3_npc_class,
        candidate_structure_4_cla_parent,
        consensus_structure_cla_kin,
        consistency_score_chemical_1_cla_kingdom,
        consensus_structure_npc_pat,
        consistency_score_chemical_1_npc_pathway,
        consensus_structure_cla_sup,
        consistency_score_chemical_2_cla_superclass,
        consensus_structure_npc_sup,
        consistency_score_chemical_2_npc_superclass,
        consensus_structure_cla_cla,
        consistency_score_chemical_3_cla_class,
        consensus_structure_npc_cla,
        consistency_score_chemical_3_npc_class,
        consensus_structure_cla_par,
        consistency_score_chemical_4_cla_parent
      )

    log_debug("calculating chemical score at all levels ... \n")
    log_debug("... (classyfire) kingdom \n")
    step_cla_kin <- df2 |>
      tidytable::distinct(
        candidate_structure_1_cla_kingdom,
        consensus_structure_cla_kin,
        consistency_score_chemical_1_cla_kingdom
      ) |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_structure_1_cla_kingdom,
          str = consensus_structure_cla_kin
        )
      ) |>
      tidytable::mutate(score_chemical_1 = score_chemical_cla_kingdom *
        1)

    log_debug("... (NPC) pathway \n")
    step_npc_pat <- df2 |>
      tidytable::distinct(
        candidate_structure_1_npc_pathway,
        consensus_structure_npc_pat,
        consistency_score_chemical_1_npc_pathway
      ) |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_structure_1_npc_pathway,
          str = consensus_structure_npc_pat
        )
      ) |>
      tidytable::mutate(score_chemical_2 = score_chemical_npc_pathway *
        1)

    log_debug("... (classyfire) superclass \n")
    step_cla_sup <- df2 |>
      tidytable::distinct(
        candidate_structure_2_cla_superclass,
        consensus_structure_cla_sup,
        consistency_score_chemical_2_cla_superclass
      ) |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_structure_2_cla_superclass,
          str = consensus_structure_cla_sup
        )
      ) |>
      tidytable::mutate(score_chemical_3 = score_chemical_cla_superclass *
        1)

    log_debug("... (NPC) superclass \n")
    step_npc_sup <- df2 |>
      tidytable::distinct(
        candidate_structure_2_npc_superclass,
        consensus_structure_npc_sup,
        consistency_score_chemical_2_npc_superclass
      ) |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_structure_2_npc_superclass,
          str = consensus_structure_npc_sup
        )
      ) |>
      tidytable::mutate(score_chemical_4 = score_chemical_npc_superclass *
        1)

    log_debug("... (classyfire) class \n")
    step_cla_cla <- df2 |>
      tidytable::distinct(
        candidate_structure_3_cla_class,
        consensus_structure_cla_cla,
        consistency_score_chemical_3_cla_class
      ) |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_structure_3_cla_class,
          str = consensus_structure_cla_cla
        )
      ) |>
      tidytable::mutate(score_chemical_5 = score_chemical_cla_class *
        1)

    log_debug("... (NPC) class \n")
    step_npc_cla <- df2 |>
      tidytable::distinct(
        candidate_structure_3_npc_class,
        consensus_structure_npc_cla,
        consistency_score_chemical_3_npc_class
      ) |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_structure_3_npc_class,
          str = consensus_structure_npc_cla
        )
      ) |>
      tidytable::mutate(score_chemical_6 = score_chemical_npc_class *
        1)

    log_debug("... (classyfire) parent \n")
    step_cla_par <- df2 |>
      tidytable::distinct(
        candidate_structure_4_cla_parent,
        consensus_structure_cla_par,
        consistency_score_chemical_4_cla_parent
      ) |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_structure_4_cla_parent,
          str = consensus_structure_cla_par
        )
      ) |>
      tidytable::mutate(score_chemical_7 = score_chemical_cla_parent *
        consistency_score_chemical_4_cla_parent)

    log_debug("... keeping best chemical score \n")
    annot_table_wei_chemo <- df2 |>
      tidytable::left_join(step_cla_kin) |>
      tidytable::left_join(step_npc_pat) |>
      tidytable::left_join(step_cla_sup) |>
      tidytable::left_join(step_npc_sup) |>
      tidytable::left_join(step_cla_cla) |>
      tidytable::left_join(step_npc_cla) |>
      tidytable::left_join(step_cla_par) |>
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
      tidytable::select(
        -tidytable::contains("score_chemical_")
      ) |>
      tidytable::right_join(annot_table_wei_bio_clean) |>
      log_pipe("... calculating weighted chemical score \n") |>
      tidytable::mutate(
        score_pondered_chemo =
          (1 / (
            weight_chemical +
              weight_biological +
              weight_spectral
          )) *
            weight_chemical *
            score_chemical +
            (1 / (
              weight_chemical +
                weight_biological +
                weight_spectral
            )) *
              weight_biological *
              score_biological +
            (1 / (
              weight_chemical +
                weight_biological +
                weight_spectral
            )) *
              weight_spectral *
              as.numeric(score_input)
      )

    rm(
      annot_table_wei_bio_clean,
      df2,
      step_cla_kin,
      step_npc_pat,
      step_cla_sup,
      step_npc_sup,
      step_cla_cla,
      step_npc_cla,
      step_cla_par
    )

    return(annot_table_wei_chemo)
  }
