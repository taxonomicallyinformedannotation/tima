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
    "sample_organism_01_domain",
    "sample_organism_02_kingdom",
    "sample_organism_03_phylum",
    "sample_organism_04_class",
    "sample_organism_05_order",
    "sample_organism_06_family",
    "sample_organism_07_tribe",
    "sample_organism_08_genus",
    "sample_organism_09_species",
    "sample_organism_10_varietas",
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
    "structure_inchikey_2D",
    "structure_smiles_2D",
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
  function(annot_table_wei_bio_clean = get(
             "annot_table_wei_bio_clean",
             envir = parent.frame()
           ),
           weight_spectral = get(
             "weight_spectral",
             envir = parent.frame()
           ),
           weight_biological = get(
             "weight_biological",
             envir = parent.frame()
           ),
           weight_chemical = get(
             "weight_chemical",
             envir = parent.frame()
           ),
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
           )) {
    log_debug("calculating chemical score ... \n")
    df1 <- annot_table_wei_bio_clean

    log_debug("... adding metadata \n")
    df2 <- df1 |>
      tidytable::distinct(
        feature_id,
        structure_inchikey_2D,
        structure_smiles_2D,
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

    log_debug("... (classyfire) kingdom \n")
    step_cla_kin <- df2 |>
      dplyr::filter(
        stringi::stri_detect_regex(
          pattern = candidate_structure_1_cla_kingdom,
          str = consensus_structure_cla_kin
        )
      ) |>
      dplyr::mutate(score_chemical = score_chemical_cla_kingdom)

    log_debug("... (NPC) pathway \n")
    step_npc_pat <- df2 |>
      dplyr::filter(
        stringi::stri_detect_regex(
          pattern = candidate_structure_1_npc_pathway,
          str = consensus_structure_npc_pat
        )
      ) |>
      dplyr::mutate(score_chemical = score_chemical_npc_pathway)

    log_debug("... (classyfire) superclass \n")
    step_cla_sup <- step_cla_kin |>
      dplyr::filter(
        stringi::stri_detect_regex(
          pattern = candidate_structure_2_cla_superclass,
          str = consensus_structure_cla_sup
        )
      ) |>
      dplyr::mutate(score_chemical = score_chemical_cla_superclass)

    log_debug("... (NPC) superclass \n")
    step_npc_sup <- step_npc_pat |>
      dplyr::filter(
        stringi::stri_detect_regex(
          pattern = candidate_structure_2_npc_superclass,
          str = consensus_structure_npc_sup
        )
      ) |>
      dplyr::mutate(score_chemical = score_chemical_npc_superclass)

    log_debug("... (classyfire) class \n")
    step_cla_cla <- step_cla_sup |>
      dplyr::filter(
        stringi::stri_detect_regex(
          pattern = candidate_structure_3_cla_class,
          str = consensus_structure_cla_cla
        )
      ) |>
      dplyr::mutate(score_chemical = score_chemical_cla_class)

    log_debug("... (NPC) class \n")
    step_npc_cla <- step_npc_sup |>
      dplyr::filter(
        stringi::stri_detect_regex(
          pattern = candidate_structure_3_npc_class,
          str = consensus_structure_npc_cla
        )
      ) |>
      dplyr::mutate(score_chemical = score_chemical_npc_class)

    log_debug("... (classyfire) parent \n")
    step_cla_par <- step_cla_cla |>
      dplyr::filter(
        stringi::stri_detect_regex(
          pattern = candidate_structure_4_cla_parent,
          str = consensus_structure_cla_par
        )
      ) |>
      dplyr::mutate(score_chemical = score_chemical_cla_parent)

    log_debug("... outputting best score \n")
    df3 <-
      tidytable::bind_rows(
        step_cla_kin,
        step_npc_pat,
        step_cla_sup,
        step_npc_sup,
        step_cla_cla,
        step_npc_cla,
        step_cla_par
      ) |>
      dplyr::mutate(score_chemical = ifelse(
        test = is.na(score_chemical),
        yes = 0,
        no = score_chemical
      )) |>
      tidytable::select(
        feature_id,
        structure_inchikey_2D,
        structure_smiles_2D,
        score_chemical
      )

    log_debug("... joining \n")
    df4 <- tidytable::left_join(
      df1 |>
        tidytable::select(-tidytable::contains("candidate_structure")),
      df3
    ) |>
      data.frame()

    df4$score_chemical[is.na(df4$score_chemical)] <- 0

    log_debug("... cleaning \n")
    df4 <- df4 |>
      dplyr::rowwise() |>
      dplyr::mutate(
        score_pondered_chemo = (
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
      ) |>
      tidytable::arrange(tidytable::desc(score_chemical)) |>
      tidytable::arrange(tidytable::desc(score_pondered_chemo)) |>
      tidytable::distinct(feature_id,
        structure_inchikey_2D,
        structure_smiles_2D,
        .keep_all = TRUE
      ) |>
      dplyr::mutate(
        rank_initial = dplyr::dense_rank(-as.numeric(score_input)),
        rank_final = dplyr::dense_rank(-score_pondered_chemo),
        .by = c(feature_id)
      ) |>
      tidytable::arrange(rank_final) |>
      dplyr::arrange(as.numeric(feature_id)) |>
      data.frame()

    return(df4)
  }
