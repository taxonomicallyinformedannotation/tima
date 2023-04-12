utils::globalVariables(
  c(
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
#' @description This function weights the biologically weighted annotations according their chemical consistency
#'
#' @param annotationTableWeightedBioCleaned Table containing the biologically weighted annotation
#' @param weightSpectral Weight for the spectral score
#' @param weightBiological Weight for the biological score
#' @param weightChemical Weight for the chemical consistency score
#' @param scoreChemicalClaKingdom Score for a `Classyfire kingdom` match (should be lower than ` Classyfire superclass`)
#' @param scoreChemicalClaSuperclass Score for a `Classyfire superclass` match (should be lower than `Classyfire class`)
#' @param scoreChemicalClaClass Score for a `Classyfire class` match (should be lower than `Classyfire parent`)
#' @param scoreChemicalClaParent Score for a `Classyfire parent` match (should be the highest)
#' @param scoreChemicalNpcPathway Score for a `pathway` match (should be lower than `superclass`)
#' @param scoreChemicalNpcSuperclass Score for a `superclass` match (should be lower than `class`)
#' @param scoreChemicalNpcClass Score for a `class` match (should be the highest)
#'
#' @return A table containing the chemically weighted annotation
#'
#' @export
#'
#' @examples NULL
weight_chemo <-
  function(annotationTableWeightedBioCleaned = annotation_table_weighted_bio_cleaned,
           weightSpectral = weight_spectral,
           weightBiological = weight_biological,
           weightChemical = weight_chemical,
           scoreChemicalClaKingdom = score_chemical_cla_kingdom,
           scoreChemicalClaSuperclass = score_chemical_cla_superclass,
           scoreChemicalClaClass = score_chemical_cla_class,
           scoreChemicalClaParent = score_chemical_cla_parent,
           scoreChemicalNpcPathway = score_chemical_npc_pathway,
           scoreChemicalNpcSuperclass = score_chemical_npc_superclass,
           scoreChemicalNpcClass = score_chemical_npc_class) {
    log_debug("calculating chemical score ... \n")
    df1 <- annotationTableWeightedBioCleaned

    log_debug("... adding metadata \n")
    df2 <- df1 |>
      dplyr::distinct(
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
        stringr::str_detect(pattern = candidate_structure_1_cla_kingdom, string = consensus_structure_cla_kin)
      ) |>
      dplyr::mutate(score_chemical = scoreChemicalClaKingdom)

    log_debug("... (NPC) pathway \n")
    step_npc_pat <- df2 |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_structure_1_npc_pathway, string = consensus_structure_npc_pat)
      ) |>
      dplyr::mutate(score_chemical = scoreChemicalNpcPathway)

    log_debug("... (classyfire) superclass \n")
    step_cla_sup <- step_cla_kin |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_structure_2_cla_superclass, string = consensus_structure_cla_sup)
      ) |>
      dplyr::mutate(score_chemical = scoreChemicalClaSuperclass)

    log_debug("... (NPC) superclass \n")
    step_npc_sup <- step_npc_pat |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_structure_2_npc_superclass, string = consensus_structure_npc_sup)
      ) |>
      dplyr::mutate(score_chemical = scoreChemicalNpcSuperclass)

    log_debug("... (classyfire) class \n")
    step_cla_cla <- step_cla_sup |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_structure_3_cla_class, string = consensus_structure_cla_cla)
      ) |>
      dplyr::mutate(score_chemical = scoreChemicalClaClass)

    log_debug("... (NPC) class \n")
    step_npc_cla <- step_npc_sup |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_structure_3_npc_class, string = consensus_structure_npc_cla)
      ) |>
      dplyr::mutate(score_chemical = scoreChemicalNpcClass)

    log_debug("... (classyfire) parent \n")
    step_cla_par <- step_cla_cla |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_structure_4_cla_parent, string = consensus_structure_cla_par)
      ) |>
      dplyr::mutate(score_chemical = scoreChemicalClaParent)

    log_debug("... outputting best score \n")
    df3 <-
      dplyr::bind_rows(
        step_cla_kin,
        step_npc_pat,
        step_cla_sup,
        step_npc_sup,
        step_cla_cla,
        step_npc_cla,
        step_cla_par
      ) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.logical), as.numeric)) |>
      dplyr::mutate(score_chemical = ifelse(
        test = is.na(score_chemical),
        yes = 0,
        no = score_chemical
      )) |>
      dplyr::select(
        feature_id,
        structure_inchikey_2D,
        structure_smiles_2D,
        # candidate_structure_1_pathway,
        # candidate_structure_2_superclass,
        # candidate_structure_3_class,
        score_chemical
      )

    log_debug("... joining \n")
    df4 <- dplyr::left_join(
      df1 |>
        dplyr::select(-dplyr::contains("candidate_structure")),
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
            weightChemical +
              weightBiological +
              weightSpectral
          )) *
            weightChemical *
            score_chemical +
            (1 / (
              weightChemical +
                weightBiological +
                weightSpectral
            )) *
              weightBiological *
              score_biological +
            (1 / (
              weightChemical +
                weightBiological +
                weightSpectral
            )) *
              weightSpectral *
              as.numeric(score_input)
        )
      ) |>
      dplyr::group_by(feature_id) |>
      dplyr::arrange(dplyr::desc(score_chemical)) |>
      dplyr::arrange(dplyr::desc(score_pondered_chemo)) |>
      dplyr::distinct(feature_id,
        structure_inchikey_2D,
        structure_smiles_2D,
        # candidate_structure_1_pathway,
        # candidate_structure_2_superclass,
        # candidate_structure_3_class,
        .keep_all = TRUE
      ) |>
      dplyr::mutate(
        rank_initial = (dplyr::dense_rank(-as.numeric(score_input))),
        rank_final = (dplyr::dense_rank(-score_pondered_chemo))
      ) |>
      dplyr::arrange(rank_final) |>
      dplyr::ungroup() |>
      dplyr::arrange(as.numeric(feature_id)) |>
      data.frame()

    return(df4)
  }
