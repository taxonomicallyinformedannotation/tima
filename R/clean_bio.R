utils::globalVariables(
  c(
    "candidate_structure_1_cla_kingdom",
    "candidate_structure_1_npc_pathway",
    "candidate_structure_2_cla_superclass",
    "candidate_structure_2_npc_superclass",
    "candidate_structure_3_cla_class",
    "candidate_structure_3_npc_class",
    "candidate_structure_4_cla_parent",
    "candidates_initial",
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
    "consistency_structure_cla_cla",
    "consistency_structure_cla_kin",
    "consistency_structure_cla_par",
    "consistency_structure_cla_sup",
    "consistency_structure_npc_cla",
    "consistency_structure_npc_pat",
    "consistency_structure_npc_sup",
    "count_cla",
    "count_kin",
    "count_par",
    "count_pat",
    "count_sup",
    "edges_table",
    "feature_id",
    "feature_source",
    "feature_target",
    "minimal_ms1_bio",
    "n",
    "rank_avg_cla",
    "rank_avg_par",
    "rank_avg_pat",
    "rank_avg_sup",
    "rank_final",
    "score_biological",
    "score_input",
    "structure_inchikey_2D",
    "structure_taxonomy_classyfire_01kingdom",
    "structure_taxonomy_classyfire_02superclass",
    "structure_taxonomy_classyfire_03class",
    "structure_taxonomy_classyfire_04directparent",
    "structure_taxonomy_npclassifier_01pathway",
    "structure_taxonomy_npclassifier_02superclass",
    "structure_taxonomy_npclassifier_03class"
  )
)

#' @title Clean bio
#'
#' @description This function cleans the results obtained after biological weighting
#'
#' @param annotationTableWeightedBio Table containing your biologically weighted annotation
#' @param edgesTable Table containing the edges between features
#' @param candidatesInitial Number of initial candidates to keep
#' @param minimalMs1Bio Minimal biological score to keep MS1 based annotation
#'
#' @return A table containing the biologically weighted annotation where only a given number of initial candidates are kept
#'
#' @export
#'
#' @seealso weight_bio
#'
#' @examples NULL
clean_bio <-
  function(annotationTableWeightedBio = annotation_table_weighted_bio,
           edgesTable = edges_table,
           candidatesInitial = candidates_initial,
           minimalMs1Bio = minimal_ms1_bio) {
    log_debug(
      "keeping only MS1 candidates with minimum \n",
      minimalMs1Bio,
      " biological score \n"
    )

    df01 <- annotationTableWeightedBio |>
      dplyr::filter(score_input > 0 |
        # Those lines are to keep ms1 annotation
        score_biological >= minimalMs1Bio)

    log_debug("erasing other MS1 candidates \n")
    df02 <-
      tidytable::anti_join(
        annotationTableWeightedBio |>
          tidytable::distinct(feature_id,
            structure_inchikey_2D,
            .keep_all = TRUE
          ),
        df01
      ) |>
      dplyr::mutate(structure_inchikey_2D = "notAnnotated")

    df03 <- tidytable::bind_rows(df01, df02)

    df <- df03 |>
      dplyr::mutate(
        candidate_structure_1_cla_kingdom = structure_taxonomy_classyfire_01kingdom,
        candidate_structure_1_npc_pathway = structure_taxonomy_npclassifier_01pathway,
        candidate_structure_2_cla_superclass = structure_taxonomy_classyfire_02superclass,
        candidate_structure_2_npc_superclass = structure_taxonomy_npclassifier_02superclass,
        candidate_structure_3_cla_class = structure_taxonomy_classyfire_03class,
        candidate_structure_3_npc_class = structure_taxonomy_npclassifier_03class,
        candidate_structure_4_cla_parent = structure_taxonomy_classyfire_04directparent
      ) |>
      tidytable::distinct(
        structure_inchikey_2D,
        feature_id,
        candidate_structure_1_cla_kingdom,
        candidate_structure_1_npc_pathway,
        candidate_structure_2_cla_superclass,
        candidate_structure_2_npc_superclass,
        candidate_structure_3_cla_class,
        candidate_structure_3_npc_class,
        candidate_structure_4_cla_parent,
        rank_final,
        .keep_all = TRUE
      ) |>
      data.frame()

    ## Loosing CANOPUS from SIRIUS
    ## TODO improve
    log_debug("adding \"notAnnotated\" \n")
    df$candidate_structure_1_cla_kingdom[df["structure_inchikey_2D"] == "notAnnotated"] <-
      "notAnnotated"
    df$candidate_structure_1_npc_pathway[df["structure_inchikey_2D"] == "notAnnotated"] <-
      "notAnnotated"
    df$candidate_structure_2_cla_superclass[df["structure_inchikey_2D"] == "notAnnotated"] <-
      "notAnnotated"
    df$candidate_structure_2_npc_superclass[df["structure_inchikey_2D"] == "notAnnotated"] <-
      "notAnnotated"
    df$candidate_structure_3_cla_class[df["structure_inchikey_2D"] == "notAnnotated"] <-
      "notAnnotated"
    df$candidate_structure_3_npc_class[df["structure_inchikey_2D"] == "notAnnotated"] <-
      "notAnnotated"
    df$candidate_structure_4_cla_parent[df["structure_inchikey_2D"] == "notAnnotated"] <-
      "notAnnotated"

    log_debug("adding \"notClassified\" \n")
    df[is.character(is.na(df))] <- "notClassified"

    df <- df |>
      tidytable::tidytable()
    # log_debug("keeping clusters with at least 3 features  \n")
    # df1 <- df |>
    #   dplyr::filter(component_id != -1) |>
    #   tidytable::group_by(component_id) |>
    #   tidytable::distinct(feature_id,
    #     structure_inchikey_2D,
    #     .keep_all = TRUE
    #   ) |>
    #   tidytable::add_count() |>
    #   tidytable::ungroup() |>
    #   dplyr::filter(n >= 3) |>
    #   tidytable::select(-n)
    #
    # log_debug("keeping clusters with less than 3 features \n")
    # df2 <- tidytable::full_join(
    #   x = df |>
    #     dplyr::filter(component_id == -1),
    #   y = df |>
    #     tidytable::group_by(component_id) |>
    #     tidytable::distinct(feature_id, .keep_all = TRUE) |>
    #     tidytable::add_count() |>
    #     tidytable::ungroup() |>
    #     dplyr::filter(n <= 2) |>
    #     tidytable::select(-n)
    # )

    log_debug("calculating chemical consistency features with at least 2 neighbors ... \n")

    log_debug("... among edges ... \n")
    df3 <-
      tidytable::right_join(
        edgesTable |>
          tidytable::group_by(feature_source) |>
          tidytable::add_count() |>
          tidytable::ungroup() |>
          dplyr::filter(n >= 2) |>
          tidytable::select(-n),
        df,
        by = stats::setNames("feature_id", "feature_target")
      ) |>
      dplyr::filter(!is.na(feature_source))

    log_debug("... at the (classyfire) kingdom level \n")
    freq_cla_kin <- df3 |>
      tidytable::group_by(
        feature_source,
        candidate_structure_1_cla_kingdom
      ) |>
      dplyr::mutate(count_kin = tidytable::n_distinct(feature_target)) |>
      tidytable::ungroup() |>
      tidytable::group_by(feature_source) |>
      dplyr::mutate(sum = tidytable::n_distinct(feature_target)) |>
      dplyr::mutate(consistency_structure_cla_kin = count_kin / sum) |>
      tidytable::ungroup() |>
      tidytable::group_by(
        feature_target,
        candidate_structure_1_cla_kingdom
      ) |>
      dplyr::mutate(rank_final = as.numeric(rank_final)) |>
      tidytable::arrange(rank_final) |>
      tidytable::distinct(feature_source,
        candidate_structure_1_cla_kingdom,
        .keep_all = TRUE
      ) |>
      tidytable::group_by(
        feature_source,
        candidate_structure_1_cla_kingdom
      ) |>
      dplyr::mutate(
        rank_avg_pat = ifelse(
          test = candidate_structure_1_cla_kingdom == "notAnnotated" |
            candidate_structure_1_cla_kingdom == "notClassified",
          yes = candidatesInitial / 2,
          no = mean(as.numeric(rank_final))
        )
      ) |>
      dplyr::mutate(consistency_score_chemical_1_cla_kingdom = consistency_structure_cla_kin / sqrt(rank_avg_pat)) |>
      tidytable::group_by(feature_source) |>
      tidytable::arrange(-consistency_score_chemical_1_cla_kingdom) |>
      tidytable::ungroup() |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        consensus_structure_cla_kin = candidate_structure_1_cla_kingdom,
        consistency_structure_cla_kin,
        consistency_score_chemical_1_cla_kingdom
      ) |>
      dplyr::mutate(
        consensus_structure_cla_kin = ifelse(
          test = consistency_score_chemical_1_cla_kingdom > 0.5,
          yes = consensus_structure_cla_kin,
          no = "notConsistent"
        )
      )

    log_debug("... at the (NPC) pathway level \n")
    freq_npc_pat <- df3 |>
      tidytable::group_by(
        feature_source,
        candidate_structure_1_npc_pathway
      ) |>
      dplyr::mutate(count_pat = tidytable::n_distinct(feature_target)) |>
      tidytable::ungroup() |>
      tidytable::group_by(feature_source) |>
      dplyr::mutate(sum = tidytable::n_distinct(feature_target)) |>
      dplyr::mutate(consistency_structure_npc_pat = count_pat / sum) |>
      tidytable::ungroup() |>
      tidytable::group_by(
        feature_target,
        candidate_structure_1_npc_pathway
      ) |>
      dplyr::mutate(rank_final = as.numeric(rank_final)) |>
      tidytable::arrange(rank_final) |>
      tidytable::distinct(feature_source,
        candidate_structure_1_npc_pathway,
        .keep_all = TRUE
      ) |>
      tidytable::group_by(
        feature_source,
        candidate_structure_1_npc_pathway
      ) |>
      dplyr::mutate(
        rank_avg_pat = ifelse(
          test = candidate_structure_1_npc_pathway == "notAnnotated" |
            candidate_structure_1_npc_pathway == "notClassified",
          yes = candidatesInitial / 2,
          no = mean(as.numeric(rank_final))
        )
      ) |>
      dplyr::mutate(consistency_score_chemical_1_npc_pathway = consistency_structure_npc_pat / sqrt(rank_avg_pat)) |>
      tidytable::group_by(feature_source) |>
      tidytable::arrange(-consistency_score_chemical_1_npc_pathway) |>
      tidytable::ungroup() |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        consensus_structure_npc_pat = candidate_structure_1_npc_pathway,
        consistency_structure_npc_pat,
        consistency_score_chemical_1_npc_pathway
      ) |>
      dplyr::mutate(
        consensus_structure_npc_pat = ifelse(
          test = consistency_score_chemical_1_npc_pathway > 0.5,
          yes = consensus_structure_npc_pat,
          no = "notConsistent"
        )
      )

    log_debug("... at the (classyfire) superclass level \n")
    freq_cla_sup <- df3 |>
      tidytable::group_by(
        feature_source,
        candidate_structure_2_cla_superclass
      ) |>
      dplyr::mutate(count_sup = tidytable::n_distinct(feature_target)) |>
      tidytable::ungroup() |>
      tidytable::group_by(feature_source) |>
      dplyr::mutate(sum = tidytable::n_distinct(feature_target)) |>
      dplyr::mutate(consistency_structure_cla_sup = count_sup / sum) |>
      tidytable::ungroup() |>
      tidytable::group_by(
        feature_target,
        candidate_structure_2_cla_superclass
      ) |>
      dplyr::mutate(rank_final = as.numeric(rank_final)) |>
      tidytable::arrange(rank_final) |>
      tidytable::distinct(feature_source,
        candidate_structure_2_cla_superclass,
        .keep_all = TRUE
      ) |>
      tidytable::group_by(
        feature_source,
        candidate_structure_2_cla_superclass
      ) |>
      dplyr::mutate(
        rank_avg_sup = ifelse(
          test = candidate_structure_2_cla_superclass == "notAnnotated" |
            candidate_structure_2_cla_superclass == "notClassified",
          yes = candidatesInitial / 2,
          no = mean(as.numeric(rank_final))
        )
      ) |>
      dplyr::mutate(consistency_score_chemical_2_cla_superclass = consistency_structure_cla_sup / sqrt(rank_avg_sup)) |>
      tidytable::group_by(feature_source) |>
      tidytable::arrange(-consistency_score_chemical_2_cla_superclass) |>
      tidytable::ungroup() |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        consensus_structure_cla_sup = candidate_structure_2_cla_superclass,
        consistency_structure_cla_sup,
        consistency_score_chemical_2_cla_superclass
      ) |>
      dplyr::mutate(
        consensus_structure_cla_sup = ifelse(
          test = consistency_score_chemical_2_cla_superclass > 0.5,
          yes = consensus_structure_cla_sup,
          no = "notConsistent"
        )
      )

    log_debug("... at the (NPC) superclass level \n")
    freq_npc_sup <- df3 |>
      tidytable::group_by(
        feature_source,
        candidate_structure_2_npc_superclass
      ) |>
      dplyr::mutate(count_sup = tidytable::n_distinct(feature_target)) |>
      tidytable::ungroup() |>
      tidytable::group_by(feature_source) |>
      dplyr::mutate(sum = tidytable::n_distinct(feature_target)) |>
      dplyr::mutate(consistency_structure_npc_sup = count_sup / sum) |>
      tidytable::ungroup() |>
      tidytable::group_by(
        feature_target,
        candidate_structure_2_npc_superclass
      ) |>
      dplyr::mutate(rank_final = as.numeric(rank_final)) |>
      tidytable::arrange(rank_final) |>
      tidytable::distinct(feature_source,
        candidate_structure_2_npc_superclass,
        .keep_all = TRUE
      ) |>
      tidytable::group_by(
        feature_source,
        candidate_structure_2_npc_superclass
      ) |>
      dplyr::mutate(
        rank_avg_sup = ifelse(
          test = candidate_structure_2_npc_superclass == "notAnnotated" |
            candidate_structure_2_npc_superclass == "notClassified",
          yes = candidatesInitial / 2,
          no = mean(as.numeric(rank_final))
        )
      ) |>
      dplyr::mutate(consistency_score_chemical_2_npc_superclass = consistency_structure_npc_sup / sqrt(rank_avg_sup)) |>
      tidytable::group_by(feature_source) |>
      tidytable::arrange(-consistency_score_chemical_2_npc_superclass) |>
      tidytable::ungroup() |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        consensus_structure_npc_sup = candidate_structure_2_npc_superclass,
        consistency_structure_npc_sup,
        consistency_score_chemical_2_npc_superclass
      ) |>
      dplyr::mutate(
        consensus_structure_npc_sup = ifelse(
          test = consistency_score_chemical_2_npc_superclass > 0.5,
          yes = consensus_structure_npc_sup,
          no = "notConsistent"
        )
      )

    log_debug("... at the (classyfire) class level \n")
    freq_cla_cla <- df3 |>
      tidytable::group_by(
        feature_source,
        candidate_structure_3_cla_class
      ) |>
      dplyr::mutate(count_cla = tidytable::n_distinct(feature_target)) |>
      tidytable::ungroup() |>
      tidytable::group_by(feature_source) |>
      dplyr::mutate(sum = tidytable::n_distinct(feature_target)) |>
      dplyr::mutate(consistency_structure_cla_cla = count_cla / sum) |>
      tidytable::ungroup() |>
      tidytable::group_by(
        feature_target,
        candidate_structure_3_cla_class
      ) |>
      dplyr::mutate(rank_final = as.numeric(rank_final)) |>
      tidytable::arrange(rank_final) |>
      tidytable::distinct(feature_source,
        candidate_structure_3_cla_class,
        .keep_all = TRUE
      ) |>
      tidytable::group_by(
        feature_source,
        candidate_structure_3_cla_class
      ) |>
      dplyr::mutate(
        rank_avg_cla = ifelse(
          test = candidate_structure_3_cla_class == "notAnnotated" |
            candidate_structure_3_cla_class == "notClassified",
          yes = candidatesInitial / 2,
          no = mean(as.numeric(rank_final))
        )
      ) |>
      dplyr::mutate(consistency_score_chemical_3_cla_class = consistency_structure_cla_cla / sqrt(rank_avg_cla)) |>
      tidytable::group_by(feature_source) |>
      tidytable::arrange(-consistency_score_chemical_3_cla_class) |>
      tidytable::ungroup() |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        consensus_structure_cla_cla = candidate_structure_3_cla_class,
        consistency_structure_cla_cla,
        consistency_score_chemical_3_cla_class
      ) |>
      dplyr::mutate(
        consensus_structure_cla_cla = ifelse(
          test = consistency_score_chemical_3_cla_class > 0.5,
          yes = consensus_structure_cla_cla,
          no = "notConsistent"
        )
      )

    log_debug("... at the (NPC) class level \n")
    freq_npc_cla <- df3 |>
      tidytable::group_by(
        feature_source,
        candidate_structure_3_npc_class
      ) |>
      dplyr::mutate(count_cla = tidytable::n_distinct(feature_target)) |>
      tidytable::ungroup() |>
      tidytable::group_by(feature_source) |>
      dplyr::mutate(sum = tidytable::n_distinct(feature_target)) |>
      dplyr::mutate(consistency_structure_npc_cla = count_cla / sum) |>
      tidytable::ungroup() |>
      tidytable::group_by(
        feature_target,
        candidate_structure_3_npc_class
      ) |>
      dplyr::mutate(rank_final = as.numeric(rank_final)) |>
      tidytable::arrange(rank_final) |>
      tidytable::distinct(feature_source,
        candidate_structure_3_npc_class,
        .keep_all = TRUE
      ) |>
      tidytable::group_by(
        feature_source,
        candidate_structure_3_npc_class
      ) |>
      dplyr::mutate(
        rank_avg_cla = ifelse(
          test = candidate_structure_3_npc_class == "notAnnotated" |
            candidate_structure_3_npc_class == "notClassified",
          yes = candidatesInitial / 2,
          no = mean(as.numeric(rank_final))
        )
      ) |>
      dplyr::mutate(consistency_score_chemical_3_npc_class = consistency_structure_npc_cla / sqrt(rank_avg_cla)) |>
      tidytable::group_by(feature_source) |>
      tidytable::arrange(-consistency_score_chemical_3_npc_class) |>
      tidytable::ungroup() |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        consensus_structure_npc_cla = candidate_structure_3_npc_class,
        consistency_structure_npc_cla,
        consistency_score_chemical_3_npc_class
      ) |>
      dplyr::mutate(
        consensus_structure_npc_cla = ifelse(
          test = consistency_score_chemical_3_npc_class > 0.5,
          yes = consensus_structure_npc_cla,
          no = "notConsistent"
        )
      )

    log_debug("... at the (classyfire) parent level \n")
    freq_cla_par <- df3 |>
      tidytable::group_by(
        feature_source,
        candidate_structure_4_cla_parent
      ) |>
      dplyr::mutate(count_par = tidytable::n_distinct(feature_target)) |>
      tidytable::ungroup() |>
      tidytable::group_by(feature_source) |>
      dplyr::mutate(sum = tidytable::n_distinct(feature_target)) |>
      dplyr::mutate(consistency_structure_cla_par = count_par / sum) |>
      tidytable::ungroup() |>
      tidytable::group_by(
        feature_target,
        candidate_structure_4_cla_parent
      ) |>
      dplyr::mutate(rank_final = as.numeric(rank_final)) |>
      tidytable::arrange(rank_final) |>
      tidytable::distinct(feature_source,
        candidate_structure_4_cla_parent,
        .keep_all = TRUE
      ) |>
      tidytable::group_by(
        feature_source,
        candidate_structure_4_cla_parent
      ) |>
      dplyr::mutate(
        rank_avg_par = ifelse(
          test = candidate_structure_4_cla_parent == "notAnnotated" |
            candidate_structure_4_cla_parent == "notClassified",
          yes = candidatesInitial / 2,
          no = mean(as.numeric(rank_final))
        )
      ) |>
      dplyr::mutate(consistency_score_chemical_4_cla_parent = consistency_structure_cla_par / sqrt(rank_avg_par)) |>
      tidytable::group_by(feature_source) |>
      tidytable::arrange(-consistency_score_chemical_4_cla_parent) |>
      tidytable::ungroup() |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        consensus_structure_cla_par = candidate_structure_4_cla_parent,
        consistency_structure_cla_par,
        consistency_score_chemical_4_cla_parent
      ) |>
      dplyr::mutate(
        consensus_structure_cla_par = ifelse(
          test = consistency_score_chemical_4_cla_parent > 0.5,
          yes = consensus_structure_cla_par,
          no = "notConsistent"
        )
      )

    log_debug("joining all except -1 together \n")
    df4 <-
      tidytable::left_join(df,
        freq_cla_kin,
        by = stats::setNames("feature_source", "feature_id")
      ) |>
      tidytable::left_join(freq_npc_pat,
        by = stats::setNames("feature_source", "feature_id")
      ) |>
      tidytable::left_join(freq_cla_sup,
        by = stats::setNames("feature_source", "feature_id")
      ) |>
      tidytable::left_join(freq_npc_sup,
        by = stats::setNames("feature_source", "feature_id")
      ) |>
      tidytable::left_join(freq_cla_cla,
        by = stats::setNames("feature_source", "feature_id")
      ) |>
      tidytable::left_join(freq_npc_cla,
        by = stats::setNames("feature_source", "feature_id")
      ) |>
      tidytable::left_join(freq_cla_par,
        by = stats::setNames("feature_source", "feature_id")
      ) |>
      tidytable::select(
        feature_id,
        tidytable::everything()
      ) |>
      # In case there are no consensus at all because no network
      dplyr::mutate(dplyr::across(dplyr::where(is.logical), as.character)) |>
      tidytable::tidytable()

    # TODO Think about better scoring option
    log_debug("adding dummy consistency for features with less than 2 neighbors \n")
    dummy_consistency <- df4 |>
      dplyr::mutate(
        consensus_structure_cla_kin = dplyr::coalesce(consensus_structure_cla_kin, "dummy"),
        consistency_structure_cla_kin = dplyr::coalesce(consistency_structure_cla_kin, 1),
        consistency_score_chemical_1_cla_kingdom = dplyr::coalesce(consistency_score_chemical_1_cla_kingdom, 0),
        consensus_structure_npc_pat = dplyr::coalesce(consensus_structure_npc_pat, "dummy"),
        consistency_structure_npc_pat = dplyr::coalesce(consistency_structure_npc_pat, 1),
        consistency_score_chemical_1_npc_pathway = dplyr::coalesce(consistency_score_chemical_1_npc_pathway, 0),
        consensus_structure_cla_sup = dplyr::coalesce(consensus_structure_cla_sup, "dummy"),
        consistency_structure_cla_sup = dplyr::coalesce(consistency_structure_cla_sup, 1),
        consistency_score_chemical_2_cla_superclass = dplyr::coalesce(consistency_score_chemical_2_cla_superclass, 0),
        consensus_structure_npc_sup = dplyr::coalesce(consensus_structure_npc_sup, "dummy"),
        consistency_structure_npc_sup = dplyr::coalesce(consistency_structure_npc_sup, 1),
        consistency_score_chemical_2_npc_superclass = dplyr::coalesce(consistency_score_chemical_2_npc_superclass, 0),
        consensus_structure_cla_cla = dplyr::coalesce(consensus_structure_cla_cla, "dummy"),
        consistency_structure_cla_cla = dplyr::coalesce(consistency_structure_cla_cla, 1),
        consistency_score_chemical_3_cla_class = dplyr::coalesce(consistency_score_chemical_3_cla_class, 0),
        consensus_structure_npc_cla = dplyr::coalesce(consensus_structure_npc_cla, "dummy"),
        consistency_structure_npc_cla = dplyr::coalesce(consistency_structure_npc_cla, 1),
        consistency_score_chemical_3_npc_class = dplyr::coalesce(consistency_score_chemical_3_npc_class, 0),
        consensus_structure_cla_par = dplyr::coalesce(consensus_structure_cla_par, "dummy"),
        consistency_structure_cla_par = dplyr::coalesce(consistency_structure_cla_par, 1),
        consistency_score_chemical_4_cla_parent = dplyr::coalesce(consistency_score_chemical_4_cla_parent, 0)
      )

    return(dummy_consistency)
  }
