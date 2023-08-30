utils::globalVariables(
  c(
    "annot_table_wei_bio",
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
    "n",
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
#' @description This function cleans the results
#'    obtained after biological weighting
#'
#' @param annot_table_wei_bio Table containing your
#'    biologically weighted annotation
#' @param edges_table Table containing the edges between features
#' @param minimal_consistency Minimal consistency score for a class. FLOAT
#'
#' @return A table containing the biologically weighted annotation
#'    where only a given number of initial candidates are kept
#'
#' @export
#'
#' @seealso weight_bio
#'
#' @examples NULL
clean_bio <-
  function(annot_table_wei_bio = get("annot_table_wei_bio",
             envir = parent.frame()
           ),
           edges_table = get("edges_table",
             envir = parent.frame()
           ),
           minimal_consistency = get("minimal_consistency",
             envir = parent.frame()
           )) {
    df <- annot_table_wei_bio |>
      tidytable::mutate(
        candidate_structure_1_cla_kingdom =
          structure_taxonomy_classyfire_01kingdom,
        candidate_structure_1_npc_pathway =
          structure_taxonomy_npclassifier_01pathway,
        candidate_structure_2_cla_superclass =
          structure_taxonomy_classyfire_02superclass,
        candidate_structure_2_npc_superclass =
          structure_taxonomy_npclassifier_02superclass,
        candidate_structure_3_cla_class =
          structure_taxonomy_classyfire_03class,
        candidate_structure_3_npc_class =
          structure_taxonomy_npclassifier_03class,
        candidate_structure_4_cla_parent =
          structure_taxonomy_classyfire_04directparent
      ) |>
      tidytable::distinct(
        feature_id,
        structure_inchikey_2D,
        candidate_structure_1_cla_kingdom,
        candidate_structure_1_npc_pathway,
        candidate_structure_2_cla_superclass,
        candidate_structure_2_npc_superclass,
        candidate_structure_3_cla_class,
        candidate_structure_3_npc_class,
        candidate_structure_4_cla_parent,
        score_pondered_bio,
        .keep_all = TRUE
      ) |>
      tidytable::arrange(tidytable::desc(score_pondered_bio))

    ## Loosing CANOPUS from SIRIUS
    ## TODO improve
    # log_debug("adding \"notAnnotated\" \n")
    # df$candidate_structure_1_cla_kingdom
    # [df["structure_inchikey_2D"] == "notAnnotated"] <-
    #   "notAnnotated"
    # and so on ...

    log_debug("adding \"notClassified\" \n")
    df[is.character(is.na(df))] <- "notClassified"

    # log_debug("keeping clusters with at least 3 features  \n")
    # df1 <- df |>
    #   tidytable::filter(component_id != -1) |>
    #   tidytable::group_by(component_id) |>
    #   tidytable::distinct(feature_id,
    #     structure_inchikey_2D,
    #     .keep_all = TRUE
    #   ) |>
    #   tidytable::add_count() |>
    #   tidytable::ungroup() |>
    #   tidytable::filter(n >= 3) |>
    #   tidytable::select(-n)
    #
    # log_debug("keeping clusters with less than 3 features \n")
    # df2 <- tidytable::full_join(
    #   x = df |>
    #     tidytable::filter(component_id == -1),
    #   y = df |>
    #     tidytable::group_by(component_id) |>
    #     tidytable::distinct(feature_id, .keep_all = TRUE) |>
    #     tidytable::add_count() |>
    #     tidytable::ungroup() |>
    #     tidytable::filter(n <= 2) |>
    #     tidytable::select(-n)
    # )

    log_debug("calculating chemical consistency
              features with at least 2 neighbors ... \n")

    log_debug("... among edges ... \n")
    df3 <-
      tidytable::right_join(
        edges_table |>
          tidytable::group_by(feature_source) |>
          tidytable::add_count() |>
          tidytable::ungroup() |>
          tidytable::filter(n >= 2) |>
          tidytable::select(-n),
        df |>
          tidytable::distinct(
            feature_id,
            candidate_structure_1_cla_kingdom,
            candidate_structure_1_npc_pathway,
            candidate_structure_2_cla_superclass,
            candidate_structure_2_npc_superclass,
            candidate_structure_3_cla_class,
            candidate_structure_3_npc_class,
            candidate_structure_4_cla_parent,
            score_pondered_bio
          ),
        by = stats::setNames("feature_id", "feature_target")
      ) |>
      tidytable::filter(!is.na(feature_source))

    log_debug("... at the (classyfire) kingdom level \n")
    freq_cla_kin <- df3 |>
      tidytable::distinct(
        feature_source,
        feature_target,
        candidate_structure_1_cla_kingdom,
        score_pondered_bio
      ) |>
      tidytable::mutate(
        count_kin = tidytable::n_distinct(feature_target),
        .by = c(
          feature_source,
          candidate_structure_1_cla_kingdom
        )
      ) |>
      tidytable::mutate(
        sum = tidytable::n_distinct(feature_target),
        .by = c(feature_source)
      ) |>
      tidytable::mutate(
        consistency_structure_cla_kin = count_kin / sum,
        .by = c(feature_source)
      ) |>
      tidytable::distinct(feature_source,
        candidate_structure_1_cla_kingdom,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        consistency_score_chemical_1_cla_kingdom =
          consistency_structure_cla_kin * score_pondered_bio,
        .by = c(
          feature_source,
          candidate_structure_1_cla_kingdom
        )
      ) |>
      tidytable::arrange(-consistency_score_chemical_1_cla_kingdom) |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        consensus_structure_cla_kin = candidate_structure_1_cla_kingdom,
        consistency_structure_cla_kin,
        consistency_score_chemical_1_cla_kingdom
      ) |>
      tidytable::mutate(
        consensus_structure_cla_kin = ifelse(
          test = consistency_score_chemical_1_cla_kingdom > minimal_consistency,
          yes = consensus_structure_cla_kin,
          no = "notConsistent"
        )
      )

    log_debug("... at the (NPC) pathway level \n")
    freq_npc_pat <- df3 |>
      tidytable::distinct(
        feature_source,
        feature_target,
        candidate_structure_1_npc_pathway,
        score_pondered_bio
      ) |>
      tidytable::mutate(
        count_pat = tidytable::n_distinct(feature_target),
        .by = c(
          feature_source,
          candidate_structure_1_npc_pathway
        )
      ) |>
      tidytable::mutate(
        sum = tidytable::n_distinct(feature_target),
        .by = c(feature_source)
      ) |>
      tidytable::mutate(
        consistency_structure_npc_pat = count_pat / sum,
        .by = c(feature_source)
      ) |>
      tidytable::distinct(feature_source,
        candidate_structure_1_npc_pathway,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        consistency_score_chemical_1_npc_pathway =
          consistency_structure_npc_pat * score_pondered_bio,
        .by = c(
          feature_source,
          candidate_structure_1_npc_pathway
        )
      ) |>
      tidytable::arrange(-consistency_score_chemical_1_npc_pathway) |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        consensus_structure_npc_pat = candidate_structure_1_npc_pathway,
        consistency_structure_npc_pat,
        consistency_score_chemical_1_npc_pathway
      ) |>
      tidytable::mutate(
        consensus_structure_npc_pat = ifelse(
          test = consistency_score_chemical_1_npc_pathway > minimal_consistency,
          yes = consensus_structure_npc_pat,
          no = "notConsistent"
        )
      )

    log_debug("... at the (classyfire) superclass level \n")
    freq_cla_sup <- df3 |>
      tidytable::distinct(
        feature_source,
        feature_target,
        candidate_structure_2_cla_superclass,
        score_pondered_bio
      ) |>
      tidytable::mutate(
        count_sup = tidytable::n_distinct(feature_target),
        .by = c(
          feature_source,
          candidate_structure_2_cla_superclass
        )
      ) |>
      tidytable::mutate(
        sum = tidytable::n_distinct(feature_target),
        .by = c(feature_source)
      ) |>
      tidytable::mutate(
        consistency_structure_cla_sup = count_sup / sum,
        .by = c(feature_source)
      ) |>
      tidytable::distinct(feature_source,
        candidate_structure_2_cla_superclass,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        consistency_score_chemical_2_cla_superclass =
          consistency_structure_cla_sup * score_pondered_bio,
        .by = c(
          feature_source,
          candidate_structure_2_cla_superclass
        )
      ) |>
      tidytable::arrange(-consistency_score_chemical_2_cla_superclass) |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        consensus_structure_cla_sup = candidate_structure_2_cla_superclass,
        consistency_structure_cla_sup,
        consistency_score_chemical_2_cla_superclass
      ) |>
      tidytable::mutate(
        consensus_structure_cla_sup = ifelse(
          test = consistency_score_chemical_2_cla_superclass > minimal_consistency,
          yes = consensus_structure_cla_sup,
          no = "notConsistent"
        )
      )

    log_debug("... at the (NPC) superclass level \n")
    freq_npc_sup <- df3 |>
      tidytable::distinct(
        feature_source,
        feature_target,
        candidate_structure_2_npc_superclass,
        score_pondered_bio
      ) |>
      tidytable::mutate(
        count_sup = tidytable::n_distinct(feature_target),
        .by = c(
          feature_source,
          candidate_structure_2_npc_superclass
        )
      ) |>
      tidytable::mutate(
        sum = tidytable::n_distinct(feature_target),
        .by = c(feature_source)
      ) |>
      tidytable::mutate(
        consistency_structure_npc_sup = count_sup / sum,
        .by = c(feature_source)
      ) |>
      tidytable::distinct(feature_source,
        candidate_structure_2_npc_superclass,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        consistency_score_chemical_2_npc_superclass =
          consistency_structure_npc_sup * score_pondered_bio,
        .by = c(
          feature_source,
          candidate_structure_2_npc_superclass
        )
      ) |>
      tidytable::arrange(-consistency_score_chemical_2_npc_superclass) |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        consensus_structure_npc_sup = candidate_structure_2_npc_superclass,
        consistency_structure_npc_sup,
        consistency_score_chemical_2_npc_superclass
      ) |>
      tidytable::mutate(
        consensus_structure_npc_sup = ifelse(
          test = consistency_score_chemical_2_npc_superclass > minimal_consistency,
          yes = consensus_structure_npc_sup,
          no = "notConsistent"
        )
      )

    log_debug("... at the (classyfire) class level \n")
    freq_cla_cla <- df3 |>
      tidytable::distinct(
        feature_source,
        feature_target,
        candidate_structure_3_cla_class,
        score_pondered_bio
      ) |>
      tidytable::mutate(
        count_cla = tidytable::n_distinct(feature_target),
        .by = c(
          feature_source,
          candidate_structure_3_cla_class
        )
      ) |>
      tidytable::mutate(
        sum = tidytable::n_distinct(feature_target),
        .by = c(feature_source)
      ) |>
      tidytable::mutate(
        consistency_structure_cla_cla = count_cla / sum,
        .by = c(feature_source)
      ) |>
      tidytable::distinct(feature_source,
        candidate_structure_3_cla_class,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        consistency_score_chemical_3_cla_class =
          consistency_structure_cla_cla * score_pondered_bio,
        .by = c(
          feature_source,
          candidate_structure_3_cla_class
        )
      ) |>
      tidytable::arrange(-consistency_score_chemical_3_cla_class) |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        consensus_structure_cla_cla = candidate_structure_3_cla_class,
        consistency_structure_cla_cla,
        consistency_score_chemical_3_cla_class
      ) |>
      tidytable::mutate(
        consensus_structure_cla_cla = ifelse(
          test = consistency_score_chemical_3_cla_class > minimal_consistency,
          yes = consensus_structure_cla_cla,
          no = "notConsistent"
        )
      )

    log_debug("... at the (NPC) class level \n")
    freq_npc_cla <- df3 |>
      tidytable::distinct(
        feature_source,
        feature_target,
        candidate_structure_3_npc_class,
        score_pondered_bio
      ) |>
      tidytable::mutate(
        count_cla = tidytable::n_distinct(feature_target),
        .by = c(
          feature_source,
          candidate_structure_3_npc_class
        )
      ) |>
      tidytable::mutate(
        sum = tidytable::n_distinct(feature_target),
        .by = c(feature_source)
      ) |>
      tidytable::mutate(
        consistency_structure_npc_cla = count_cla / sum,
        .by = c(feature_source)
      ) |>
      tidytable::distinct(feature_source,
        candidate_structure_3_npc_class,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        consistency_score_chemical_3_npc_class =
          consistency_structure_npc_cla * score_pondered_bio,
        .by = c(
          feature_source,
          candidate_structure_3_npc_class
        )
      ) |>
      tidytable::arrange(-consistency_score_chemical_3_npc_class) |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        consensus_structure_npc_cla = candidate_structure_3_npc_class,
        consistency_structure_npc_cla,
        consistency_score_chemical_3_npc_class
      ) |>
      tidytable::mutate(
        consensus_structure_npc_cla = ifelse(
          test = consistency_score_chemical_3_npc_class > minimal_consistency,
          yes = consensus_structure_npc_cla,
          no = "notConsistent"
        )
      )

    log_debug("... at the (classyfire) parent level \n")
    freq_cla_par <- df3 |>
      tidytable::distinct(
        feature_source,
        feature_target,
        candidate_structure_4_cla_parent,
        score_pondered_bio
      ) |>
      tidytable::mutate(
        count_par = tidytable::n_distinct(feature_target),
        .by = c(
          feature_source,
          candidate_structure_4_cla_parent
        )
      ) |>
      tidytable::mutate(
        sum = tidytable::n_distinct(feature_target),
        .by = c(feature_source)
      ) |>
      tidytable::mutate(
        consistency_structure_cla_par = count_par / sum,
        .by = c(feature_source)
      ) |>
      tidytable::distinct(feature_source,
        candidate_structure_4_cla_parent,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        consistency_score_chemical_4_cla_parent =
          consistency_structure_cla_par * score_pondered_bio,
        .by = c(
          feature_source,
          candidate_structure_4_cla_parent
        )
      ) |>
      tidytable::arrange(-consistency_score_chemical_4_cla_parent) |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        consensus_structure_cla_par = candidate_structure_4_cla_parent,
        consistency_structure_cla_par,
        consistency_score_chemical_4_cla_parent
      ) |>
      tidytable::mutate(
        consensus_structure_cla_par = ifelse(
          test = consistency_score_chemical_4_cla_parent > minimal_consistency,
          yes = consensus_structure_cla_par,
          no = "notConsistent"
        )
      )

    rm(df3)

    log_debug("joining all except -1 together \n")
    annot_table_wei_bio_clean <-
      tidytable::left_join(df,
        freq_cla_kin,
        by = stats::setNames(
          "feature_source",
          "feature_id"
        )
      ) |>
      tidytable::left_join(freq_npc_pat,
        by = stats::setNames(
          "feature_source",
          "feature_id"
        )
      ) |>
      tidytable::left_join(freq_cla_sup,
        by = stats::setNames(
          "feature_source",
          "feature_id"
        )
      ) |>
      tidytable::left_join(freq_npc_sup,
        by = stats::setNames(
          "feature_source",
          "feature_id"
        )
      ) |>
      tidytable::left_join(freq_cla_cla,
        by = stats::setNames(
          "feature_source",
          "feature_id"
        )
      ) |>
      tidytable::left_join(freq_npc_cla,
        by = stats::setNames(
          "feature_source",
          "feature_id"
        )
      ) |>
      tidytable::left_join(freq_cla_par,
        by = stats::setNames(
          "feature_source",
          "feature_id"
        )
      ) |>
      tidytable::select(
        feature_id,
        tidytable::everything()
      ) |>
      ## In case there are no consensus at all because no network
      tidytable::mutate(tidytable::across(
        .cols = tidytable::where(is.logical),
        .fns = as.character
      )) |>
      log_pipe("adding dummy consistency for features
              with less than 2 neighbors \n") |>
      tidytable::mutate(
        consensus_structure_cla_kin =
          tidytable::coalesce(
            consensus_structure_cla_kin,
            "dummy"
          ),
        consistency_structure_cla_kin =
          tidytable::coalesce(
            consistency_structure_cla_kin,
            1
          ),
        consistency_score_chemical_1_cla_kingdom =
          tidytable::coalesce(
            consistency_score_chemical_1_cla_kingdom,
            0
          ),
        consensus_structure_npc_pat =
          tidytable::coalesce(
            consensus_structure_npc_pat,
            "dummy"
          ),
        consistency_structure_npc_pat =
          tidytable::coalesce(
            consistency_structure_npc_pat,
            1
          ),
        consistency_score_chemical_1_npc_pathway =
          tidytable::coalesce(
            consistency_score_chemical_1_npc_pathway,
            0
          ),
        consensus_structure_cla_sup =
          tidytable::coalesce(
            consensus_structure_cla_sup,
            "dummy"
          ),
        consistency_structure_cla_sup =
          tidytable::coalesce(
            consistency_structure_cla_sup,
            1
          ),
        consistency_score_chemical_2_cla_superclass =
          tidytable::coalesce(
            consistency_score_chemical_2_cla_superclass,
            0
          ),
        consensus_structure_npc_sup =
          tidytable::coalesce(
            consensus_structure_npc_sup,
            "dummy"
          ),
        consistency_structure_npc_sup =
          tidytable::coalesce(
            consistency_structure_npc_sup,
            1
          ),
        consistency_score_chemical_2_npc_superclass =
          tidytable::coalesce(
            consistency_score_chemical_2_npc_superclass,
            0
          ),
        consensus_structure_cla_cla =
          tidytable::coalesce(
            consensus_structure_cla_cla,
            "dummy"
          ),
        consistency_structure_cla_cla =
          tidytable::coalesce(
            consistency_structure_cla_cla,
            1
          ),
        consistency_score_chemical_3_cla_class =
          tidytable::coalesce(
            consistency_score_chemical_3_cla_class,
            0
          ),
        consensus_structure_npc_cla =
          tidytable::coalesce(
            consensus_structure_npc_cla,
            "dummy"
          ),
        consistency_structure_npc_cla =
          tidytable::coalesce(
            consistency_structure_npc_cla,
            1
          ),
        consistency_score_chemical_3_npc_class =
          tidytable::coalesce(
            consistency_score_chemical_3_npc_class,
            0
          ),
        consensus_structure_cla_par =
          tidytable::coalesce(
            consensus_structure_cla_par,
            "dummy"
          ),
        consistency_structure_cla_par =
          tidytable::coalesce(
            consistency_structure_cla_par,
            1
          ),
        consistency_score_chemical_4_cla_parent =
          tidytable::coalesce(
            consistency_score_chemical_4_cla_parent,
            0
          )
      )

    return(annot_table_wei_bio_clean)
  }
