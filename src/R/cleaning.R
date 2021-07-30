############################  Functions - cleaning  ###########################

# require(...)

#' Title
#'
#' @param annotationTableWeightedBio
#' @param structureOrganismPairsTable
#' @param edgesTable
#'
#' @return
#' @export
#'
#' @examples
biological_cleaning <-
  function(annotationTableWeightedBio = annotation_table_weighted_bio,
           structureOrganismPairsTable = structure_organism_pairs_table,
           edgesTable = edges_table) {
    if (params$ms$annotate == TRUE) {
      cat(
        "keeping only MS1 candidates with minimum \n",
        params$score$biological$order,
        " biological score \n"
      )
    }
    df01 <- annotationTableWeightedBio %>%
      filter(score_initialNormalized > 0 |
               # those lines are to keep ms1 annotation
               score_biological >= params$score$biological$order) %>%
      distinct(feature_id,
               inchikey_2D,
               .keep_all = TRUE
      )

    cat("erasing other MS1 candidates \n")
    df02 <-
      anti_join(
        annotationTableWeightedBio %>% distinct(feature_id,
                                                rank_initial,
                                                rank_final,
                                                .keep_all = TRUE
        ),
        df01
      ) %>%
        mutate(inchikey_2D = "notAnnotated")

    df03 <- bind_rows(df01, df02)

    df <- df03 %>%
      distinct(
        inchikey_2D,
        feature_id,
        component_id,
        candidate_structure_1_pathway = structure_taxonomy_npclassifier_01pathway,
        candidate_structure_2_superclass = structure_taxonomy_npclassifier_02superclass,
        candidate_structure_3_class = structure_taxonomy_npclassifier_03class,
        rank_final,
        .keep_all = TRUE
      )

    cat("adding \"notAnnotated\" \n")
    df$candidate_structure_1_pathway[df["inchikey_2D"] == "notAnnotated"] <-
      "notAnnotated"
    df$candidate_structure_2_superclass[df["inchikey_2D"] == "notAnnotated"] <-
      "notAnnotated"
    df$candidate_structure_3_class[df["inchikey_2D"] == "notAnnotated"] <-
      "notAnnotated"

    cat("adding \"notClassified\" \n")
    df[is.character(is.na(df))] <- "notClassified"

    cat("keeping clusters with at least 3 features  \n")
    df1 <- df %>%
      filter(component_id != -1) %>%
      group_by(component_id) %>%
      distinct(feature_id, inchikey_2D, .keep_all = TRUE) %>%
      add_count() %>%
      filter(n >= 3) %>%
      select(-n)

    cat("keeping clusters with less than 3 features \n")
    df2 <- full_join(
      x = df %>%
        filter(component_id == -1),
      y = df %>%
        group_by(component_id) %>%
        distinct(feature_id, .keep_all = TRUE) %>%
        add_count() %>%
        filter(n <= 2) %>%
        select(-n)
    )

    cat("calculating chemical consistency features with at least 2 neighbors ... \n")

    cat("... among edges... \n")
    df3 <-
      right_join(edgesTable,
                 df1,
                 by = setNames("feature_id", "feature_target")
      ) %>%
        filter(!is.na(feature_source))

    cat("... at the pathway level \n")
    freq_pat <- df3 %>%
      group_by(
        feature_source,
        candidate_structure_1_pathway
      ) %>%
      mutate(count_pat = n_distinct(feature_target)) %>%
      ungroup() %>%
      group_by(feature_source) %>%
      mutate(sum = n_distinct(feature_target)) %>%
      mutate(consistency_structure_pat = count_pat / sum) %>%
      ungroup() %>%
      group_by(
        feature_target,
        candidate_structure_1_pathway
      ) %>%
      mutate(rank_final = as.numeric(rank_final)) %>%
      arrange(rank_final) %>%
      distinct(feature_source,
               candidate_structure_1_pathway,
               .keep_all = TRUE
      ) %>%
      group_by(
        feature_source,
        candidate_structure_1_pathway
      ) %>%
      mutate(
        rank_avg_pat = ifelse(
          test = candidate_structure_1_pathway == "notAnnotated" |
            candidate_structure_1_pathway == "notClassified",
          yes = params$top_k$candidates$initial / 2,
          no = mean(as.numeric(rank_final))
        )
      ) %>%
      mutate(consistency_score_chemical_1_pathway = consistency_structure_pat / sqrt(rank_avg_pat)) %>%
      group_by(feature_source) %>%
      arrange(-consistency_score_chemical_1_pathway) %>%
      ungroup() %>%
      distinct(feature_source, .keep_all = TRUE) %>%
      select(
        feature_source,
        consensus_structure_pat = candidate_structure_1_pathway,
        consistency_structure_pat,
        consistency_score_chemical_1_pathway
      ) %>%
      mutate(
        consensus_structure_pat = ifelse(
          test = consistency_score_chemical_1_pathway > 0.5,
          yes = consensus_structure_pat,
          no = "notConsistent"
        )
      )

    cat("... at the superclass level \n")
    freq_sup <- df3 %>%
      group_by(
        feature_source,
        candidate_structure_2_superclass
      ) %>%
      mutate(count_sup = n_distinct(feature_target)) %>%
      ungroup() %>%
      group_by(feature_source) %>%
      mutate(sum = n_distinct(feature_target)) %>%
      mutate(consistency_structure_sup = count_sup / sum) %>%
      ungroup() %>%
      group_by(
        feature_target,
        candidate_structure_2_superclass
      ) %>%
      mutate(rank_final = as.numeric(rank_final)) %>%
      arrange(rank_final) %>%
      distinct(feature_source,
               candidate_structure_2_superclass,
               .keep_all = TRUE
      ) %>%
      group_by(
        feature_source,
        candidate_structure_2_superclass
      ) %>%
      mutate(
        rank_avg_sup = ifelse(
          test = candidate_structure_2_superclass == "notAnnotated" |
            candidate_structure_2_superclass == "notClassified",
          yes = params$top_k$candidates$initial / 2,
          no = mean(as.numeric(rank_final))
        )
      ) %>%
      mutate(consistency_score_chemical_2_superclass = consistency_structure_sup / sqrt(rank_avg_sup)) %>%
      group_by(feature_source) %>%
      arrange(-consistency_score_chemical_2_superclass) %>%
      ungroup() %>%
      distinct(feature_source, .keep_all = TRUE) %>%
      select(
        feature_source,
        consensus_structure_sup = candidate_structure_2_superclass,
        consistency_structure_sup,
        consistency_score_chemical_2_superclass
      ) %>%
      mutate(
        consensus_structure_sup = ifelse(
          test = consistency_score_chemical_2_superclass > 0.5,
          yes = consensus_structure_sup,
          no = "notConsistent"
        )
      )

    cat("... at the class level \n")
    freq_cla <- df3 %>%
      group_by(
        feature_source,
        candidate_structure_3_class
      ) %>%
      mutate(count_cla = n_distinct(feature_target)) %>%
      ungroup() %>%
      group_by(feature_source) %>%
      mutate(sum = n_distinct(feature_target)) %>%
      mutate(consistency_structure_cla = count_cla / sum) %>%
      ungroup() %>%
      group_by(
        feature_target,
        candidate_structure_3_class
      ) %>%
      mutate(rank_final = as.numeric(rank_final)) %>%
      arrange(rank_final) %>%
      distinct(feature_source,
               candidate_structure_3_class,
               .keep_all = TRUE
      ) %>%
      group_by(
        feature_source,
        candidate_structure_3_class
      ) %>%
      mutate(
        rank_avg_cla = ifelse(
          test = candidate_structure_3_class == "notAnnotated" |
            candidate_structure_3_class == "notClassified",
          yes = params$top_k$candidates$initial / 2,
          no = mean(as.numeric(rank_final))
        )
      ) %>%
      mutate(consistency_score_chemical_3_class = consistency_structure_cla / sqrt(rank_avg_cla)) %>%
      group_by(feature_source) %>%
      arrange(-consistency_score_chemical_3_class) %>%
      ungroup() %>%
      distinct(feature_source, .keep_all = TRUE) %>%
      select(
        feature_source,
        consensus_structure_cla = candidate_structure_3_class,
        consistency_structure_cla,
        consistency_score_chemical_3_class
      ) %>%
      mutate(
        consensus_structure_cla = ifelse(
          test = consistency_score_chemical_3_class > 0.5,
          yes = consensus_structure_cla,
          no = "notConsistent"
        )
      )

    cat("joining all except -1 together \n")
    df4 <-
      left_join(df1,
                freq_pat,
                by = setNames("feature_source", "feature_id")
      ) %>%
        left_join(.,
                  freq_sup,
                  by = setNames("feature_source", "feature_id")
        ) %>%
        left_join(.,
                  freq_cla,
                  by = setNames("feature_source", "feature_id")
        ) %>%
        mutate(component_id = as.numeric(component_id)) %>%
        select(
          feature_id,
          everything()
        ) %>%
        tibble()

    # think about the score
    cat("adding dummy consistency for features with less than 2 neighbors \n")
    dummy_consistency <- df2 %>%
      mutate(
        consensus_structure_pat = "dummy",
        consistency_structure_pat = 1,
        consistency_score_chemical_1_pathway = 0,
        consensus_structure_sup = "dummy",
        consistency_structure_sup = 1,
        consistency_score_chemical_2_superclass = 0,
        consensus_structure_cla = "dummy",
        consistency_structure_cla = 1,
        consistency_score_chemical_3_class = 0
      ) %>%
      mutate(component_id = as.numeric(component_id))

    cat("binding results together \n")
    df5 <- bind_rows(df4, dummy_consistency)

    return(df5)
  }

###############################################################################

#' Title
#'
#' @param annotationTableWeightedChemo
#' @param structureOrganismPairsTable
#'
#' @return
#' @export
#'
#' @examples
chemical_cleaning <-
  function(annotationTableWeightedChemo = annotation_table_weighted_chemo,
           structureOrganismPairsTable = structure_organism_pairs_table) {
    if (params$ms$annotate == TRUE) {
      cat(
        "filtering top ",
        params$top_k$final,
        " candidates and keeping only MS1 candidates with minimum \n",
        params$score$biological$order,
        " biological score or \n",
        params$score$chemical$superclass,
        "chemical score \n"
      )
    }
    df1 <- annotationTableWeightedChemo %>%
      filter(
        score_initialNormalized > 0 |
          # those lines are to keep ms1 annotation
          score_biological >= params$score$biological$order |
          # only if a good biological
          score_chemical >= params$score$chemical$superclass
        # or chemical consistency score is obtained
      ) %>%
      group_by(feature_id) %>%
      distinct(inchikey_2D,
               .keep_all = TRUE
      ) %>%
      mutate(rank_final = (dense_rank(-score_pondered_chemo))) %>%
      filter(rank_final <= params$top_k$final) %>%
      ungroup()

    df11 <- metadata_table_spectral_annotation
    if (exists(x = "annotation_table_ms1")) {
      df11 <- annotation_table_ms1
    }

    df12 <- df11 %>%
      mutate(across(rank_initial, as.numeric)) %>%
      mutate(component_id = as.numeric(component_id))

    cat("adding initial metadata (RT, etc.) and simplifying columns \n")
    df2 <- left_join(df1, df12) %>%
      mutate(
        best_candidate_structure = paste(
          candidate_structure_1_pathway,
          candidate_structure_2_superclass,
          candidate_structure_3_class,
          sep = "§"
        )
      ) %>%
      distinct(
        feature_id,
        component_id,
        rt,
        mz,
        inchikey_2D,
        library,
        mz_error,
        rank_initial,
        rank_final,
        score_initialNormalized,
        score_biological,
        score_interim = score_pondered_bio,
        score_chemical,
        score_final = score_pondered_chemo,
        best_candidate_organism = best_candidate,
        best_candidate_structure,
        consensus_structure_pat,
        consistency_structure_pat,
        consensus_structure_sup,
        consistency_structure_sup,
        consensus_structure_cla,
        consistency_structure_cla
      )

    df3 <- structureOrganismPairsTable %>%
      distinct(structure_inchikey_2D,
               .keep_all = TRUE
      ) %>%
      select(
        inchikey_2D = structure_inchikey_2D,
        smiles_2D = structure_smiles_2D,
        molecular_formula = structure_molecular_formula
      )

    cat("adding structures metaddata \n")
    df4a <- left_join(df2, df3) %>%
      select(
        feature_id,
        component_id,
        molecular_formula,
        inchikey_2D,
        smiles_2D,
        score_initialNormalized,
        score_biological,
        rank_initial,
        rank_final,
        best_candidate_organism,
        best_candidate_structure,
        score_chemical,
        rt,
        mz,
        library,
        mz_error,
        score_interim,
        score_final,
        everything()
      )

    cat("selecting columns for cytoscape output \n")
    df4b <- df4a %>%
      select(
        -component_id,
        -molecular_formula,
        -inchikey_2D,
        -smiles_2D,
        -score_initialNormalized,
        -score_biological,
        -rank_initial,
        -rank_final,
        -best_candidate_structure,
        -score_chemical,
        -rt,
        -mz,
        -library,
        -best_candidate_structure,
        -mz_error,
        -score_interim,
        -score_final
      ) %>%
      distinct()

    cat("summarizing results \n")
    df5a <- df4a %>%
      group_by(feature_id) %>%
      summarise(across(
        colnames(df4a)[3:18],
        ~gsub(
          pattern = "\\bNA\\b",
          replacement = "",
          x = paste(.x, collapse = "|")
        )
      )) %>%
      select(
        -rt,
        -mz
      )

    df5b <- left_join(df5a, df4b) %>%
      distinct()

    if (!any(names(metadata_table_spectral_annotation) == "rt")) {
      df6 <- metadata_table_spectral_annotation %>%
        distinct(
          feature_id,
          component_id,
          mz
        )
    }

    if (any(names(metadata_table_spectral_annotation) == "rt")) {
      df6 <- metadata_table_spectral_annotation %>%
        distinct(
          feature_id,
          component_id,
          mz,
          rt
        )
    }

    if (!any(names(metadata_table_spectral_annotation) == "rt")) {
      df7 <- left_join(df6, df5b) %>%
        arrange(feature_id) %>%
        mutate(across(
          everything(),
          ~y_as_na(x = .x, y = "")
        )) %>%
        select(
          feature_id,
          component_id,
          mz,
          molecular_formula,
          mz_error,
          library,
          smiles_2D,
          inchikey_2D,
          score_initialNormalized,
          score_biological,
          score_chemical,
          score_final,
          rank_initial,
          rank_final,
          candidate_organism_short_taxonomy,
          candidate_structure_short_taxonomy,
          consensus_structure_kin,
          consistency_structure_kin,
          consensus_structure_sup,
          consistency_structure_sup,
          consensus_structure_cla,
          consistency_structure_cla,
          consensus_structure_sub,
          consistency_structure_sub,
          consensus_structure_par,
          consistency_structure_par
        )

      cat("adding consensus again to droped candidates \n")
      df8 <- df7 %>%
        filter(!is.na(inchikey_2D))

      df9 <- df7 %>%
        filter(is.na(inchikey_2D)) %>%
        select(
          feature_id,
          component_id,
          mz
        )

      df10 <- left_join(
        df9,
        annotationTableWeightedChemo
      ) %>%
        select(
          feature_id,
          component_id,
          mz,
          consensus_structure_kin,
          consistency_structure_kin,
          consensus_structure_sup,
          consistency_structure_sup,
          consensus_structure_cla,
          consistency_structure_cla,
          consensus_structure_sub,
          consistency_structure_sub,
          consensus_structure_par,
          consistency_structure_par
        ) %>%
        distinct()
    }


    if (any(names(metadata_table_spectral_annotation) == "rt")) {
      df7 <- left_join(df6, df5b) %>%
        arrange(feature_id) %>%
        mutate(across(
          everything(),
          ~y_as_na(x = .x, y = "")
        )) %>%
        select(
          feature_id,
          component_id,
          rt,
          mz,
          molecular_formula,
          mz_error,
          library,
          smiles_2D,
          inchikey_2D,
          score_initialNormalized,
          score_biological,
          score_chemical,
          score_final,
          rank_initial,
          rank_final,
          best_candidate_organism,
          best_candidate_structure,
          consensus_structure_pat,
          consistency_structure_pat,
          consensus_structure_sup,
          consistency_structure_sup,
          consensus_structure_cla,
          consistency_structure_cla
        )

      df8 <- df7 %>%
        filter(!is.na(inchikey_2D)) %>%
        mutate(
          feature_id = as.numeric(feature_id),
          component_id = as.numeric(component_id),
          mz = as.numeric(mz),
          rt = as.numeric(rt)
        )

      df9 <- df7 %>%
        filter(is.na(inchikey_2D)) %>%
        select(
          feature_id,
          component_id,
          rt,
          mz
        ) %>%
        mutate_all(as.numeric)

      df10 <- left_join(
        df9,
        annotationTableWeightedChemo
      ) %>%
        select(
          feature_id,
          component_id,
          rt,
          mz,
          consensus_structure_pat,
          consistency_structure_pat,
          consensus_structure_sup,
          consistency_structure_sup,
          consensus_structure_cla,
          consistency_structure_cla
        ) %>%
        distinct()
    }

    df11 <- bind_rows(df8, df10) %>%
      arrange(as.numeric(feature_id))

    # because cytoscape import fails otherwise
    colnames(df11) <-
      gsub(
        pattern = "_structure",
        replacement = "",
        x = colnames(df11),
        fixed = TRUE
      )

    return(df11)
  }

###############################################################################
