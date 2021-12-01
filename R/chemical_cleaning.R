# require(...)

#' Title
#'
#' @param annotationTableWeightedChemo TODO
#' @param structureOrganismPairsTable TODO
#'
#' @return TODO
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
    df1 <- annotationTableWeightedChemo |>
      filter(
        score_initialNormalized > 0 |
          # those lines are to keep ms1 annotation
          score_biological >= params$score$biological$order |
          # only if a good biological
          score_chemical >= params$score$chemical$superclass
        # or chemical consistency score is obtained
      ) |>
      group_by(feature_id) |>
      distinct(inchikey_2D,
        .keep_all = TRUE
      ) |>
      mutate(rank_final = (dense_rank(-score_pondered_chemo))) |>
      filter(rank_final <= params$top_k$final) |>
      ungroup()

    df11 <- metadata_table_spectral_annotation
    if (exists(x = "annotation_table_ms1")) {
      df11 <- annotation_table_ms1
    }

    df12 <- df11 |>
      mutate(across(rank_initial, as.numeric)) |>
      mutate(component_id = as.numeric(component_id))

    cat("adding initial metadata (RT, etc.) and simplifying columns \n")
    df2 <- left_join(df1, df12) |>
      mutate(
        best_candidate_structure = paste(
          candidate_structure_1_pathway,
          candidate_structure_2_superclass,
          candidate_structure_3_class,
          sep = "§"
        )
      ) |>
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

    df3 <- structureOrganismPairsTable |>
      distinct(structure_inchikey_2D,
        .keep_all = TRUE
      ) |>
      dplyr::select(
        inchikey_2D = structure_inchikey_2D,
        smiles_2D = structure_smiles_2D,
        molecular_formula = structure_molecular_formula
      )

    cat("adding structures metaddata \n")
    df4a <- left_join(df2, df3) |>
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
    df4b <- df4a |>
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
      ) |>
      distinct()

    cat("summarizing results \n")
    df5a <- df4a |>
      group_by(feature_id) |>
      summarise(across(
        colnames(df4a)[3:18],
        ~ gsub(
          pattern = "\\bNA\\b",
          replacement = "",
          x = paste(.x, collapse = "|")
        )
      )) |>
      select(
        -rt,
        -mz
      )

    df5b <- left_join(df5a, df4b) |>
      distinct()

    if (!any(names(metadata_table_spectral_annotation) == "rt")) {
      df6 <- metadata_table_spectral_annotation |>
        distinct(
          feature_id,
          component_id,
          mz
        )
    }

    if (any(names(metadata_table_spectral_annotation) == "rt")) {
      df6 <- metadata_table_spectral_annotation |>
        distinct(
          feature_id,
          component_id,
          mz,
          rt
        )
    }

    if (!any(names(metadata_table_spectral_annotation) == "rt")) {
      df7 <- left_join(df6, df5b) |>
        arrange(feature_id) |>
        mutate(across(
          everything(),
          ~ y_as_na(x = .x, y = "")
        )) |>
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
      df8 <- df7 |>
        filter(!is.na(inchikey_2D))

      df9 <- df7 |>
        filter(is.na(inchikey_2D)) |>
        select(
          feature_id,
          component_id,
          mz
        )

      df10 <- left_join(
        df9,
        annotationTableWeightedChemo
      ) |>
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
        ) |>
        distinct()
    }


    if (any(names(metadata_table_spectral_annotation) == "rt")) {
      df7 <- left_join(df6, df5b) |>
        arrange(feature_id) |>
        mutate(across(
          everything(),
          ~ y_as_na(x = .x, y = "")
        )) |>
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

      df8 <- df7 |>
        filter(!is.na(inchikey_2D)) |>
        mutate(
          feature_id = as.numeric(feature_id),
          component_id = as.numeric(component_id),
          mz = as.numeric(mz),
          rt = as.numeric(rt)
        )

      df9 <- df7 |>
        filter(is.na(inchikey_2D)) |>
        select(
          feature_id,
          component_id,
          rt,
          mz
        ) |>
        dplyr::mutate_all(as.numeric)

      df10 <- left_join(
        df9,
        annotationTableWeightedChemo
      ) |>
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
        ) |>
        distinct()
    }

    df11 <- bind_rows(df8, df10) |>
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
