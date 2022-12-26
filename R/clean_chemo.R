#' @title Clean chemo
#'
#' @description TODO
#'
#' @param annotationTableWeightedChemo Table containing your chemically weighted annotation
#' @param structureOrganismPairsTable Table containing the structure - organism pairs
#' @param candidatesFinal Number of final candidates to keep
#' @param minimalMs1Bio Minimal biological score to keep MS1 based annotation
#' @param minimalMs1Chemo Minimal chemical score to keep MS1 based annotation
#'
#' @return A table containing the chemically weighted annotation where only a given number of initial candidates are kept
#'
#' @export
#'
#' @importFrom dplyr across arrange bind_rows dense_rank desc distinct
#' @importFrom dplyr everything filter group_by left_join mutate mutate_all
#' @importFrom dplyr na_if select summarise ungroup
#'
#' @examples NULL
clean_chemo <-
  function(annotationTableWeightedChemo = annotation_table_weighted_chemo,
           structureOrganismPairsTable = structure_organism_pairs_table,
           candidatesFinal = candidates_final,
           minimalMs1Bio = minimal_ms1_bio,
           minimalMs1Chemo = minimal_ms1_chemo) {
    if (annotate == TRUE) {
      cat(
        "filtering top ",
        candidatesFinal,
        " candidates and keeping only MS1 candidates with minimum \n",
        minimalMs1Bio,
        " biological score or \n",
        minimalMs1Chemo,
        "chemical score \n"
      )
    }
    df1 <- annotationTableWeightedChemo |>
      dplyr::filter(
        score_initialNormalized > 0 |
          # Those lines are to keep ms1 annotation
          score_biological >= minimalMs1Bio |
          # Only if a good biological
          score_chemical >= minimalMs1Chemo
        # Or chemical consistency score is obtained
      ) |>
      dplyr::group_by(feature_id) |>
      dplyr::distinct(inchikey_2D,
        .keep_all = TRUE
      ) |>
      dplyr::mutate(rank_final = (dplyr::dense_rank(-score_pondered_chemo))) |>
      dplyr::filter(rank_final <= candidatesFinal) |>
      dplyr::ungroup()

    df11 <- metadata_table_spectral_annotation
    if (exists(x = "annotation_table_ms1")) {
      df11 <- annotation_table_ms1
    }

    df12 <- df11 |>
      dplyr::distinct(
        feature_id,
        component_id,
        rt,
        mz,
        molecular_formula,
        inchikey_2D,
        smiles_2D,
        library,
        mz_error
      )

    cat("adding initial metadata (RT, etc.) and simplifying columns \n")
    df2 <- dplyr::left_join(df1, df12) |>
      dplyr::mutate(
        best_candidate_structure = paste(
          candidate_structure_1_pathway,
          candidate_structure_2_superclass,
          candidate_structure_3_class,
          sep = "\u00a7"
        )
      ) |>
      dplyr::distinct(
        feature_id,
        component_id,
        rt,
        mz,
        molecular_formula,
        inchikey_2D,
        smiles_2D,
        library,
        mz_error,
        rank_initial,
        rank_final,
        score_initialNormalized,
        score_biological,
        score_interim = score_pondered_bio,
        score_chemical,
        score_final = score_pondered_chemo,
        lowest_candidate_organism = organism_name,
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
      dplyr::arrange(reference_doi) |>
      dplyr::distinct(
        inchikey_2D = structure_inchikey_2D,
        structure_name = structure_nameTraditional,
        structure_xlogp,
        structure_01pathway = structure_taxonomy_npclassifier_01pathway,
        structure_02superclass = structure_taxonomy_npclassifier_02superclass,
        structure_03class = structure_taxonomy_npclassifier_03class,
        lowest_candidate_organism = organism_name,
        reference_doi
      )

    cat("adding structures metadata \n")
    df4a <- dplyr::left_join(df2, df3) |>
      dplyr::select(
        feature_id,
        component_id,
        rt,
        mz,
        molecular_formula,
        inchikey_2D,
        smiles_2D,
        structure_name,
        structure_xlogp,
        structure_01pathway,
        structure_02superclass,
        structure_03class,
        score_initialNormalized,
        score_biological,
        score_chemical,
        library,
        mz_error,
        score_interim,
        score_final,
        reference_doi,
        rank_initial,
        rank_final,
        best_candidate_organism,
        best_candidate_structure,
        lowest_candidate_organism,
        dplyr::everything()
      )

    cat("selecting columns for cytoscape output \n")
    df4b <- df4a |>
      dplyr::select(
        -component_id,
        -molecular_formula,
        -inchikey_2D,
        -smiles_2D,
        -score_initialNormalized,
        -score_biological,
        -rank_initial,
        -rank_final,
        -best_candidate_structure,
        -best_candidate_organism,
        -lowest_candidate_organism,
        -score_chemical,
        -rt,
        -mz,
        -library,
        -mz_error,
        -score_interim,
        -score_final,
        -structure_name,
        -structure_xlogp,
        -structure_01pathway,
        -structure_02superclass,
        -structure_03class,
        -reference_doi
      ) |>
      dplyr::distinct()

    cat("summarizing results \n")
    df5a <- df4a |>
      dplyr::group_by(dplyr::across(
        c(
          -reference_doi,
          -structure_name,
          -structure_01pathway,
          -structure_02superclass,
          -structure_03class
        )
      )) |>
      dplyr::summarise(dplyr::across(
        c(
          reference_doi,
          structure_name,
          structure_01pathway,
          structure_02superclass,
          structure_03class
        ),
        ~ gsub(
          pattern = "\\bNA\\b",
          replacement = "",
          x = paste(.x, collapse = "$")
        )
      )) |>
      dplyr::group_by(feature_id) |>
      dplyr::arrange(dplyr::desc(score_final)) |>
      dplyr::summarise(dplyr::across(
        colnames(df4a)[3:25],
        ~ gsub(
          pattern = "\\bNA\\b",
          replacement = "",
          x = paste(.x, collapse = "|")
        )
      )) |>
      dplyr::select(
        -rt,
        -mz
      )

    df5b <- dplyr::left_join(df5a, df4b) |>
      dplyr::distinct()

    if (!any(names(metadata_table_spectral_annotation) == "rt")) {
      df6 <- metadata_table_spectral_annotation |>
        dplyr::distinct(
          feature_id,
          component_id,
          mz
        )
    } else {
      df6 <- metadata_table_spectral_annotation |>
        dplyr::distinct(
          feature_id,
          component_id,
          mz,
          rt
        )
    }

    if (!any(names(metadata_table_spectral_annotation) == "rt")) {
      df7 <- dplyr::left_join(df6, df5b) |>
        dplyr::arrange(feature_id) |>
        dplyr::mutate_all(dplyr::na_if, "") |>
        dplyr::select(
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
          best_candidate_organism,
          best_candidate_structure,
          consensus_structure_pat,
          consistency_structure_pat,
          consensus_structure_sup,
          consistency_structure_sup,
          consensus_structure_cla,
          consistency_structure_cla,
          reference_doi
        )

      cat("adding consensus again to droped candidates \n")
      df8 <- df7 |>
        dplyr::filter(!is.na(inchikey_2D)) |>
        dplyr::mutate(
          feature_id = as.numeric(feature_id),
          component_id = as.numeric(component_id),
          mz = as.numeric(mz)
        )

      df9 <- df7 |>
        dplyr::filter(is.na(inchikey_2D)) |>
        dplyr::select(
          feature_id,
          component_id,
          mz
        ) |>
        dplyr::mutate_all(as.numeric)

      df10 <- dplyr::left_join(
        df9,
        annotationTableWeightedChemo
      ) |>
        dplyr::select(
          feature_id,
          component_id,
          mz,
          consensus_structure_pat,
          consistency_structure_pat,
          consensus_structure_sup,
          consistency_structure_sup,
          consensus_structure_cla,
          consistency_structure_cla
        ) |>
        dplyr::distinct()
    } else {
      df7 <- dplyr::left_join(df6, df5b) |>
        dplyr::arrange(feature_id) |>
        dplyr::mutate_all(dplyr::na_if, "") |>
        dplyr::select(
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
          consistency_structure_cla,
          reference_doi
        )

      df8 <- df7 |>
        dplyr::filter(!is.na(inchikey_2D)) |>
        dplyr::mutate(
          feature_id = as.numeric(feature_id),
          component_id = as.numeric(component_id),
          mz = as.numeric(mz),
          rt = as.numeric(rt)
        )

      df9 <- df7 |>
        dplyr::filter(is.na(inchikey_2D)) |>
        dplyr::select(
          feature_id,
          component_id,
          rt,
          mz
        ) |>
        dplyr::mutate_all(as.numeric)

      df10 <- dplyr::left_join(
        df9,
        annotationTableWeightedChemo
      ) |>
        dplyr::select(
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
        dplyr::distinct()
    }

    df11 <- dplyr::bind_rows(df8, df10) |>
      dplyr::arrange(as.numeric(feature_id))

    ## Because cytoscape import fails otherwise
    colnames(df11) <-
      stringr::str_remove(
        string = colnames(df11),
        pattern = stringr::fixed(pattern = "_structure")
      )

    return(df11)
  }
