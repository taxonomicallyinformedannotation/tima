# require(...)

#' Title
#'
#' @noRd
#'
#' @param annotationTableWeightedChemo TODO
#' @param structureOrganismPairsTable TODO
#' @param candidatesFinal TODO
#' @param minimalMs1Bio TODO
#' @param minimalMs1Chemo TODO
#'
#' @return TODO
#' @export
#'
#' @examples
chemical_cleaning <-
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
          # those lines are to keep ms1 annotation
          score_biological >= minimalMs1Bio |
          # only if a good biological
          score_chemical >= minimalMs1Chemo
        # or chemical consistency score is obtained
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
      dplyr::mutate(dplyr::across(rank_initial, as.numeric)) |>
      dplyr::mutate(component_id = as.numeric(component_id))

    cat("adding initial metadata (RT, etc.) and simplifying columns \n")
    df2 <- dplyr::left_join(df1, df12) |>
      dplyr::mutate(
        best_candidate_structure = paste(
          candidate_structure_1_pathway,
          candidate_structure_2_superclass,
          candidate_structure_3_class,
          sep = "§"
        )
      ) |>
      dplyr::distinct(
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
      dplyr::distinct(structure_inchikey_2D,
        organism_name,
        reference_doi,
        .keep_all = TRUE
      ) |>
      dplyr::select(
        inchikey_2D = structure_inchikey_2D,
        smiles_2D = structure_smiles_2D,
        molecular_formula = structure_molecular_formula,
        lowest_candidate_organism = organism_name,
        reference_doi
      )

    cat("adding structures metadata \n")
    df4a <- dplyr::left_join(df2, df3) |>
      dplyr::select(
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
        reference_doi,
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
        -score_chemical,
        -rt,
        -mz,
        -library,
        -best_candidate_structure,
        -mz_error,
        -score_interim,
        -score_final
      ) |>
      dplyr::distinct()

    cat("summarizing results \n")
    df5a <- df4a |>
      dplyr::group_by(feature_id) |>
      dplyr::summarise(dplyr::across(
        colnames(df4a)[3:19],
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
    }

    if (any(names(metadata_table_spectral_annotation) == "rt")) {
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
        dplyr::mutate(dplyr::across(
          dplyr::everything(),
          ~ y_as_na(x = .x, y = "")
        )) |>
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
          consistency_structure_par,
          reference_doi
        )

      cat("adding consensus again to droped candidates \n")
      df8 <- df7 |>
        dplyr::filter(!is.na(inchikey_2D))

      df9 <- df7 |>
        dplyr::filter(is.na(inchikey_2D)) |>
        dplyr::select(
          feature_id,
          component_id,
          mz
        )

      df10 <- dplyr::left_join(
        df9,
        annotationTableWeightedChemo
      ) |>
        dplyr::select(
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
        dplyr::distinct()
    }


    if (any(names(metadata_table_spectral_annotation) == "rt")) {
      df7 <- dplyr::left_join(df6, df5b) |>
        dplyr::arrange(feature_id) |>
        dplyr::mutate(dplyr::across(
          dplyr::everything(),
          ~ y_as_na(x = .x, y = "")
        )) |>
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
