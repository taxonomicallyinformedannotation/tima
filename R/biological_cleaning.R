# require(...)

#' Title
#'
#' @noRd
#'
#' @param annotationTableWeightedBio TODO
#' @param structureOrganismPairsTable TODO
#' @param edgesTable TODO
#' @param aNnOtAtE TODO
#' @param minimalMs1Bio TODO
#'
#' @return TODO
#' @export
#'
#' @examples
biological_cleaning <-
  function(annotationTableWeightedBio = annotation_table_weighted_bio,
           structureOrganismPairsTable = structure_organism_pairs_table,
           edgesTable = edges_table,
           aNnOtAtE = annotate,
           candidatesInitial = candidates_initial,
           minimalMs1Bio = minimal_ms1_bio) {
    if (aNnOtAtE == TRUE) {
      cat(
        "keeping only MS1 candidates with minimum \n",
        minimalMs1Bio,
        " biological score \n"
      )
    }
    df01 <- annotationTableWeightedBio |>
      dplyr::filter(score_initialNormalized > 0 |
        # those lines are to keep ms1 annotation
        score_biological >= minimalMs1Bio) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        .keep_all = TRUE
      )

    cat("erasing other MS1 candidates \n")
    df02 <-
      dplyr::anti_join(
        annotationTableWeightedBio |>
          dplyr::distinct(feature_id,
            rank_initial,
            rank_final,
            .keep_all = TRUE
          ),
        df01
      ) |>
      dplyr::mutate(inchikey_2D = "notAnnotated")

    df03 <- dplyr::bind_rows(df01, df02)

    df <- df03 |>
      dplyr::distinct(
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
    df1 <- df |>
      dplyr::filter(component_id != -1) |>
      dplyr::group_by(component_id) |>
      dplyr::distinct(feature_id, inchikey_2D, .keep_all = TRUE) |>
      dplyr::add_count() |>
      dplyr::filter(n >= 3) |>
      dplyr::select(-n)

    cat("keeping clusters with less than 3 features \n")
    df2 <- dplyr::full_join(
      x = df |>
        dplyr::filter(component_id == -1),
      y = df |>
        dplyr::group_by(component_id) |>
        dplyr::distinct(feature_id, .keep_all = TRUE) |>
        dplyr::add_count() |>
        dplyr::filter(n <= 2) |>
        dplyr::select(-n)
    )

    cat("calculating chemical consistency features with at least 2 neighbors ... \n")

    cat("... among edges ... \n")
    df3 <-
      dplyr::right_join(edgesTable,
        df1,
        by = setNames("feature_id", "feature_target")
      ) |>
      dplyr::filter(!is.na(feature_source))

    cat("... at the pathway level \n")
    freq_pat <- df3 |>
      dplyr::group_by(
        feature_source,
        candidate_structure_1_pathway
      ) |>
      dplyr::mutate(count_pat = dplyr::n_distinct(feature_target)) |>
      dplyr::ungroup() |>
      dplyr::group_by(feature_source) |>
      dplyr::mutate(sum = dplyr::n_distinct(feature_target)) |>
      dplyr::mutate(consistency_structure_pat = count_pat / sum) |>
      dplyr::ungroup() |>
      dplyr::group_by(
        feature_target,
        candidate_structure_1_pathway
      ) |>
      dplyr::mutate(rank_final = as.numeric(rank_final)) |>
      dplyr::arrange(rank_final) |>
      dplyr::distinct(feature_source,
        candidate_structure_1_pathway,
        .keep_all = TRUE
      ) |>
      dplyr::group_by(
        feature_source,
        candidate_structure_1_pathway
      ) |>
      dplyr::mutate(
        rank_avg_pat = ifelse(
          test = candidate_structure_1_pathway == "notAnnotated" |
            candidate_structure_1_pathway == "notClassified",
          yes = candidatesInitial / 2,
          no = mean(as.numeric(rank_final))
        )
      ) |>
      dplyr::mutate(consistency_score_chemical_1_pathway = consistency_structure_pat / sqrt(rank_avg_pat)) |>
      dplyr::group_by(feature_source) |>
      dplyr::arrange(-consistency_score_chemical_1_pathway) |>
      dplyr::ungroup() |>
      dplyr::distinct(feature_source, .keep_all = TRUE) |>
      dplyr::select(
        feature_source,
        consensus_structure_pat = candidate_structure_1_pathway,
        consistency_structure_pat,
        consistency_score_chemical_1_pathway
      ) |>
      dplyr::mutate(
        consensus_structure_pat = ifelse(
          test = consistency_score_chemical_1_pathway > 0.5,
          yes = consensus_structure_pat,
          no = "notConsistent"
        )
      )

    cat("... at the superclass level \n")
    freq_sup <- df3 |>
      dplyr::group_by(
        feature_source,
        candidate_structure_2_superclass
      ) |>
      dplyr::mutate(count_sup = dplyr::n_distinct(feature_target)) |>
      dplyr::ungroup() |>
      dplyr::group_by(feature_source) |>
      dplyr::mutate(sum = dplyr::n_distinct(feature_target)) |>
      dplyr::mutate(consistency_structure_sup = count_sup / sum) |>
      dplyr::ungroup() |>
      dplyr::group_by(
        feature_target,
        candidate_structure_2_superclass
      ) |>
      dplyr::mutate(rank_final = as.numeric(rank_final)) |>
      dplyr::arrange(rank_final) |>
      dplyr::distinct(feature_source,
        candidate_structure_2_superclass,
        .keep_all = TRUE
      ) |>
      dplyr::group_by(
        feature_source,
        candidate_structure_2_superclass
      ) |>
      dplyr::mutate(
        rank_avg_sup = ifelse(
          test = candidate_structure_2_superclass == "notAnnotated" |
            candidate_structure_2_superclass == "notClassified",
          yes = candidatesInitial / 2,
          no = mean(as.numeric(rank_final))
        )
      ) |>
      dplyr::mutate(consistency_score_chemical_2_superclass = consistency_structure_sup / sqrt(rank_avg_sup)) |>
      dplyr::group_by(feature_source) |>
      dplyr::arrange(-consistency_score_chemical_2_superclass) |>
      dplyr::ungroup() |>
      dplyr::distinct(feature_source, .keep_all = TRUE) |>
      dplyr::select(
        feature_source,
        consensus_structure_sup = candidate_structure_2_superclass,
        consistency_structure_sup,
        consistency_score_chemical_2_superclass
      ) |>
      dplyr::mutate(
        consensus_structure_sup = ifelse(
          test = consistency_score_chemical_2_superclass > 0.5,
          yes = consensus_structure_sup,
          no = "notConsistent"
        )
      )

    cat("... at the class level \n")
    freq_cla <- df3 |>
      dplyr::group_by(
        feature_source,
        candidate_structure_3_class
      ) |>
      dplyr::mutate(count_cla = dplyr::n_distinct(feature_target)) |>
      dplyr::ungroup() |>
      dplyr::group_by(feature_source) |>
      dplyr::mutate(sum = dplyr::n_distinct(feature_target)) |>
      dplyr::mutate(consistency_structure_cla = count_cla / sum) |>
      dplyr::ungroup() |>
      dplyr::group_by(
        feature_target,
        candidate_structure_3_class
      ) |>
      dplyr::mutate(rank_final = as.numeric(rank_final)) |>
      dplyr::arrange(rank_final) |>
      dplyr::distinct(feature_source,
        candidate_structure_3_class,
        .keep_all = TRUE
      ) |>
      dplyr::group_by(
        feature_source,
        candidate_structure_3_class
      ) |>
      dplyr::mutate(
        rank_avg_cla = ifelse(
          test = candidate_structure_3_class == "notAnnotated" |
            candidate_structure_3_class == "notClassified",
          yes = candidatesInitial / 2,
          no = mean(as.numeric(rank_final))
        )
      ) |>
      dplyr::mutate(consistency_score_chemical_3_class = consistency_structure_cla / sqrt(rank_avg_cla)) |>
      dplyr::group_by(feature_source) |>
      dplyr::arrange(-consistency_score_chemical_3_class) |>
      dplyr::ungroup() |>
      dplyr::distinct(feature_source, .keep_all = TRUE) |>
      dplyr::select(
        feature_source,
        consensus_structure_cla = candidate_structure_3_class,
        consistency_structure_cla,
        consistency_score_chemical_3_class
      ) |>
      dplyr::mutate(
        consensus_structure_cla = ifelse(
          test = consistency_score_chemical_3_class > 0.5,
          yes = consensus_structure_cla,
          no = "notConsistent"
        )
      )

    cat("joining all except -1 together \n")
    df4 <-
      dplyr::left_join(df1,
        freq_pat,
        by = setNames("feature_source", "feature_id")
      ) |>
      dplyr::left_join(freq_sup,
        by = setNames("feature_source", "feature_id")
      ) |>
      dplyr::left_join(freq_cla,
        by = setNames("feature_source", "feature_id")
      ) |>
      dplyr::mutate(component_id = as.numeric(component_id)) |>
      dplyr::select(
        feature_id,
        dplyr::everything()
      ) |>
      dplyr::tibble()

    # think about the score
    cat("adding dummy consistency for features with less than 2 neighbors \n")
    dummy_consistency <- df2 |>
      dplyr::mutate(
        consensus_structure_pat = "dummy",
        consistency_structure_pat = 1,
        consistency_score_chemical_1_pathway = 0,
        consensus_structure_sup = "dummy",
        consistency_structure_sup = 1,
        consistency_score_chemical_2_superclass = 0,
        consensus_structure_cla = "dummy",
        consistency_structure_cla = 1,
        consistency_score_chemical_3_class = 0
      ) |>
      dplyr::mutate(component_id = as.numeric(component_id))

    cat("binding results together \n")
    df5 <- dplyr::bind_rows(df4, dummy_consistency)

    return(df5)
  }
