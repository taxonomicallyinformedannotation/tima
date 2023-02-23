#' @title Clean bio
#'
#' @description This function cleans the results obtained after biological weighting
#'
#' @param annotationTableWeightedBio Table containing your biologically weighted annotation
#' @param edgesTable Table containing the edges between features
#' @param aNnOtAtE Boolean parameter indicating if you performed MS1 annotation completion
#' @param candidatesInitial Number of initial candidates to keep
#' @param minimalMs1Bio Minimal biological score to keep MS1 based annotation
#'
#' @return A table containing the biologically weighted annotation where only a given number of initial candidates are kept
#'
#' @export
#'
#' @importFrom dplyr add_count anti_join arrange bind_rows distinct filter
#' @importFrom dplyr full_join group_by mutate n_distinct right_join select
#' @importFrom dplyr tibble ungroup
#' @importFrom stats setNames
#' @importFrom tidyselect where
#'
#' @seealso weight_bio
#'
#' @examples NULL
clean_bio <-
  function(annotationTableWeightedBio = annotation_table_weighted_bio,
           edgesTable = edges_table,
           aNnOtAtE = annotate,
           candidatesInitial = candidates_initial,
           minimalMs1Bio = minimal_ms1_bio) {
    if (aNnOtAtE == TRUE) {
      log_debug(
        "keeping only MS1 candidates with minimum \n",
        minimalMs1Bio,
        " biological score \n"
      )
    }
    df01 <- annotationTableWeightedBio |>
      dplyr::filter(score_initialNormalized > 0 |
        # Those lines are to keep ms1 annotation
        score_biological >= minimalMs1Bio)

    log_debug("erasing other MS1 candidates \n")
    df02 <-
      dplyr::anti_join(
        annotationTableWeightedBio |>
          dplyr::distinct(
            feature_id,
            structure_inchikey_2D,
            .keep_all = TRUE
          ),
        df01
      ) |>
      dplyr::mutate(structure_inchikey_2D = "notAnnotated")

    df03 <- dplyr::bind_rows(df01, df02)

    df <- df03 |>
      dplyr::distinct(
        structure_inchikey_2D,
        feature_id,
        component_id,
        candidate_structure_1_pathway = structure_taxonomy_npclassifier_01pathway,
        candidate_structure_2_superclass = structure_taxonomy_npclassifier_02superclass,
        candidate_structure_3_class = structure_taxonomy_npclassifier_03class,
        rank_final,
        .keep_all = TRUE
      )

    log_debug("adding \"notAnnotated\" \n")
    df$candidate_structure_1_pathway[df["structure_inchikey_2D"] == "notAnnotated"] <-
      "notAnnotated"
    df$candidate_structure_2_superclass[df["structure_inchikey_2D"] == "notAnnotated"] <-
      "notAnnotated"
    df$candidate_structure_3_class[df["structure_inchikey_2D"] == "notAnnotated"] <-
      "notAnnotated"

    log_debug("adding \"notClassified\" \n")
    df[is.character(is.na(df))] <- "notClassified"

    log_debug("keeping clusters with at least 3 features  \n")
    df1 <- df |>
      dplyr::filter(component_id != -1) |>
      dplyr::group_by(component_id) |>
      dplyr::distinct(feature_id,
        structure_inchikey_2D,
        .keep_all = TRUE
      ) |>
      dplyr::add_count() |>
      dplyr::ungroup() |>
      dplyr::filter(n >= 3) |>
      dplyr::select(-n)

    log_debug("keeping clusters with less than 3 features \n")
    df2 <- dplyr::full_join(
      x = df |>
        dplyr::filter(component_id == -1),
      y = df |>
        dplyr::group_by(component_id) |>
        dplyr::distinct(feature_id, .keep_all = TRUE) |>
        dplyr::add_count() |>
        dplyr::ungroup() |>
        dplyr::filter(n <= 2) |>
        dplyr::select(-n)
    )

    log_debug("calculating chemical consistency features with at least 2 neighbors ... \n")

    log_debug("... among edges ... \n")
    df3 <-
      dplyr::right_join(edgesTable,
        df1,
        by = stats::setNames("feature_id", "feature_target")
      ) |>
      dplyr::filter(!is.na(feature_source))

    log_debug("... at the pathway level \n")
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

    log_debug("... at the superclass level \n")
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

    log_debug("... at the class level \n")
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

    log_debug("joining all except -1 together \n")
    df4 <-
      dplyr::left_join(df1,
        freq_pat,
        by = stats::setNames("feature_source", "feature_id")
      ) |>
      dplyr::left_join(freq_sup,
        by = stats::setNames("feature_source", "feature_id")
      ) |>
      dplyr::left_join(freq_cla,
        by = stats::setNames("feature_source", "feature_id")
      ) |>
      dplyr::mutate(component_id = as.numeric(component_id)) |>
      dplyr::select(
        feature_id,
        dplyr::everything()
      ) |>
      # In case there are no consensus at all because no network
      dplyr::mutate(dplyr::across(tidyselect::where(is.logical), as.character)) |>
      dplyr::tibble()

    # Think about better scoring option
    log_debug("adding dummy consistency for features with less than 2 neighbors \n")
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

    log_debug("binding results together \n")
    df5 <- dplyr::bind_rows(df4, dummy_consistency)

    return(df5)
  }
