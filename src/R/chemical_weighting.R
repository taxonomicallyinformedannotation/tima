###########################   Functions - chemical    #########################

require(package = dplyr, quietly = TRUE)
require(package = stringr, quietly = TRUE)

#' Title
#'
#' @param annotationTableWeightedBioCleaned
#'
#' @return
#' @export
#'
#' @examples
chemical_weighting <-
  function(annotationTableWeightedBioCleaned = annotation_table_weighted_bio_cleaned) {
    cat("calculating chemical score ... \n")
    df1 <- annotationTableWeightedBioCleaned

    cat("... adding metadata \n")
    df2 <- df1 |>
      dplyr::distinct(
        feature_id,
        component_id,
        inchikey_2D,
        smiles_2D,
        candidate_structure_1_pathway,
        candidate_structure_2_superclass,
        candidate_structure_3_class,
        consensus_structure_pat,
        consistency_score_chemical_1_pathway,
        consensus_structure_sup,
        consistency_score_chemical_2_superclass,
        consensus_structure_cla,
        consistency_score_chemical_3_class
      )

    cat("... pathway \n")
    step_pat <- df2 |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_structure_1_pathway, string = consensus_structure_pat)
      ) |>
      dplyr::mutate(score_chemical = params$score$chemical$pathway)

    cat("... superclass \n")
    step_sup <- step_pat |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_structure_2_superclass, string = consensus_structure_sup)
      ) |>
      dplyr::mutate(score_chemical = params$score$chemical$superclass)

    cat("... class \n")
    step_cla <- step_sup |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_structure_3_class, string = consensus_structure_cla)
      ) |>
      dplyr::mutate(score_chemical = params$score$chemical$class)

    cat("... outputting best score \n")
    df3 <-
      dplyr::bind_rows(step_pat, step_sup, step_cla) |>
      dplyr::mutate(dplyr::across(where(is.logical), as.numeric)) |>
      dplyr::mutate(score_chemical = ifelse(
        test = is.na(score_chemical),
        yes = 0,
        no = score_chemical
      )) |>
      dplyr::select(
        feature_id,
        inchikey_2D,
        smiles_2D,
        score_chemical
      )

    cat("... joining \n")
    df4 <- dplyr::left_join(df1, df3) |>
      data.frame()

    df4$score_chemical[is.na(df4$score_chemical)] <- 0

    cat("... cleaning \n")
    df4 <- df4 |>
      dplyr::rowwise() |>
      dplyr::mutate(score_pondered_chemo = (
        params$weight$chemical * score_chemical + as.numeric(score_pondered_bio)
      )) |>
      dplyr::group_by(feature_id) |>
      dplyr::arrange(desc(score_chemical)) |>
      dplyr::arrange(desc(score_pondered_chemo)) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        .keep_all = TRUE
      ) |>
      dplyr::mutate(
        rank_initial = (dplyr::dense_rank(-as.numeric(
          score_initialNormalized
        ))),
        rank_final = (dplyr::dense_rank(-score_pondered_chemo))
      ) |>
      arrange(rank_final) |>
      ungroup() |>
      arrange(as.numeric(feature_id)) |>
      data.frame()

    return(df4)
  }

###############################################################################
