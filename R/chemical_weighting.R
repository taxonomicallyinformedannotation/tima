if (!require(dplyr)) {
  install.packages("dplyr")
  require(
    package = "dplyr",
    quietly = TRUE,
    warn.conflicts = FALSE
  )
}
if (!require(stringr)) {
  install.packages("stringr")
  require(package = "stringr", quietly = TRUE)
}

#' Title
#'
#' @param annotationTableWeightedBioCleaned TODO
#' @param weightSpectral TODO
#' @param weightBiological TODO
#' @param weightChemical TODO
#' @param scoreChemicalPathway TODO
#' @param scoreChemicalSuperclass TODO
#' @param scoreChemicalClass TODO
#'
#' @return TODO
#' @export
#'
#' @examples
chemical_weighting <-
  function(annotationTableWeightedBioCleaned = annotation_table_weighted_bio_cleaned,
           weightSpectral = weight_spectral,
           weightBiological = weight_biological,
           weightChemical = weight_chemical,
           scoreChemicalPathway = score_chemical_pathway,
           scoreChemicalSuperclass = score_chemical_superclass,
           scoreChemicalClass = score_chemical_class) {
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
      dplyr::mutate(score_chemical = scoreChemicalPathway)

    cat("... superclass \n")
    step_sup <- step_pat |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_structure_2_superclass, string = consensus_structure_sup)
      ) |>
      dplyr::mutate(score_chemical = scoreChemicalSuperclass)

    cat("... class \n")
    step_cla <- step_sup |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_structure_3_class, string = consensus_structure_cla)
      ) |>
      dplyr::mutate(score_chemical = scoreChemicalClass)

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
        candidate_structure_1_pathway,
        candidate_structure_2_superclass,
        candidate_structure_3_class,
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
        (1 / (weightChemical + weightBiological + weightSpectral)) *
          weightChemical *
          score_chemical +
          (1 / (weightChemical + weightBiological + weightSpectral)) *
            weightBiological *
            score_biological +
          (1 / (weightChemical + weightBiological + weightSpectral)) *
            weightSpectral *
            score_initialNormalized
      )) |>
      dplyr::group_by(feature_id) |>
      dplyr::arrange(dplyr::desc(score_chemical)) |>
      dplyr::arrange(dplyr::desc(score_pondered_chemo)) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        smiles_2D,
        candidate_structure_1_pathway,
        candidate_structure_2_superclass,
        candidate_structure_3_class,
        .keep_all = TRUE
      ) |>
      dplyr::mutate(
        rank_initial = (dplyr::dense_rank(-as.numeric(
          score_initialNormalized
        ))),
        rank_final = (dplyr::dense_rank(-score_pondered_chemo))
      ) |>
      dplyr::arrange(rank_final) |>
      dplyr::ungroup() |>
      dplyr::arrange(as.numeric(feature_id)) |>
      data.frame()

    return(df4)
  }
