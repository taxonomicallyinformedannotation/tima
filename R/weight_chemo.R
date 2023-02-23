#' @title Weight chemo
#'
#' @description This function weights the biologically weigthed annotations according their chemical consistency
#'
#' @param annotationTableWeightedBioCleaned Table containing the biologically weighted annotation
#' @param weightSpectral Weight for the spectral score
#' @param weightBiological Weight for the biological score
#' @param weightChemical Weight for the chemical consistency score
#' @param scoreChemicalPathway Score for a `pathway` match (should be lower than `superclass`)
#' @param scoreChemicalSuperclass Score for a `superclass` match (should be lower than `class`)
#' @param scoreChemicalClass Score for a `class` match (should be the highest)
#'
#' @return A table containing the chemically weighted annotation
#'
#' @export
#'
#' @importFrom dplyr across arrange bind_rows dense_rank desc distinct filter
#' @importFrom dplyr group_by left_join mutate rowwise select ungroup where
#' @importFrom stringr str_detect
#'
#' @examples NULL
weight_chemo <-
  function(annotationTableWeightedBioCleaned = annotation_table_weighted_bio_cleaned,
           weightSpectral = weight_spectral,
           weightBiological = weight_biological,
           weightChemical = weight_chemical,
           scoreChemicalPathway = score_chemical_pathway,
           scoreChemicalSuperclass = score_chemical_superclass,
           scoreChemicalClass = score_chemical_class) {
    log_debug("calculating chemical score ... \n")
    df1 <- annotationTableWeightedBioCleaned

    log_debug("... adding metadata \n")
    df2 <- df1 |>
      dplyr::distinct(
        feature_id,
        component_id,
        structure_inchikey_2D,
        structure_smiles_2D,
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

    log_debug("... pathway \n")
    step_pat <- df2 |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_structure_1_pathway, string = consensus_structure_pat)
      ) |>
      dplyr::mutate(score_chemical = scoreChemicalPathway)

    log_debug("... superclass \n")
    step_sup <- step_pat |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_structure_2_superclass, string = consensus_structure_sup)
      ) |>
      dplyr::mutate(score_chemical = scoreChemicalSuperclass)

    log_debug("... class \n")
    step_cla <- step_sup |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_structure_3_class, string = consensus_structure_cla)
      ) |>
      dplyr::mutate(score_chemical = scoreChemicalClass)

    log_debug("... outputting best score \n")
    df3 <-
      dplyr::bind_rows(step_pat, step_sup, step_cla) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.logical), as.numeric)) |>
      dplyr::mutate(score_chemical = ifelse(
        test = is.na(score_chemical),
        yes = 0,
        no = score_chemical
      )) |>
      dplyr::select(
        feature_id,
        structure_inchikey_2D,
        structure_smiles_2D,
        candidate_structure_1_pathway,
        candidate_structure_2_superclass,
        candidate_structure_3_class,
        score_chemical
      )

    log_debug("... joining \n")
    df4 <- dplyr::left_join(df1, df3) |>
      data.frame()

    df4$score_chemical[is.na(df4$score_chemical)] <- 0

    log_debug("... cleaning \n")
    df4 <- df4 |>
      dplyr::rowwise() |>
      dplyr::mutate(
        score_pondered_chemo = (
          (1 / (
            weightChemical +
              weightBiological +
              weightSpectral
          )) *
            weightChemical *
            score_chemical +
            (1 / (
              weightChemical +
                weightBiological +
                weightSpectral
            )) *
              weightBiological *
              score_biological +
            (1 / (
              weightChemical +
                weightBiological +
                weightSpectral
            )) *
              weightSpectral *
              score_initialNormalized
        )
      ) |>
      dplyr::group_by(feature_id) |>
      dplyr::arrange(dplyr::desc(score_chemical)) |>
      dplyr::arrange(dplyr::desc(score_pondered_chemo)) |>
      dplyr::distinct(
        feature_id,
        structure_inchikey_2D,
        structure_smiles_2D,
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
