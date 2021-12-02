# require(...)

#' Title
#'
#' @noRd
#'
#' @param annotationTable TODO
#'
#' @return TODO
#' @export
#'
#' @examples
non_ms1_annotation <-
  function(annotationTable = metadata_table_spectral_annotation) {
    cat("formatting \n")
    df15 <- annotationTable |>
      dplyr::mutate(dplyr::across(
        c(
          mz_error,
          component_id,
          mz,
          rt,
          score_input
        ),
        as.numeric
      )) |>
      dplyr::distinct()

    cat("ranking \n")
    df16 <- df15 |>
      dplyr::group_by(feature_id) |>
      dplyr::mutate(rank_initial = dplyr::dense_rank(dplyr::desc(score_input))) |>
      dplyr::ungroup() |>
      dplyr::filter(rank_initial <= params$top_k$initial) |>
      dplyr::select(
        -rt,
        -mz
      )

    if (!any(names(annotationTable) == "rt")) {
      annotationTable[, "rt"] <- 0
    }

    df17 <- annotationTable |>
      dplyr::select(
        feature_id,
        component_id,
        mz,
        rt,
      ) |>
      dplyr::distinct()

    cat("adding \"notAnnotated\" \n")
    df18 <- dplyr::left_join(df17, df16) |>
      dplyr::distinct() |>
      dplyr::mutate(dplyr::across(mz_error, as.numeric)) |>
      data.frame()

    df18["inchikey_2D"][is.na(df18["inchikey_2D"])] <-
      "notAnnotated"
    df18["score_input"][is.na(df18["score_input"])] <-
      0
    df18["library"][is.na(df18["library"])] <-
      "N/A"
    df18["mz_error"][is.na(df18["mz_error"])] <-
      666
    df18["rank_initial"][is.na(df18["rank_initial"])] <-
      params$top_k$initial

    df19 <- dplyr::left_join(
      df18,
      structure_organism_pairs_table |>
        dplyr::distinct(
          inchikey_2D = structure_inchikey_2D,
          smiles_2D = structure_smiles_2D,
          structure_taxonomy_npclassifier_01pathway,
          structure_taxonomy_npclassifier_02superclass,
          structure_taxonomy_npclassifier_03class
        )
    )

    return(df19)
  }
