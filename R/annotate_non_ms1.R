#' @title Annotate non MS1
#'
#' @description This function filters MS2 results in case no MS1 annotation was performed
#'
#' @param annotationTable Table containing your previous annotation
#' @param candidatesInitial Number of initial candidates to keep
#'
#' @return A table containing the initial annotation where only part of initial candidates are kept
#'
#' @export
#'
#' @importFrom dplyr across dense_rank desc group_by distinct filter left_join
#' @importFrom dplyr mutate mutate_all select ungroup
#'
#' @seealso annotate_ms1
#'
#' @examples NULL
annotate_non_ms1 <-
  function(annotationTable = metadata_table_spectral_annotation,
           candidatesInitial = candidates_initial) {
    log_debug("formatting initial results \n")
    df23 <- annotationTable |>
      dplyr::mutate(dplyr::across(
        dplyr::any_of(
          c(
            "mz_error",
            "component_id",
            "mz",
            "rt",
            "score_input"
          )
        ),
        as.numeric
      )) |>
      dplyr::distinct()

    log_debug("ranking \n")
    df24 <- df23 |>
      dplyr::group_by(feature_id) |>
      dplyr::mutate(rank_initial = dplyr::dense_rank(dplyr::desc(score_input))) |>
      dplyr::ungroup() |>
      dplyr::filter(rank_initial <= candidatesInitial) |>
      dplyr::distinct(
        feature_id,
        component_id,
        score_input,
        library,
        mz_error,
        molecular_formula,
        inchikey_2D,
        smiles_2D,
        rank_initial
      )

    if (!any(names(annotationTable) == "rt")) {
      annotationTable[, "rt"] <- 0
    }

    df25 <- annotationTable |>
      dplyr::select(
        feature_id,
        component_id,
        mz,
        rt,
      ) |>
      dplyr::distinct() |>
      dplyr::mutate_all(as.numeric)

    log_debug("adding \"notAnnotated\" \n")
    df26 <- dplyr::left_join(df25, df24) |>
      dplyr::distinct() |>
      dplyr::mutate(dplyr::across(mz_error, as.numeric)) |>
      data.frame()

    df26["inchikey_2D"][is.na(df26["inchikey_2D"])] <-
      "notAnnotated"
    df26["score_input"][is.na(df26["score_input"])] <-
      0
    df26["library"][is.na(df26["library"])] <-
      "N/A"
    df26["mz_error"][is.na(df26["mz_error"])] <-
      666
    df26["rank_initial"][is.na(df26["rank_initial"])] <-
      candidatesInitial

    df27 <- dplyr::left_join(
      df26,
      structure_organism_pairs_table |>
        dplyr::distinct(
          inchikey_2D = structure_inchikey_2D,
          smiles_2D = structure_smiles_2D,
          structure_taxonomy_npclassifier_01pathway,
          structure_taxonomy_npclassifier_02superclass,
          structure_taxonomy_npclassifier_03class
        )
    )

    return(df27)
  }
