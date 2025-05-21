#' @title Clean chemo
#'
#' @description This function cleans the results
#'    obtained after chemical weighting
#'
#' @include filter_high_confidence_only.R
#' @include minimize_results.R
#' @include summarize_results.R
#'
#' @param annot_table_wei_chemo Table containing your
#'    chemically weighted annotation
#' @param components_table Prepared components file
#' @param features_table Prepared features file
#' @param structure_organism_pairs_table Table containing the
#'    structure - organism pairs
#' @param candidates_final Number of final candidates to keep
#' @param minimal_ms1_bio Minimal biological score to keep MS1 based annotation
#' @param minimal_ms1_chemo Minimal chemical score to keep MS1 based annotation
#' @param minimal_ms1_condition Condition to be used. Must be "OR" or "AND".
#' @param compounds_names Report compounds names. Can be very large. BOOLEAN
#' @param high_confidence Report high confidence candidates only. BOOLEAN
#' @param remove_ties Remove ties. BOOLEAN
#' @param summarize Boolean. summarize results (1 row per feature)
#'
#' @return A table containing the chemically weighted annotation
#'    where only a given number of initial candidates are kept
#'
#' @seealso weight_chemo
#'
#' @examples NULL
clean_chemo <-
  function(
    annot_table_wei_chemo = get(
      "annot_table_wei_chemo",
      envir = parent.frame()
    ),
    components_table = get("components_table", envir = parent.frame()),
    features_table = get("features_table", envir = parent.frame()),
    structure_organism_pairs_table = get(
      "structure_organism_pairs_table",
      envir = parent.frame()
    ),
    candidates_final = get("candidates_final", envir = parent.frame()),
    minimal_ms1_bio = get("minimal_ms1_bio", envir = parent.frame()),
    minimal_ms1_chemo = get("minimal_ms1_chemo", envir = parent.frame()),
    minimal_ms1_condition = get(
      "minimal_ms1_condition",
      envir = parent.frame()
    ),
    compounds_names = get("compounds_names", envir = parent.frame()),
    high_confidence = get("high_confidence", envir = parent.frame()),
    remove_ties = get("remove_ties", envir = parent.frame()),
    summarize = get("summarize", envir = parent.frame())
  ) {
    logger::log_info(
      "Filtering top ",
      candidates_final,
      " candidates and keeping only MS1 candidates with minimum ",
      minimal_ms1_bio,
      " biological score ",
      minimal_ms1_condition,
      " ",
      minimal_ms1_chemo,
      " chemical score "
    )

    ## Those lines are to keep ms1 annotation
    ## Only if a good biological
    ## Or chemical consistency score is obtained
    if (minimal_ms1_condition == "OR") {
      df1 <- annot_table_wei_chemo |>
        tidytable::filter(
          (!is.na(candidate_score_similarity) |
            !is.na(candidate_score_sirius_csi)) |
            (score_biological >= minimal_ms1_bio |
              score_chemical >= minimal_ms1_chemo)
        )
    }
    if (minimal_ms1_condition == "AND") {
      df1 <- annot_table_wei_chemo |>
        tidytable::filter(
          (!is.na(candidate_score_similarity) |
            !is.na(candidate_score_sirius_csi)) |
            (score_biological >= minimal_ms1_bio &
              score_chemical >= minimal_ms1_chemo)
        )
    }

    df1 <- df1 |>
      tidytable::arrange(
        score_weighted_chemo |>
          tidytable::desc()
      ) |>
      tidytable::distinct(
        feature_id,
        candidate_structure_inchikey_connectivity_layer,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        rank_initial = tidytable::dense_rank(-candidate_score_pseudo_initial),
        rank_final = tidytable::dense_rank(-score_weighted_chemo),
        .by = c(feature_id)
      )

    results_mini <- df1 |>
      minimize_results(
        features_table = features_table
      )
    results_candidates <- results_mini |>
      tidytable::distinct(feature_id, candidates_evaluated, candidates_best)

    if (high_confidence) {
      df1 <- df1 |>
        filter_high_confidence_only()
    }

    if (compounds_names == FALSE) {
      df1 <- df1 |>
        tidytable::select(-candidate_structure_name)
    }

    df1_filtered <- df1 |>
      tidytable::filter(rank_final <= candidates_final)

    logger::log_trace("Processing full results")
    results_full <- df1 |>
      summarize_results(
        features_table = features_table,
        components_table = components_table,
        structure_organism_pairs_table = structure_organism_pairs_table,
        annot_table_wei_chemo = annot_table_wei_chemo,
        remove_ties = remove_ties,
        summarize = summarize
      )
    logger::log_trace("Processing filtered results")
    results_filtered <- df1_filtered |>
      summarize_results(
        features_table = features_table,
        components_table = components_table,
        structure_organism_pairs_table = structure_organism_pairs_table,
        annot_table_wei_chemo = annot_table_wei_chemo,
        remove_ties = remove_ties,
        summarize = summarize
      )
    if (
      results_candidates |>
        nrow() >
        0L
    ) {
      results_full <- results_full |>
        tidytable::left_join(results_candidates)
      results_filtered <- results_filtered |>
        tidytable::left_join(results_candidates)
    }
    rm(
      annot_table_wei_chemo,
      features_table,
      components_table,
      structure_organism_pairs_table
    )

    return(
      list(
        "full" = results_full,
        "filtered" = results_filtered,
        "mini" = results_mini
      )
    )
  }
