#' @title Clean bio
#'
#' @description This function cleans the results
#'    obtained after biological weighting
#'
#' @param annot_table_wei_bio Table containing your
#'    biologically weighted annotation
#' @param edges_table Table containing the edges between features
#' @param minimal_consistency Minimal consistency score for a class. FLOAT
#'
#' @return A table containing the biologically weighted annotation
#'    where only a given number of initial candidates are kept
#'
#' @export
#'
#' @seealso weight_bio
#'
#' @examples NULL
clean_bio <-
  function(annot_table_wei_bio = get("annot_table_wei_bio",
             envir = parent.frame()
           ),
           edges_table = get("edges_table",
             envir = parent.frame()
           ),
           minimal_consistency = get("minimal_consistency",
             envir = parent.frame()
           )) {
    df <- annot_table_wei_bio |>
      tidytable::distinct(
        feature_id,
        candidate_structure_inchikey_no_stereo,
        candidate_structure_tax_cla_01kin,
        candidate_structure_tax_npc_01pat,
        candidate_structure_tax_cla_02sup,
        candidate_structure_tax_npc_02sup,
        candidate_structure_tax_cla_03cla,
        candidate_structure_tax_npc_03cla,
        candidate_structure_tax_cla_04dirpar,
        score_pondered_bio,
        .keep_all = TRUE
      )

    ## Loosing CANOPUS from SIRIUS
    ## TODO improve
    # log_debug("adding \"notAnnotated\" \n")
    # df$candidate_structure_1_cla_kingdom
    # [df["structure_inchikey_no_stereo"] == "notAnnotated"] <-
    #   "notAnnotated"
    # and so on ...

    # log_debug("keeping clusters with at least 3 features  \n")
    # df1 <- df |>
    #   tidytable::filter(component_id != -1) |>
    #   tidytable::group_by(component_id) |>
    #   tidytable::distinct(feature_id,
    #     structure_inchikey_no_stereo,
    #     .keep_all = TRUE
    #   ) |>
    #   tidytable::add_count() |>
    #   tidytable::ungroup() |>
    #   tidytable::filter(n >= 3) |>
    #   tidytable::select(-n)
    #
    # log_debug("keeping clusters with less than 3 features \n")
    # df2 <- tidytable::full_join(
    #   x = df |>
    #     tidytable::filter(component_id == -1),
    #   y = df |>
    #     tidytable::group_by(component_id) |>
    #     tidytable::distinct(feature_id, .keep_all = TRUE) |>
    #     tidytable::add_count() |>
    #     tidytable::ungroup() |>
    #     tidytable::filter(n <= 2) |>
    #     tidytable::select(-n)
    # )

    log_debug("calculating chemical consistency
              features with at least 2 neighbors ... \n")

    log_debug("... among edges ... \n")
    df3 <-
      tidytable::right_join(
        edges_table |>
          tidytable::group_by(feature_source) |>
          tidytable::add_count() |>
          tidytable::ungroup() |>
          tidytable::filter(n >= 2) |>
          tidytable::select(-n),
        df |>
          tidytable::distinct(
            feature_id,
            candidate_structure_tax_cla_01kin,
            candidate_structure_tax_npc_01pat,
            candidate_structure_tax_cla_02sup,
            candidate_structure_tax_npc_02sup,
            candidate_structure_tax_cla_03cla,
            candidate_structure_tax_npc_03cla,
            candidate_structure_tax_cla_04dirpar,
            score_pondered_bio
          ),
        by = stats::setNames("feature_id", "feature_target")
      ) |>
      tidytable::filter(!is.na(feature_source))

    log_debug("... at the (classyfire) kingdom level \n")
    freq_cla_kin <- df3 |>
      tidytable::distinct(
        feature_source,
        feature_target,
        candidate_structure_tax_cla_01kin,
        score_pondered_bio
      ) |>
      tidytable::mutate(
        count_kin = tidytable::n_distinct(feature_target),
        .by = c(
          feature_source,
          candidate_structure_tax_cla_01kin
        )
      ) |>
      tidytable::mutate(
        consistency_structure_cla_kin = count_kin /
          tidytable::n_distinct(feature_target),
        .by = c(feature_source)
      ) |>
      tidytable::distinct(feature_source,
        candidate_structure_tax_cla_01kin,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        feature_pred_tax_cla_01kin_score =
          consistency_structure_cla_kin * score_pondered_bio,
        .by = c(
          feature_source,
          candidate_structure_tax_cla_01kin
        )
      ) |>
      tidytable::arrange(-feature_pred_tax_cla_01kin_score) |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        feature_pred_tax_cla_01kin_val = candidate_structure_tax_cla_01kin,
        consistency_structure_cla_kin,
        feature_pred_tax_cla_01kin_score
      ) |>
      tidytable::mutate(
        feature_pred_tax_cla_01kin_val = ifelse(
          test = feature_pred_tax_cla_01kin_score >= minimal_consistency,
          yes = feature_pred_tax_cla_01kin_val,
          no = "notConsistent"
        )
      )

    log_debug("... at the (NPC) pathway level \n")
    freq_npc_pat <- df3 |>
      tidytable::distinct(
        feature_source,
        feature_target,
        candidate_structure_tax_npc_01pat,
        score_pondered_bio
      ) |>
      tidytable::mutate(
        count_pat = tidytable::n_distinct(feature_target),
        .by = c(
          feature_source,
          candidate_structure_tax_npc_01pat
        )
      ) |>
      tidytable::mutate(
        consistency_structure_npc_pat = count_pat /
          tidytable::n_distinct(feature_target),
        .by = c(feature_source)
      ) |>
      tidytable::distinct(feature_source,
        candidate_structure_tax_npc_01pat,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        feature_pred_tax_npc_01pat_score =
          consistency_structure_npc_pat * score_pondered_bio,
        .by = c(
          feature_source,
          candidate_structure_tax_npc_01pat
        )
      ) |>
      tidytable::arrange(-feature_pred_tax_npc_01pat_score) |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        feature_pred_tax_npc_01pat_val = candidate_structure_tax_npc_01pat,
        consistency_structure_npc_pat,
        feature_pred_tax_npc_01pat_score
      ) |>
      tidytable::mutate(
        feature_pred_tax_npc_01pat_val = ifelse(
          test = feature_pred_tax_npc_01pat_score >= minimal_consistency,
          yes = feature_pred_tax_npc_01pat_val,
          no = "notConsistent"
        )
      )

    log_debug("... at the (classyfire) superclass level \n")
    freq_cla_sup <- df3 |>
      tidytable::distinct(
        feature_source,
        feature_target,
        candidate_structure_tax_cla_02sup,
        score_pondered_bio
      ) |>
      tidytable::mutate(
        count_sup = tidytable::n_distinct(feature_target),
        .by = c(
          feature_source,
          candidate_structure_tax_cla_02sup
        )
      ) |>
      tidytable::mutate(
        consistency_structure_cla_sup = count_sup /
          tidytable::n_distinct(feature_target),
        .by = c(feature_source)
      ) |>
      tidytable::distinct(feature_source,
        candidate_structure_tax_cla_02sup,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        feature_pred_tax_cla_02sup_score =
          consistency_structure_cla_sup * score_pondered_bio,
        .by = c(
          feature_source,
          candidate_structure_tax_cla_02sup
        )
      ) |>
      tidytable::arrange(-feature_pred_tax_cla_02sup_score) |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        feature_pred_tax_cla_02sup_val = candidate_structure_tax_cla_02sup,
        consistency_structure_cla_sup,
        feature_pred_tax_cla_02sup_score
      ) |>
      tidytable::mutate(
        feature_pred_tax_cla_02sup_val = ifelse(
          test = feature_pred_tax_cla_02sup_score >= minimal_consistency,
          yes = feature_pred_tax_cla_02sup_val,
          no = "notConsistent"
        )
      )

    log_debug("... at the (NPC) superclass level \n")
    freq_npc_sup <- df3 |>
      tidytable::distinct(
        feature_source,
        feature_target,
        candidate_structure_tax_npc_02sup,
        score_pondered_bio
      ) |>
      tidytable::mutate(
        count_sup = tidytable::n_distinct(feature_target),
        .by = c(
          feature_source,
          candidate_structure_tax_npc_02sup
        )
      ) |>
      tidytable::mutate(
        consistency_structure_npc_sup = count_sup /
          tidytable::n_distinct(feature_target),
        .by = c(feature_source)
      ) |>
      tidytable::distinct(feature_source,
        candidate_structure_tax_npc_02sup,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        feature_pred_tax_npc_02sup_score =
          consistency_structure_npc_sup * score_pondered_bio,
        .by = c(
          feature_source,
          candidate_structure_tax_npc_02sup
        )
      ) |>
      tidytable::arrange(-feature_pred_tax_npc_02sup_score) |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        feature_pred_tax_npc_02sup_val = candidate_structure_tax_npc_02sup,
        consistency_structure_npc_sup,
        feature_pred_tax_npc_02sup_score
      ) |>
      tidytable::mutate(
        feature_pred_tax_npc_02sup_val = ifelse(
          test = feature_pred_tax_npc_02sup_score >= minimal_consistency,
          yes = feature_pred_tax_npc_02sup_val,
          no = "notConsistent"
        )
      )

    log_debug("... at the (classyfire) class level \n")
    freq_cla_cla <- df3 |>
      tidytable::distinct(
        feature_source,
        feature_target,
        candidate_structure_tax_cla_03cla,
        score_pondered_bio
      ) |>
      tidytable::mutate(
        count_cla = tidytable::n_distinct(feature_target),
        .by = c(
          feature_source,
          candidate_structure_tax_cla_03cla
        )
      ) |>
      tidytable::mutate(
        consistency_structure_cla_cla = count_cla /
          tidytable::n_distinct(feature_target),
        .by = c(feature_source)
      ) |>
      tidytable::distinct(feature_source,
        candidate_structure_tax_cla_03cla,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        feature_pred_tax_cla_03cla_score =
          consistency_structure_cla_cla * score_pondered_bio,
        .by = c(
          feature_source,
          candidate_structure_tax_cla_03cla
        )
      ) |>
      tidytable::arrange(-feature_pred_tax_cla_03cla_score) |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        feature_pred_tax_cla_03cla_val = candidate_structure_tax_cla_03cla,
        consistency_structure_cla_cla,
        feature_pred_tax_cla_03cla_score
      ) |>
      tidytable::mutate(
        feature_pred_tax_cla_03cla_val = ifelse(
          test = feature_pred_tax_cla_03cla_score >= minimal_consistency,
          yes = feature_pred_tax_cla_03cla_val,
          no = "notConsistent"
        )
      )

    log_debug("... at the (NPC) class level \n")
    freq_npc_cla <- df3 |>
      tidytable::distinct(
        feature_source,
        feature_target,
        candidate_structure_tax_npc_03cla,
        score_pondered_bio
      ) |>
      tidytable::mutate(
        count_cla = tidytable::n_distinct(feature_target),
        .by = c(
          feature_source,
          candidate_structure_tax_npc_03cla
        )
      ) |>
      tidytable::mutate(
        consistency_structure_npc_cla = count_cla /
          tidytable::n_distinct(feature_target),
        .by = c(feature_source)
      ) |>
      tidytable::distinct(feature_source,
        candidate_structure_tax_npc_03cla,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        feature_pred_tax_npc_03cla_score =
          consistency_structure_npc_cla * score_pondered_bio,
        .by = c(
          feature_source,
          candidate_structure_tax_npc_03cla
        )
      ) |>
      tidytable::arrange(-feature_pred_tax_npc_03cla_score) |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        feature_pred_tax_npc_03cla_val = candidate_structure_tax_npc_03cla,
        consistency_structure_npc_cla,
        feature_pred_tax_npc_03cla_score
      ) |>
      tidytable::mutate(
        feature_pred_tax_npc_03cla_val = ifelse(
          test = feature_pred_tax_npc_03cla_score >= minimal_consistency,
          yes = feature_pred_tax_npc_03cla_val,
          no = "notConsistent"
        )
      )

    log_debug("... at the (classyfire) parent level \n")
    freq_cla_par <- df3 |>
      tidytable::distinct(
        feature_source,
        feature_target,
        candidate_structure_tax_cla_04dirpar,
        score_pondered_bio
      ) |>
      tidytable::mutate(
        count_par = tidytable::n_distinct(feature_target),
        .by = c(
          feature_source,
          candidate_structure_tax_cla_04dirpar
        )
      ) |>
      tidytable::mutate(
        consistency_structure_cla_par = count_par /
          tidytable::n_distinct(feature_target),
        .by = c(feature_source)
      ) |>
      tidytable::distinct(feature_source,
        candidate_structure_tax_cla_04dirpar,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        feature_pred_tax_cla_04dirpar_score =
          consistency_structure_cla_par * score_pondered_bio,
        .by = c(
          feature_source,
          candidate_structure_tax_cla_04dirpar
        )
      ) |>
      tidytable::arrange(-feature_pred_tax_cla_04dirpar_score) |>
      tidytable::distinct(feature_source, .keep_all = TRUE) |>
      tidytable::select(
        feature_source,
        feature_pred_tax_cla_04dirpar_val = candidate_structure_tax_cla_04dirpar,
        consistency_structure_cla_par,
        feature_pred_tax_cla_04dirpar_score
      ) |>
      tidytable::mutate(
        feature_pred_tax_cla_04dirpar_val = ifelse(
          test = feature_pred_tax_cla_04dirpar_score >= minimal_consistency,
          yes = feature_pred_tax_cla_04dirpar_val,
          no = "notConsistent"
        )
      )

    log_debug("joining all except -1 together \n")
    annot_table_wei_bio_clean <-
      tidytable::left_join(df,
        freq_cla_kin,
        by = stats::setNames(
          "feature_source",
          "feature_id"
        )
      ) |>
      tidytable::left_join(freq_npc_pat,
        by = stats::setNames(
          "feature_source",
          "feature_id"
        )
      ) |>
      tidytable::left_join(freq_cla_sup,
        by = stats::setNames(
          "feature_source",
          "feature_id"
        )
      ) |>
      tidytable::left_join(freq_npc_sup,
        by = stats::setNames(
          "feature_source",
          "feature_id"
        )
      ) |>
      tidytable::left_join(freq_cla_cla,
        by = stats::setNames(
          "feature_source",
          "feature_id"
        )
      ) |>
      tidytable::left_join(freq_npc_cla,
        by = stats::setNames(
          "feature_source",
          "feature_id"
        )
      ) |>
      tidytable::left_join(freq_cla_par,
        by = stats::setNames(
          "feature_source",
          "feature_id"
        )
      ) |>
      tidytable::select(
        feature_id,
        tidytable::everything()
      ) |>
      ## In case there are no consensus at all because no network
      tidytable::mutate(tidytable::across(
        .cols = tidytable::where(is.logical),
        .fns = as.character
      )) |>
      log_pipe("adding dummy consistency for features
              with less than 2 neighbors \n") |>
      tidytable::mutate(
        feature_pred_tax_cla_01kin_val =
          tidytable::coalesce(
            feature_pred_tax_cla_01kin_val,
            "dummy"
          ),
        consistency_structure_cla_kin =
          tidytable::coalesce(
            consistency_structure_cla_kin,
            1
          ),
        feature_pred_tax_cla_01kin_score =
          tidytable::coalesce(
            feature_pred_tax_cla_01kin_score,
            0
          ),
        feature_pred_tax_npc_01pat_val =
          tidytable::coalesce(
            feature_pred_tax_npc_01pat_val,
            "dummy"
          ),
        consistency_structure_npc_pat =
          tidytable::coalesce(
            consistency_structure_npc_pat,
            1
          ),
        feature_pred_tax_npc_01pat_score =
          tidytable::coalesce(
            feature_pred_tax_npc_01pat_score,
            0
          ),
        feature_pred_tax_cla_02sup_val =
          tidytable::coalesce(
            feature_pred_tax_cla_02sup_val,
            "dummy"
          ),
        consistency_structure_cla_sup =
          tidytable::coalesce(
            consistency_structure_cla_sup,
            1
          ),
        feature_pred_tax_cla_02sup_score =
          tidytable::coalesce(
            feature_pred_tax_cla_02sup_score,
            0
          ),
        feature_pred_tax_npc_02sup_val =
          tidytable::coalesce(
            feature_pred_tax_npc_02sup_val,
            "dummy"
          ),
        consistency_structure_npc_sup =
          tidytable::coalesce(
            consistency_structure_npc_sup,
            1
          ),
        feature_pred_tax_npc_02sup_score =
          tidytable::coalesce(
            feature_pred_tax_npc_02sup_score,
            0
          ),
        feature_pred_tax_cla_03cla_val =
          tidytable::coalesce(
            feature_pred_tax_cla_03cla_val,
            "dummy"
          ),
        consistency_structure_cla_cla =
          tidytable::coalesce(
            consistency_structure_cla_cla,
            1
          ),
        feature_pred_tax_cla_03cla_score =
          tidytable::coalesce(
            feature_pred_tax_cla_03cla_score,
            0
          ),
        feature_pred_tax_npc_03cla_val =
          tidytable::coalesce(
            feature_pred_tax_npc_03cla_val,
            "dummy"
          ),
        consistency_structure_npc_cla =
          tidytable::coalesce(
            consistency_structure_npc_cla,
            1
          ),
        feature_pred_tax_npc_03cla_score =
          tidytable::coalesce(
            feature_pred_tax_npc_03cla_score,
            0
          ),
        feature_pred_tax_cla_04dirpar_val =
          tidytable::coalesce(
            feature_pred_tax_cla_04dirpar_val,
            "dummy"
          ),
        consistency_structure_cla_par =
          tidytable::coalesce(
            consistency_structure_cla_par,
            1
          ),
        feature_pred_tax_cla_04dirpar_score =
          tidytable::coalesce(
            feature_pred_tax_cla_04dirpar_score,
            0
          )
      )

    rm(
      annot_table_wei_bio,
      df,
      df3
    )

    return(annot_table_wei_bio_clean)
  }
