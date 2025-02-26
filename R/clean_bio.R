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
#' @seealso weight_bio
#'
#' @examples NULL
clean_bio <-
  function(annot_table_wei_bio = get("annot_table_wei_bio", envir = parent.frame()),
           edges_table = get("edges_table", envir = parent.frame()),
           minimal_consistency = get("minimal_consistency", envir = parent.frame())) {
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
        score_weighted_bio,
        .keep_all = TRUE
      )
    rm(annot_table_wei_bio)

    log_debug("calculating chemical consistency
              features with at least 2 neighbors ... \n")
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
            score_weighted_bio
          ),
        by = stats::setNames("feature_id", "feature_target")
      ) |>
      tidytable::filter(!is.na(feature_source))

    log_debug("... among all edges ... \n")
    clean_per_level_bio <-
      function(df,
               candidates,
               consistency_name,
               feature_score_name,
               feature_val_name) {
        freq <- df |>
          tidytable::distinct(
            feature_source,
            feature_target, !!as.name(candidates),
            score_weighted_bio
          ) |>
          tidytable::mutate(
            count = tidytable::n_distinct(feature_target),
            .by = c(feature_source, !!as.name(candidates))
          ) |>
          tidytable::mutate(
            !!as.name(consistency_name) := count /
              tidytable::n_distinct(feature_target),
            .by = c(feature_source)
          ) |>
          tidytable::distinct(feature_source, !!as.name(candidates), .keep_all = TRUE) |>
          tidytable::mutate(
            !!as.name(feature_score_name) :=
              !!as.name(consistency_name) * score_weighted_bio,
            .by = c(feature_source, !!as.name(candidates))
          ) |>
          tidytable::arrange(-!!as.name(feature_score_name)) |>
          tidytable::distinct(feature_source, .keep_all = TRUE) |>
          tidytable::select(
            feature_source, !!as.name(feature_val_name) := !!as.name(candidates), !!as.name(consistency_name), !!as.name(feature_score_name)
          ) |>
          tidytable::mutate(
            !!as.name(feature_val_name) := tidytable::if_else(
              condition = !!as.name(feature_score_name) >= minimal_consistency,
              true = !!as.name(feature_val_name),
              false = "notConsistent"
            )
          )
      }

    log_debug("... at the (classyfire) kingdom level \n")
    freq_cla_kin <- df3 |>
      clean_per_level_bio(
        candidates = "candidate_structure_tax_cla_01kin",
        consistency_name = "consistency_structure_cla_kin",
        feature_score_name = "feature_pred_tax_cla_01kin_score",
        feature_val_name = "feature_pred_tax_cla_01kin_val"
      )
    log_debug("... at the (NPC) pathway level \n")
    freq_npc_pat <- df3 |>
      clean_per_level_bio(
        candidates = "candidate_structure_tax_npc_01pat",
        consistency_name = "consistency_structure_npc_pat",
        feature_score_name = "feature_pred_tax_npc_01pat_score",
        feature_val_name = "feature_pred_tax_npc_01pat_val"
      )
    log_debug("... at the (classyfire) superclass level \n")
    freq_cla_sup <- df3 |>
      clean_per_level_bio(
        candidates = "candidate_structure_tax_cla_02sup",
        consistency_name = "consistency_structure_cla_sup",
        feature_score_name = "feature_pred_tax_cla_02sup_score",
        feature_val_name = "feature_pred_tax_cla_02sup_val"
      )
    log_debug("... at the (NPC) superclass level \n")
    freq_npc_sup <- df3 |>
      clean_per_level_bio(
        candidates = "candidate_structure_tax_npc_02sup",
        consistency_name = "consistency_structure_npc_sup",
        feature_score_name = "feature_pred_tax_npc_02sup_score",
        feature_val_name = "feature_pred_tax_npc_02sup_val"
      )
    log_debug("... at the (classyfire) class level \n")
    freq_cla_cla <- df3 |>
      clean_per_level_bio(
        candidates = "candidate_structure_tax_cla_03cla",
        consistency_name = "consistency_structure_cla_cla",
        feature_score_name = "feature_pred_tax_cla_03cla_score",
        feature_val_name = "feature_pred_tax_cla_03cla_val"
      )
    log_debug("... at the (NPC) class level \n")
    freq_npc_cla <- df3 |>
      clean_per_level_bio(
        candidates = "candidate_structure_tax_npc_03cla",
        consistency_name = "consistency_structure_npc_cla",
        feature_score_name = "feature_pred_tax_npc_03cla_score",
        feature_val_name = "feature_pred_tax_npc_03cla_val"
      )
    log_debug("... at the (classyfire) parent level \n")
    freq_cla_par <- df3 |>
      clean_per_level_bio(
        candidates = "candidate_structure_tax_cla_04dirpar",
        consistency_name = "consistency_structure_cla_par",
        feature_score_name = "feature_pred_tax_cla_04dirpar_score",
        feature_val_name = "feature_pred_tax_cla_04dirpar_val"
      )
    rm(df3)

    log_debug("splitting already computed predictions \n")
    df1 <- df |>
      tidytable::filter(!is.na(feature_pred_tax_cla_02sup_val))

    df1b <- df1 |>
      tidytable::select(-tidyselect::contains("feature_pred_tax"))

    df2 <- df |>
      tidytable::select(-tidyselect::contains("feature_pred_tax")) |>
      tidytable::anti_join(df1) |>
      tidytable::bind_rows(df1b)
    rm(df)

    log_debug("joining all except -1 together \n")
    supp_tables <- list(
      freq_cla_kin,
      freq_npc_pat,
      freq_cla_sup,
      freq_npc_sup,
      freq_cla_cla,
      freq_npc_cla,
      freq_cla_par
    )
    rm(
      freq_cla_kin,
      freq_npc_pat,
      freq_cla_sup,
      freq_npc_sup,
      freq_cla_cla,
      freq_npc_cla,
      freq_cla_par
    )

    annot_table_wei_bio_preclean <- purrr::reduce(
      .x = supp_tables,
      .init = df2,
      .f = function(x, y) {
        tidytable::left_join(x, y, by = stats::setNames("feature_source", "feature_id"))
      }
    ) |>
      tidytable::select(feature_id, tidyselect::everything()) |>
      ## In case there are no consensus at all because no network
      tidytable::mutate(tidytable::across(.cols = tidyselect::where(is.logical), .fns = as.character)) |>
      tidytable::mutate(
        feature_pred_tax_cla_01kin_val = tidytable::coalesce(feature_pred_tax_cla_01kin_val, "empty"),
        consistency_structure_cla_kin = tidytable::coalesce(consistency_structure_cla_kin, 1),
        feature_pred_tax_cla_01kin_score = tidytable::coalesce(feature_pred_tax_cla_01kin_score, 0),
        feature_pred_tax_npc_01pat_val = tidytable::coalesce(feature_pred_tax_npc_01pat_val, "empty"),
        consistency_structure_npc_pat = tidytable::coalesce(consistency_structure_npc_pat, 1),
        feature_pred_tax_npc_01pat_score = tidytable::coalesce(feature_pred_tax_npc_01pat_score, 0),
        feature_pred_tax_cla_02sup_val = tidytable::coalesce(feature_pred_tax_cla_02sup_val, "empty"),
        consistency_structure_cla_sup = tidytable::coalesce(consistency_structure_cla_sup, 1),
        feature_pred_tax_cla_02sup_score = tidytable::coalesce(feature_pred_tax_cla_02sup_score, 0),
        feature_pred_tax_npc_02sup_val = tidytable::coalesce(feature_pred_tax_npc_02sup_val, "empty"),
        consistency_structure_npc_sup = tidytable::coalesce(consistency_structure_npc_sup, 1),
        feature_pred_tax_npc_02sup_score = tidytable::coalesce(feature_pred_tax_npc_02sup_score, 0),
        feature_pred_tax_cla_03cla_val = tidytable::coalesce(feature_pred_tax_cla_03cla_val, "empty"),
        consistency_structure_cla_cla = tidytable::coalesce(consistency_structure_cla_cla, 1),
        feature_pred_tax_cla_03cla_score = tidytable::coalesce(feature_pred_tax_cla_03cla_score, 0),
        feature_pred_tax_npc_03cla_val = tidytable::coalesce(feature_pred_tax_npc_03cla_val, "empty"),
        consistency_structure_npc_cla = tidytable::coalesce(consistency_structure_npc_cla, 1),
        feature_pred_tax_npc_03cla_score = tidytable::coalesce(feature_pred_tax_npc_03cla_score, 0),
        feature_pred_tax_cla_04dirpar_val = tidytable::coalesce(feature_pred_tax_cla_04dirpar_val, "empty"),
        consistency_structure_cla_par = tidytable::coalesce(consistency_structure_cla_par, 1),
        feature_pred_tax_cla_04dirpar_score = tidytable::coalesce(feature_pred_tax_cla_04dirpar_score, 0)
      )
    rm(df2, supp_tables)

    log_debug("adding already computed predictions back \n")
    annot_table_wei_bio_clean <- annot_table_wei_bio_preclean |>
      tidytable::anti_join(df1b) |>
      tidytable::bind_rows(df1)

    rm(
      annot_table_wei_bio_preclean,
      df1,
      df1b
    )

    return(annot_table_wei_bio_clean)
  }
