import::from(stats, setNames, .into = environment())
import::from(tidytable, across, .into = environment())
import::from(tidytable, add_count, .into = environment())
import::from(tidytable, anti_join, .into = environment())
import::from(tidytable, arrange, .into = environment())
import::from(tidytable, bind_rows, .into = environment())
import::from(tidytable, coalesce, .into = environment())
import::from(tidytable, contains, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, everything, .into = environment())
import::from(tidytable, filter, .into = environment())
import::from(tidytable, group_by, .into = environment())
import::from(tidytable, left_join, .into = environment())
import::from(tidytable, mutate, .into = environment())
import::from(tidytable, n_distinct, .into = environment())
import::from(tidytable, right_join, .into = environment())
import::from(tidytable, select, .into = environment())
import::from(tidytable, ungroup, .into = environment())
import::from(tidytable, where, .into = environment())

#' @title Clean bio
#'
#' @description This function cleans the results
#'    obtained after biological weighting
#'
#' @importFrom stats setNames
#' @importFrom tidytable across
#' @importFrom tidytable add_count
#' @importFrom tidytable anti_join
#' @importFrom tidytable arrange
#' @importFrom tidytable bind_rows
#' @importFrom tidytable coalesce
#' @importFrom tidytable contains
#' @importFrom tidytable distinct
#' @importFrom tidytable everything
#' @importFrom tidytable filter
#' @importFrom tidytable group_by
#' @importFrom tidytable left_join
#' @importFrom tidytable mutate
#' @importFrom tidytable n_distinct
#' @importFrom tidytable right_join
#' @importFrom tidytable select
#' @importFrom tidytable ungroup
#' @importFrom tidytable where
#'
#' @noRd
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
      distinct(
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

    log_debug("calculating chemical consistency
              features with at least 2 neighbors ... \n")
    df3 <-
      right_join(
        edges_table |>
          group_by(feature_source) |>
          add_count() |>
          ungroup() |>
          filter(n >= 2) |>
          select(-n),
        df |>
          distinct(
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
        by = setNames("feature_id", "feature_target")
      ) |>
      filter(!is.na(feature_source))

    log_debug("... among all edges ... \n")
    clean_per_level_bio <-
      function(df,
               candidates,
               consistency_name,
               feature_score_name,
               feature_val_name) {
        freq <- df |>
          distinct(
            feature_source,
            feature_target, !!as.name(candidates),
            score_weighted_bio
          ) |>
          mutate(
            count = n_distinct(feature_target),
            .by = c(feature_source, !!as.name(candidates))
          ) |>
          mutate(
            !!as.name(consistency_name) := count /
              n_distinct(feature_target),
            .by = c(feature_source)
          ) |>
          distinct(feature_source, !!as.name(candidates), .keep_all = TRUE) |>
          mutate(
            !!as.name(feature_score_name) :=
              !!as.name(consistency_name) * score_weighted_bio,
            .by = c(feature_source, !!as.name(candidates))
          ) |>
          arrange(-!!as.name(feature_score_name)) |>
          distinct(feature_source, .keep_all = TRUE) |>
          select(
            feature_source, !!as.name(feature_val_name) := !!as.name(candidates), !!as.name(consistency_name), !!as.name(feature_score_name)
          ) |>
          mutate(
            !!as.name(feature_val_name) := ifelse(
              test = !!as.name(feature_score_name) >= minimal_consistency,
              yes = !!as.name(feature_val_name),
              no = "notConsistent"
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

    log_debug("splitting already computed predictions \n")
    df1 <- df |>
      filter(!is.na(feature_pred_tax_cla_02sup_val))

    df1b <- df1 |>
      select(-contains("feature_pred_tax"))

    df2 <- df |>
      select(-contains("feature_pred_tax")) |>
      anti_join(df1) |>
      bind_rows(df1b)

    log_debug("joining all except -1 together \n")
    annot_table_wei_bio_preclean <-
      left_join(df2, freq_cla_kin, by = setNames("feature_source", "feature_id")) |>
      left_join(freq_npc_pat, by = setNames("feature_source", "feature_id")) |>
      left_join(freq_cla_sup, by = setNames("feature_source", "feature_id")) |>
      left_join(freq_npc_sup, by = setNames("feature_source", "feature_id")) |>
      left_join(freq_cla_cla, by = setNames("feature_source", "feature_id")) |>
      left_join(freq_npc_cla, by = setNames("feature_source", "feature_id")) |>
      left_join(freq_cla_par, by = setNames("feature_source", "feature_id")) |>
      select(feature_id, everything()) |>
      ## In case there are no consensus at all because no network
      mutate(across(.cols = where(is.logical), .fns = as.character)) |>
      log_pipe("adding dummy consistency for features
              with less than 2 neighbors \n") |>
      mutate(
        feature_pred_tax_cla_01kin_val = coalesce(feature_pred_tax_cla_01kin_val, "dummy"),
        consistency_structure_cla_kin = coalesce(consistency_structure_cla_kin, 1),
        feature_pred_tax_cla_01kin_score = coalesce(feature_pred_tax_cla_01kin_score, 0),
        feature_pred_tax_npc_01pat_val = coalesce(feature_pred_tax_npc_01pat_val, "dummy"),
        consistency_structure_npc_pat = coalesce(consistency_structure_npc_pat, 1),
        feature_pred_tax_npc_01pat_score = coalesce(feature_pred_tax_npc_01pat_score, 0),
        feature_pred_tax_cla_02sup_val = coalesce(feature_pred_tax_cla_02sup_val, "dummy"),
        consistency_structure_cla_sup = coalesce(consistency_structure_cla_sup, 1),
        feature_pred_tax_cla_02sup_score = coalesce(feature_pred_tax_cla_02sup_score, 0),
        feature_pred_tax_npc_02sup_val = coalesce(feature_pred_tax_npc_02sup_val, "dummy"),
        consistency_structure_npc_sup = coalesce(consistency_structure_npc_sup, 1),
        feature_pred_tax_npc_02sup_score = coalesce(feature_pred_tax_npc_02sup_score, 0),
        feature_pred_tax_cla_03cla_val = coalesce(feature_pred_tax_cla_03cla_val, "dummy"),
        consistency_structure_cla_cla = coalesce(consistency_structure_cla_cla, 1),
        feature_pred_tax_cla_03cla_score = coalesce(feature_pred_tax_cla_03cla_score, 0),
        feature_pred_tax_npc_03cla_val = coalesce(feature_pred_tax_npc_03cla_val, "dummy"),
        consistency_structure_npc_cla = coalesce(consistency_structure_npc_cla, 1),
        feature_pred_tax_npc_03cla_score = coalesce(feature_pred_tax_npc_03cla_score, 0),
        feature_pred_tax_cla_04dirpar_val = coalesce(feature_pred_tax_cla_04dirpar_val, "dummy"),
        consistency_structure_cla_par = coalesce(consistency_structure_cla_par, 1),
        feature_pred_tax_cla_04dirpar_score = coalesce(feature_pred_tax_cla_04dirpar_score, 0)
      )

    log_debug("adding already computed predictions back \n")
    annot_table_wei_bio_clean <- annot_table_wei_bio_preclean |>
      anti_join(df1b) |>
      bind_rows(df1)

    rm(
      annot_table_wei_bio,
      annot_table_wei_bio_preclean,
      df,
      df1,
      df1b,
      df2,
      df3
    )

    return(annot_table_wei_bio_clean)
  }
