#' @title Clean chemo
#'
#' @description This function cleans the results
#'    obtained after chemical weighting
#'
#' @include clean_collapse.R
#' @include columns_model.R
#' @include filter_high_confidence_only.R
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
  function(annot_table_wei_chemo = get("annot_table_wei_chemo", envir = parent.frame()),
           components_table = get("components_table", envir = parent.frame()),
           features_table = get("features_table", envir = parent.frame()),
           structure_organism_pairs_table = get("structure_organism_pairs_table", envir = parent.frame()),
           candidates_final = get("candidates_final", envir = parent.frame()),
           minimal_ms1_bio = get("minimal_ms1_bio", envir = parent.frame()),
           minimal_ms1_chemo = get("minimal_ms1_chemo", envir = parent.frame()),
           minimal_ms1_condition = get("minimal_ms1_condition", envir = parent.frame()),
           high_confidence = get("high_confidence", envir = parent.frame()),
           remove_ties = get("remove_ties", envir = parent.frame()),
           summarize = get("summarize", envir = parent.frame())) {
    model <- columns_model()

    log_debug(
      "filtering top ",
      candidates_final,
      " candidates and keeping only MS1 candidates with minimum \n",
      minimal_ms1_bio,
      " biological score \n",
      minimal_ms1_condition,
      minimal_ms1_chemo,
      "chemical score \n"
    )

    ## Those lines are to keep ms1 annotation
    ## Only if a good biological
    ## Or chemical consistency score is obtained
    if (minimal_ms1_condition == "OR") {
      df1 <- annot_table_wei_chemo |>
        tidytable::filter(
          (
            !is.na(candidate_score_similarity) |
              !is.na(candidate_score_sirius_csi)
          ) |
            (
              score_biological >= minimal_ms1_bio |
                score_chemical >= minimal_ms1_chemo
            )
        )
    }
    if (minimal_ms1_condition == "AND") {
      df1 <- annot_table_wei_chemo |>
        tidytable::filter(
          (
            !is.na(candidate_score_similarity) |
              !is.na(candidate_score_sirius_csi)
          ) |
            (
              score_biological >= minimal_ms1_bio &
                score_chemical >= minimal_ms1_chemo
            )
        )
    }

    if (high_confidence) {
      df1 <- df1 |>
        filter_high_confidence_only()
    }

    df1 <- df1 |>
      tidytable::arrange(tidytable::desc(score_weighted_chemo)) |>
      tidytable::distinct(feature_id,
        candidate_structure_inchikey_no_stereo,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        rank_initial = tidytable::dense_rank(-candidate_score_pseudo_initial),
        rank_final = tidytable::dense_rank(-score_weighted_chemo),
        .by = c(feature_id)
      ) |>
      tidytable::filter(rank_final <= candidates_final)

    log_debug("adding initial metadata (RT, etc.) and simplifying columns \n")
    df3 <- features_table |>
      tidytable::left_join(df1) |>
      tidytable::left_join(components_table) |>
      # tidytable::mutate(
      #   candidate_structure_tax_cla = paste(
      #     structure_tax_cla_01kin,
      #     structure_tax_cla_02sup,
      #     structure_tax_cla_03cla,
      #     structure_tax_cla_04dirpar,
      #     sep = "\u00a7"
      #   ),
      #   candidate_structure_tax_npc = paste(
      #     structure_tax_npc_01pat,
      #     structure_tax_npc_02sup,
      #     structure_tax_npc_03cla,
      #     sep = "\u00a7"
      #   )
      # ) |>
      tidytable::select(tidyselect::any_of(
        c(
          "feature_id",
          "feature_rt" = "rt",
          "feature_mz" = "mz",
          model$features_calculated_columns,
          model$components_columns,
          model$candidates_calculated_columns,
          model$candidates_sirius_for_columns,
          model$candidates_sirius_str_columns,
          model$candidates_spectra_columns,
          model$candidates_structures_columns,
          model$rank_columns,
          "score_initial" = "candidate_score_pseudo_initial",
          "score_biological",
          "score_interim" = "score_weighted_bio",
          "score_chemical",
          "score_final" = "score_weighted_chemo"
        )
      )) |>
      tidytable::distinct() |>
      tidytable::left_join(
        structure_organism_pairs_table |>
          tidytable::select(
            candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
            reference_doi,
            organism_name,
            tidyselect::contains("organism_taxonomy_"), -organism_taxonomy_ottid
          ) |>
          tidytable::distinct() |>
          tidytable::pivot_longer(tidyselect::contains("organism_taxonomy_")) |>
          tidytable::filter(!is.na(value)) |>
          tidytable::filter(value != "notClassified") |>
          tidytable::distinct(
            candidate_structure_inchikey_no_stereo,
            candidate_structure_organism_occurrence_closest = value,
            candidate_structure_organism_occurrence_reference = reference_doi
          )
      ) |>
      tidytable::group_by(c(-candidate_structure_organism_occurrence_reference)) |>
      clean_collapse(cols = c("candidate_structure_organism_occurrence_reference")) |>
      tidytable::select(tidyselect::any_of(
        c(
          model$features_columns,
          model$features_calculated_columns,
          model$components_columns,
          model$candidates_calculated_columns,
          model$candidates_sirius_for_columns,
          model$candidates_sirius_str_columns,
          model$candidates_spectra_columns,
          model$candidates_structures_columns,
          model$rank_columns,
          model$score_columns
        )
      )) |>
      tidytable::arrange(rank_final) |>
      tidytable::ungroup()
    rm(df1)

    if (remove_ties == TRUE) {
      log_debug("Removing ties ...")
      df3 <- df3 |>
        tidytable::distinct(c(feature_id, rank_final), .keep_all = TRUE)
    }

    if (summarize == TRUE) {
      log_debug("Collecting garbage ...")
      gc()
      log_debug("summarizing results \n")
      df4 <- df3 |>
        tidytable::group_by(feature_id) |>
        tidytable::reframe(tidytable::across(
          .cols = colnames(df3)[grepl(
            pattern = "^candidate|^rank|^score",
            x = colnames(df3),
            perl = TRUE
          )],
          .fns = function(x) {
            gsub(
              pattern = "\\bNA\\b",
              replacement = "",
              x = paste(x, collapse = "|"),
              perl = TRUE
            )
          }
        )) |>
        tidytable::ungroup()

      df5 <- df4 |>
        tidytable::left_join(df3 |>
          tidytable::select("feature_id", !colnames(df4)) |>
          tidytable::distinct())
      rm(df4)
    } else {
      df5 <- df3
    }
    rm(df3)

    log_debug("selecting columns to export \n")
    df6 <- df5 |>
      tidytable::mutate(tidytable::across(.cols = tidyselect::everything(), .fns = as.character)) |>
      tidytable::mutate(tidytable::across(.cols = tidyselect::where(is.character), .fns = trimws)) |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::where(is.character),
        .fns = function(x) {
          tidytable::na_if(x, "")
        }
      )) |>
      tidytable::select(tidyselect::any_of(
        c(
          model$features_columns,
          model$features_calculated_columns,
          model$components_columns,
          model$candidates_calculated_columns,
          model$candidates_sirius_for_columns,
          model$candidates_sirius_str_columns,
          model$candidates_spectra_columns,
          model$candidates_structures_columns,
          model$rank_columns,
          model$score_columns
        )
      ))
    rm(df5)

    log_debug("adding consensus again to droped candidates \n")
    results <- tidytable::bind_rows(
      df6 |>
        tidytable::filter(!is.na(
          candidate_structure_inchikey_no_stereo
        )),
      tidytable::left_join(
        df6 |>
          tidytable::filter(is.na(
            candidate_structure_inchikey_no_stereo
          )) |>
          tidytable::distinct(model$features_columns),
        annot_table_wei_chemo |>
          tidytable::mutate(tidytable::across(.cols = tidyselect::everything(), .fns = as.character))
      ) |>
        tidytable::select(tidyselect::any_of(
          c(
            model$features_columns,
            model$features_calculated_columns,
            model$components_columns
          )
        )) |>
        tidytable::distinct()
    ) |>
      tidytable::arrange(as.numeric(feature_id))

    rm(annot_table_wei_chemo, df6)

    return(results)
  }
