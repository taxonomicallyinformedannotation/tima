#' @title Clean chemo
#'
#' @description This function cleans the results
#'    obtained after chemical weighting
#'
#' @include clean_collapse.R
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
#' @param summarise Boolean. summarise results (1 row per feature)
#'
#' @return A table containing the chemically weighted annotation
#'    where only a given number of initial candidates are kept
#'
#' @export
#'
#' @seealso weight_chemo
#'
#' @examples NULL
clean_chemo <-
  function(annot_table_wei_chemo = get("annot_table_wei_chemo",
             envir = parent.frame()
           ),
           components_table = get("components_table",
             envir = parent.frame()
           ),
           features_table = get("features_table",
             envir = parent.frame()
           ),
           structure_organism_pairs_table = get(
             "structure_organism_pairs_table",
             envir = parent.frame()
           ),
           candidates_final = get("candidates_final",
             envir = parent.frame()
           ),
           minimal_ms1_bio = get("minimal_ms1_bio",
             envir = parent.frame()
           ),
           minimal_ms1_chemo = get("minimal_ms1_chemo",
             envir = parent.frame()
           ),
           minimal_ms1_condition = get("minimal_ms1_condition",
             envir = parent.frame()
           ),
           summarise = get("summarise",
             envir = parent.frame()
           )) {
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
    if (minimal_ms1_condition == "OR") {
      df1 <- annot_table_wei_chemo |>
        tidytable::filter(
          as.numeric(score_input) > 0 | (
            ## Those lines are to keep ms1 annotation
            score_biological >= minimal_ms1_bio |
              ## Only if a good biological
              score_chemical >= minimal_ms1_chemo
          )
          ## Or chemical consistency score is obtained
        )
    }
    if (minimal_ms1_condition == "AND") {
      df1 <- annot_table_wei_chemo |>
        tidytable::filter(
          as.numeric(score_input) > 0 | (
            ## Those lines are to keep ms1 annotation
            score_biological >= minimal_ms1_bio &
              ## Only if a good biological
              score_chemical >= minimal_ms1_chemo
          )
          ## Or chemical consistency score is obtained
        )
    }

    df1 <- df1 |>
      tidytable::arrange(tidytable::desc(score_pondered_chemo)) |>
      tidytable::distinct(feature_id,
        structure_inchikey_no_stereo,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        rank_initial = tidytable::dense_rank(-as.numeric(score_input)),
        rank_final = tidytable::dense_rank(-score_pondered_chemo),
        .by = c(feature_id)
      ) |>
      tidytable::filter(rank_final <= candidates_final)

    log_debug("adding initial metadata (RT, etc.) and simplifying columns \n")
    df3 <- features_table |>
      tidytable::left_join(df1) |>
      tidytable::left_join(components_table) |>
      tidytable::mutate(
        candidate_structure_tax_cla = paste(
          structure_tax_cla_01kin,
          structure_tax_cla_02sup,
          structure_tax_cla_03cla,
          structure_tax_cla_04dirpar,
          sep = "\u00a7"
        ),
        candidate_structure_tax_npc = paste(
          structure_tax_npc_01pat,
          structure_tax_npc_02sup,
          structure_tax_npc_03cla,
          sep = "\u00a7"
        )
      ) |>
      tidytable::select(tidytable::any_of(c(
        "feature_id",
        "feature_rt" = "rt",
        "feature_mz" = "mz",
        "feature_spectrum_entropy",
        "feature_pred_tax_cla_01kin_val" = "consensus_structure_cla_kin",
        "feature_pred_tax_cla_01kin_score" = "consistency_structure_cla_kin",
        "feature_pred_tax_cla_02sup_val" = "consensus_structure_cla_sup",
        "feature_pred_tax_cla_02sup_score" = "consistency_structure_cla_sup",
        "feature_pred_tax_cla_03cla_val" = "consensus_structure_cla_cla",
        "feature_pred_tax_cla_03cla_score" = "consensus_structure_cla_cla",
        "feature_pred_tax_cla_04dirpar_val" = "consensus_structure_cla_par",
        "feature_pred_tax_cla_04dirpar_score" = "consensus_structure_cla_par",
        "feature_pred_tax_npc_01pat_val" = "consensus_structure_npc_pat",
        "feature_pred_tax_npc_01pat_score" = "consistency_structure_npc_pat",
        "feature_pred_tax_npc_02sup_val" = "consensus_structure_npc_sup",
        "feature_pred_tax_npc_02sup_score" = "consistency_structure_npc_sup",
        "feature_pred_tax_npc_03cla_val" = "consensus_structure_npc_cla",
        "feature_pred_tax_npc_03cla_score" = "consistency_structure_npc_cla",
        "component_id",
        "candidate_structure_molecular_formula" = "structure_molecular_formula",
        "candidate_structure_exact_mass" = "structure_exact_mass",
        "candidate_structure_xlogp" = "structure_xlogp",
        "candidate_structure_inchikey_no_stereo" = "structure_inchikey_no_stereo",
        "candidate_structure_smiles_no_stereo" = "structure_smiles_no_stereo",
        "candidate_structure_name" = "structure_name",
        "candidate_structure_tax_cla",
        "candidate_structure_tax_npc",
        "candidate_library" = "library",
        ## TODO "library_type",
        "candidate_count_similarity_peaks_matched" = "count_peaks_matched",
        "candidate_score_similarity",
        "candidate_count_sirius_peaks_explained" = "count_peaks_explained",
        "candidate_score_sirius_intensity",
        "candidate_score_sirius_isotope",
        "candidate_score_sirius_sirius",
        "candidate_score_sirius_tree",
        "candidate_score_sirius_zodiac",
        "candidate_score_sirius_confidence",
        "candidate_score_sirius_csi",
        "candidate_structure_error_mz" = "error_mz",
        "candidate_structure_error_rt" = "error_rt",
        "rank_initial",
        "rank_final",
        "score_initial" = "score_input",
        "score_biological",
        "score_interim" = "score_pondered_bio",
        "score_chemical",
        "score_final" = "score_pondered_chemo",
        "best_can_org" = "best_candidate"
      ))) |>
      tidytable::distinct() |>
      log_pipe("adding references \n") |>
      tidytable::left_join(structure_organism_pairs_table |>
        tidytable::select(
          candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
          reference_doi,
          organism_name,
          tidytable::contains("organism_taxonomy_"),
          -organism_taxonomy_ottid
        ) |>
        tidytable::distinct() |>
        tidytable::pivot_longer(tidytable::contains("organism_taxonomy_")) |>
        tidytable::filter(!is.na(value)) |>
        tidytable::filter(value != "notClassified") |>
        tidytable::distinct(
          candidate_structure_inchikey_no_stereo,
          best_can_org = value,
          reference_doi
        )) |>
      tidytable::group_by(c(-reference_doi)) |>
      clean_collapse(cols = c("reference_doi")) |>
      tidytable::select(tidytable::any_of(c(
        "feature_id",
        "feature_rt",
        "feature_mz",
        "feature_spectrum_entropy",
        "feature_pred_tax_cla_01kin_val",
        "feature_pred_tax_cla_01kin_score",
        "feature_pred_tax_cla_02sup_val",
        "feature_pred_tax_cla_02sup_score",
        "feature_pred_tax_cla_03cla_val",
        "feature_pred_tax_cla_03cla_score",
        "feature_pred_tax_cla_04dirpar_val",
        "feature_pred_tax_cla_04dirpar_score",
        "feature_pred_tax_npc_01pat_val",
        "feature_pred_tax_npc_01pat_score",
        "feature_pred_tax_npc_02sup_val",
        "feature_pred_tax_npc_02sup_score",
        "feature_pred_tax_npc_03cla_val",
        "feature_pred_tax_npc_03cla_score",
        "component_id",
        "candidate_structure_molecular_formula",
        "candidate_structure_exact_mass",
        "candidate_structure_xlogp",
        "candidate_structure_inchikey_no_stereo",
        "candidate_structure_smiles_no_stereo",
        "candidate_structure_name",
        "candidate_structure_tax_cla",
        "candidate_structure_tax_npc",
        "candidate_structure_organism_occurrence_closest" = "best_can_org",
        "candidate_structure_organism_occurrence_reference" = "reference_doi",
        "candidate_library",
        ## TODO "library_type",
        "candidate_count_similarity_peaks_matched",
        "candidate_score_similarity",
        "candidate_count_sirius_peaks_explained",
        "candidate_score_sirius_intensity",
        "candidate_score_sirius_isotope",
        "candidate_score_sirius_sirius",
        "candidate_score_sirius_tree",
        "candidate_score_sirius_zodiac",
        "candidate_score_sirius_confidence",
        "candidate_score_sirius_csi",
        "candidate_structure_error_mz",
        "candidate_structure_error_rt",
        "rank_initial",
        "rank_final",
        "score_initial",
        "score_biological",
        # "score_interim",
        "score_chemical",
        "score_final"
      ))) |>
      tidytable::arrange(rank_final)
    rm(df1)

    if (summarise == TRUE) {
      log_debug("Collecting garbage ...")
      gc()
      log_debug("summarizing results \n")
      df4 <- df3 |>
        tidytable::group_by(feature_id) |>
        tidytable::reframe(tidytable::across(
          .cols = colnames(df3)[grepl(
            pattern = "^candidate|^rank|^score",
            x = colnames(df3)
          )],
          .fns = function(x) {
            gsub(
              pattern = "\\bNA\\b",
              replacement = "",
              x = paste(x, collapse = "|")
            )
          }
        )) |>
        tidytable::ungroup()

      df5 <- df4 |>
        tidytable::left_join(df3 |>
          tidytable::select(
            "feature_id",
            !colnames(df4)
          ) |>
          tidytable::distinct())
      rm(df4)
    } else {
      df5 <- df3
    }
    rm(df3)

    log_debug("selecting columns to export \n")
    df6 <- df5 |>
      tidytable::mutate(
        tidytable::across(
          .cols = tidytable::everything(),
          .fns = as.character
        )
      ) |>
      tidytable::mutate(
        tidytable::across(
          .cols = tidytable::where(is.character),
          .fns = trimws
        )
      ) |>
      tidytable::mutate(
        tidytable::across(
          .cols = tidytable::where(is.character),
          .fns = function(x) {
            tidytable::na_if(x, "")
          }
        )
      ) |>
      tidytable::select(tidytable::any_of(
        c(
          "feature_id",
          "feature_rt",
          "feature_mz",
          "feature_spectrum_entropy",
          "feature_pred_tax_cla_01kin_val",
          "feature_pred_tax_cla_01kin_score",
          "feature_pred_tax_cla_02sup_val",
          "feature_pred_tax_cla_02sup_score",
          "feature_pred_tax_cla_03cla_val",
          "feature_pred_tax_cla_03cla_score",
          "feature_pred_tax_cla_04dirpar_val",
          "feature_pred_tax_cla_04dirpar_score",
          "feature_pred_tax_npc_01pat_val",
          "feature_pred_tax_npc_01pat_score",
          "feature_pred_tax_npc_02sup_val",
          "feature_pred_tax_npc_02sup_score",
          "feature_pred_tax_npc_03cla_val",
          "feature_pred_tax_npc_03cla_score",
          "component_id",
          "candidate_structure_molecular_formula",
          "candidate_structure_exact_mass",
          "candidate_structure_xlogp",
          "candidate_structure_inchikey_no_stereo",
          "candidate_structure_smiles_no_stereo",
          "candidate_structure_name",
          "candidate_structure_tax_cla",
          "candidate_structure_tax_npc",
          "candidate_structure_organism_occurrence_closest" = "best_can_org",
          "candidate_structure_organism_occurrence_reference" = "reference_doi",
          "candidate_library",
          ## TODO "library_type",
          "candidate_count_similarity_peaks_matched",
          "candidate_score_similarity",
          "candidate_count_sirius_peaks_explained",
          "candidate_score_sirius_intensity",
          "candidate_score_sirius_isotope",
          "candidate_score_sirius_sirius",
          "candidate_score_sirius_tree",
          "candidate_score_sirius_zodiac",
          "candidate_score_sirius_confidence",
          "candidate_score_sirius_csi",
          "candidate_structure_error_mz",
          "candidate_structure_error_rt",
          "rank_initial",
          "rank_final",
          "score_initial",
          "score_biological",
          # "score_interim",
          "score_chemical",
          "score_final"
        )
      ))
    rm(df5)

    log_debug("adding consensus again to droped candidates \n")
    results <- tidytable::bind_rows(
      df6 |>
        tidytable::filter(!is.na(candidate_structure_inchikey_no_stereo)),
      tidytable::left_join(
        df6 |>
          tidytable::filter(is.na(candidate_structure_inchikey_no_stereo)),
        annot_table_wei_chemo |>
          tidytable::mutate(tidytable::across(
            .cols = tidytable::everything(),
            .fns = as.character
          ))
      ) |>
        tidytable::select(tidytable::any_of(
          c(
            "feature_id",
            "feature_rt",
            "feature_mz",
            "feature_spectrum_entropy",
            "feature_pred_tax_cla_01kin_val",
            "feature_pred_tax_cla_01kin_score",
            "feature_pred_tax_cla_02sup_val",
            "feature_pred_tax_cla_02sup_score",
            "feature_pred_tax_cla_03cla_val",
            "feature_pred_tax_cla_03cla_score",
            "feature_pred_tax_cla_04dirpar_val",
            "feature_pred_tax_cla_04dirpar_score",
            "feature_pred_tax_npc_01pat_val",
            "feature_pred_tax_npc_01pat_score",
            "feature_pred_tax_npc_02sup_val",
            "feature_pred_tax_npc_02sup_score",
            "feature_pred_tax_npc_03cla_val",
            "feature_pred_tax_npc_03cla_score",
            "component_id"
          )
        )) |>
        tidytable::distinct()
    ) |>
      tidytable::arrange(as.numeric(feature_id))

    rm(annot_table_wei_chemo, df6)

    ## Because cytoscape import fails otherwise
    colnames(results) <-
      stringi::stri_replace_all_fixed(
        str = colnames(results),
        pattern = "_structure",
        replacement = "",
        vectorize_all = FALSE
      )

    return(results)
  }
