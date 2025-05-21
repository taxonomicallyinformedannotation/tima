#' @title Minimize results
#'
#' @description This function minimizes results
#'
#' @param df Dataframe
#' @param features_table Prepared features file
#' @param min_score_classes Minimal score to keep classes
#' @param min_score_compounds Minimal score to keep compounds
#' @param best_percentile Best percentile
#' @return A minimized table
#'
#' @examples NULL
minimize_results <- function(
  df,
  features_table,
  min_score_classes = 0.6,
  min_score_compounds = 0.4,
  best_percentile = 0.9
) {
  logger::log_trace(
    "Minimizing results"
  )
  df_classes <- df |>
    tidytable::select(tidytable::contains(c("feature_id", "feature_pred"))) |>
    tidytable::mutate(tidytable::across(
      tidytable::contains("score"),
      as.numeric
    )) |>
    tidytable::filter(feature_pred_tax_npc_01pat_val != "empty")
  if (
    df_classes |>
      nrow() ==
      0L
  ) {
    df_classes <- tidytable::tidytable(
      feature_id = NA_integer_,
      label_classyfire = NA_character_,
      label_npclassifier = NA_character_
    )
  } else {
    df_classes <- df_classes |>
      tidytable::group_by(feature_id) |>
      tidytable::pivot_longer(
        cols = tidytable::contains("feature_pred_tax"),
        names_prefix = "feature_pred_tax"
      ) |>
      tidytable::mutate(
        names = name |>
          stringi::stri_replace_all_regex(
            pattern = ".*_",
            replacement = "",
            vectorize_all = FALSE
          )
      ) |>
      tidytable::mutate(
        values = name |>
          stringi::stri_replace_all_fixed(
            pattern = names,
            replacement = "",
            vectorize_all = TRUE
          )
      ) |>
      tidytable::group_by(feature_id, values) |>
      tidytable::select(-name) |>
      tidytable::distinct() |>
      tidytable::pivot_wider(names_from = names, values_from = value) |>
      tidytable::mutate(
        score = score |>
          as.numeric()
      ) |>
      tidytable::filter(score > 0) |>
      tidytable::mutate(
        tax = tidytable::if_else(
          condition = grepl(pattern = "npc", x = values),
          true = "npc",
          false = "cla"
        )
      ) |>
      tidytable::arrange(tidytable::desc(values)) |>
      tidytable::arrange(tidytable::desc(score)) |>
      tidytable::distinct(feature_id, tax, .keep_all = TRUE) |>
      tidytable::filter(score >= min_score_classes) |>
      tidytable::select(-values, -score) |>
      tidytable::group_by(feature_id) |>
      tidytable::pivot_wider(names_from = tax, values_from = val) |>
      tidytable::select(
        feature_id,
        label_classyfire = cla,
        label_npclassifier = npc
      ) |>
      tidytable::distinct()
  }

  df_compounds <- df |>
    tidytable::select(
      feature_id,
      candidate_structure_organism_occurrence_closest,
      candidate_structure_inchikey_connectivity_layer,
      candidate_structure_smiles_no_stereo,
      candidate_structure_name,
      candidate_structure_error_mz,
      candidate_structure_error_rt,
      candidate_library,
      candidate_adduct,
      score_weighted_chemo
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(feature_id) |>
    tidytable::add_count(name = "candidates_evaluated") |>
    tidytable::filter(
      score_weighted_chemo >= best_percentile * max(score_weighted_chemo)
    ) |>
    tidytable::add_count(name = "candidates_best") |>
    tidytable::arrange(tidytable::desc(score_weighted_chemo)) |>
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::select(
      feature_id,
      label_compound = candidate_structure_name,
      adduct = candidate_adduct,
      smiles_no_stereo = candidate_structure_smiles_no_stereo,
      inchikey_connectivity_layer = candidate_structure_inchikey_connectivity_layer,
      library = candidate_library,
      error_mz = candidate_structure_error_mz,
      error_rt = candidate_structure_error_rt,
      organism_closest = candidate_structure_organism_occurrence_closest,
      score = score_weighted_chemo,
      candidates_evaluated,
      candidates_best
    ) |>
    tidytable::distinct()

  df_compounds_conf <- df_compounds |>
    tidytable::filter(score >= min_score_compounds)

  df_compounds_left <- df_compounds |>
    tidytable::anti_join(df_compounds_conf) |>
    tidytable::distinct(
      feature_id,
      candidates_evaluated,
      candidates_best
    )
  rm(df_compounds)

  df_mini <- features_table |>
    tidytable::left_join(df_classes) |>
    tidytable::left_join(df_compounds_conf) |>
    tidytable::left_join(df_compounds_left)

  return(df_mini)
}
