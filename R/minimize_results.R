#' @title Minimize results
#'
#' @description This function minimizes annotation results by filtering to the
#'     most confident chemical classifications and compound identifications.
#'     It extracts the best-scoring chemical taxonomy predictions and the top
#'     compound candidates based on weighted chemical scores.
#'
#' @param df Data frame containing full annotation results with scores and
#'     chemical taxonomy predictions
#' @param features_table Data frame of features to join with minimized results
#' @param min_score_classes Numeric minimum score threshold (0-1) for keeping
#'     chemical classifications (default: 0.6)
#' @param min_score_compounds Numeric minimum score threshold (0-1) for keeping
#'     compound identifications (default: 0.4)
#' @param best_percentile Numeric percentile threshold (0-1) for selecting top
#'     candidates within each feature (default: 0.9, keeps candidates with scores
#'     >= 90% of the maximum score for that feature)
#'
#' @return A minimized data frame with features joined to their best chemical
#'     classifications and top compound identifications
#'
#' @examples NULL
minimize_results <- function(
  df,
  features_table,
  min_score_classes = 0.6,
  min_score_compounds = 0.4,
  best_percentile = 0.9
) {
  # Validate inputs
  if (!is.data.frame(df) && !inherits(df, "tbl")) {
    stop("Input 'df' must be a data frame or tibble")
  }

  if (!is.data.frame(features_table) && !inherits(features_table, "tbl")) {
    stop("Input 'features_table' must be a data frame or tibble")
  }

  if (nrow(df) == 0L) {
    logger::log_warn("Empty annotation results provided")
    # Return features_table with empty candidate columns to maintain structure
    return(
      features_table |>
        tidytable::mutate(
          candidates_evaluated = NA_integer_,
          candidates_best = NA_integer_
        )
    )
  }

  # Validate score thresholds
  if (min_score_classes < 0 || min_score_classes > 1) {
    stop("min_score_classes must be between 0 and 1")
  }

  if (min_score_compounds < 0 || min_score_compounds > 1) {
    stop("min_score_compounds must be between 0 and 1")
  }

  if (best_percentile < 0 || best_percentile > 1) {
    stop("best_percentile must be between 0 and 1")
  }

  logger::log_trace("Minimizing annotation results")
  logger::log_debug(
    "Thresholds - Classes: ",
    min_score_classes,
    ", Compounds: ",
    min_score_compounds,
    ", Percentile: ",
    best_percentile
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
        tidyselect::any_of(
          c(
            "feature_id",
            "label_classyfire" = "cla",
            "label_npclassifier" = "npc"
          )
        )
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
