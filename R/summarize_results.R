#' @title Summarize results
#'
#' @description This function summarizes annotation results by adding feature
#'     metadata, filtering/collapsing columns, and optionally removing tied
#'     scores or summarizing to one row per feature. Creates a final, simplified
#'     results table for downstream analysis or reporting.
#'
#' @include clean_collapse.R
#' @include columns_model.R
#'
#' @param df Data frame containing weighted annotation results
#' @param annot_table_wei_chemo Data frame with chemically weighted annotations
#' @param components_table Data frame with molecular network component assignments
#' @param features_table Data frame with feature metadata (RT, m/z, etc.)
#' @param structure_organism_pairs_table Data frame with structure-organism pairs
#' @param remove_ties Logical whether to remove tied scores (keep only highest)
#' @param summarize Logical whether to collapse to 1 row per feature
#'
#' @return Data frame containing summarized annotation results
#'
#' @examples NULL
summarize_results <- function(
  df,
  features_table,
  components_table,
  structure_organism_pairs_table,
  annot_table_wei_chemo,
  remove_ties,
  summarize
) {
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  if (!is.data.frame(features_table)) {
    stop("features_table must be a data frame")
  }

  if (!is.data.frame(components_table)) {
    stop("components_table must be a data frame")
  }

  if (!is.data.frame(structure_organism_pairs_table)) {
    stop("structure_organism_pairs_table must be a data frame")
  }

  if (!is.data.frame(annot_table_wei_chemo)) {
    stop("annot_table_wei_chemo must be a data frame")
  }

  if (!is.logical(remove_ties)) {
    stop("remove_ties must be logical (TRUE/FALSE)")
  }

  if (!is.logical(summarize)) {
    stop("summarize must be logical (TRUE/FALSE)")
  }

  if (nrow(df) == 0L) {
    msg <- "Empty results table provided"
    warning(msg, call. = FALSE)
    logger::log_warn(msg)
    return(df)
  }

  logger::log_info("Summarizing annotation results")
  logger::log_debug(
    "Options - Remove ties: ",
    remove_ties,
    ", Summarize: ",
    summarize
  )

  # logger::log_trace("Adding feature metadata and simplifying columns")
  model <- columns_model()

  df3 <- features_table |>
    tidytable::left_join(df) |>
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
          candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
          reference_doi,
          organism_name,
          tidyselect::contains("organism_taxonomy_"),
          -organism_taxonomy_ottid
        ) |>
        tidytable::distinct() |>
        tidytable::pivot_longer(tidyselect::contains(
          "organism_taxonomy_"
        )) |>
        tidytable::filter(!is.na(value)) |>
        tidytable::filter(value != "notClassified") |>
        tidytable::distinct(
          candidate_structure_inchikey_connectivity_layer,
          candidate_structure_organism_occurrence_closest = value,
          candidate_structure_organism_occurrence_reference = reference_doi
        )
    ) |>
    tidytable::group_by(c(
      -candidate_structure_organism_occurrence_reference
    )) |>
    clean_collapse(
      cols = c("candidate_structure_organism_occurrence_reference")
    ) |>
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
  rm(df)

  if (remove_ties == TRUE) {
    logger::log_info("Removing ties")
    df3 <- df3 |>
      tidytable::distinct(c(feature_id, rank_final), .keep_all = TRUE)
  }

  if (summarize == TRUE) {
    # logger::log_trace("Collecting garbage")
    gc()
    # logger::log_trace("Summarizing results")
    df4 <- df3 |>
      tidytable::group_by(feature_id) |>
      tidytable::reframe(tidytable::across(
        .cols = colnames(df3)[grepl(
          pattern = "^candidate|^rank|^score",
          x = colnames(df3),
          perl = TRUE
        )],
        # TODO
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
      tidytable::left_join(
        df3 |>
          tidytable::select("feature_id", !colnames(df4)) |>
          tidytable::distinct()
      )
    rm(df4)
  } else {
    df5 <- df3
  }
  rm(df3)

  # logger::log_trace("Selecting columns to export")
  df6 <- df5 |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::everything(),
      .fns = as.character
    )) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.character),
      .fns = trimws
    )) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.character),
      .fns = ~ tidytable::na_if(.x, "")
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

  # logger::log_trace("Adding consensus again to droped candidates")
  results <- tidytable::bind_rows(
    df6 |>
      tidytable::filter(
        !is.na(
          candidate_structure_inchikey_connectivity_layer
        )
      ),
    tidytable::left_join(
      df6 |>
        tidytable::filter(is.na(
          candidate_structure_inchikey_connectivity_layer
        )) |>
        tidytable::distinct(model$features_columns),
      annot_table_wei_chemo |>
        tidytable::mutate(tidytable::across(
          .cols = tidyselect::everything(),
          .fns = as.character
        ))
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
    tidytable::arrange(as.numeric(feature_id)) |>
    tidytable::select(tidyselect::where(~ any(!is.na(.))))

  return(results)
}
