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

  model <- columns_model()

  # Pre-process organism taxonomy data
  organism_lookup <- structure_organism_pairs_table |>
    tidytable::filter(
      structure_inchikey_connectivity_layer %in%
        df$candidate_structure_inchikey_connectivity_layer
    ) |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
      reference_doi,
      tidyselect::contains(match = "organism_taxonomy_"),
      -organism_taxonomy_ottid
    ) |>
    tidytable::distinct() |>
    tidytable::inner_join(
      y = df |>
        tidytable::distinct(
          candidate_structure_organism_occurrence_closest,
          candidate_structure_inchikey_connectivity_layer
        )
    ) |>
    tidytable::filter(
      candidate_structure_organism_occurrence_closest ==
        organism_taxonomy_01domain |
        candidate_structure_organism_occurrence_closest ==
          organism_taxonomy_02kingdom |
        candidate_structure_organism_occurrence_closest ==
          organism_taxonomy_03phylum |
        candidate_structure_organism_occurrence_closest ==
          organism_taxonomy_04class |
        candidate_structure_organism_occurrence_closest ==
          organism_taxonomy_05order |
        candidate_structure_organism_occurrence_closest ==
          organism_taxonomy_06family |
        candidate_structure_organism_occurrence_closest ==
          organism_taxonomy_07tribe |
        candidate_structure_organism_occurrence_closest ==
          organism_taxonomy_08genus |
        candidate_structure_organism_occurrence_closest ==
          organism_taxonomy_09species |
        candidate_structure_organism_occurrence_closest ==
          organism_taxonomy_10varietas
    ) |>
    tidytable::distinct(
      candidate_structure_inchikey_connectivity_layer,
      candidate_structure_organism_occurrence_closest,
      candidate_structure_organism_occurrence_reference = reference_doi,
      .keep_all = TRUE
    ) |>
    tidytable::group_by(
      candidate_structure_inchikey_connectivity_layer,
      candidate_structure_organism_occurrence_closest
    ) |>
    clean_collapse(
      cols = c("candidate_structure_organism_occurrence_reference")
    ) |>
    tidytable::ungroup()

  # Define columns to select
  select_cols <- c(
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
    "score_final" = "score_weighted_chemo",
    # TODO
    "annotation_note"
  )

  final_select_cols <- c(
    model$features_columns,
    model$features_calculated_columns,
    model$components_columns,
    model$candidates_calculated_columns,
    model$candidates_sirius_for_columns,
    model$candidates_sirius_str_columns,
    model$candidates_spectra_columns,
    model$candidates_structures_columns,
    model$rank_columns,
    model$score_columns,
    # TODO
    "annotation_note"
  )

  # Join smaller tables first, then select columns early
  # This reduces the amount of data carried through the pipeline
  df_joined <- features_table |>
    tidytable::left_join(y = components_table) |>
    tidytable::left_join(y = df) |>
    tidytable::select(tidyselect::any_of(x = select_cols)) |>
    tidytable::distinct() |>
    tidytable::left_join(y = organism_lookup) |>
    tidytable::select(tidyselect::any_of(x = final_select_cols)) |>
    tidytable::arrange(rank_final)

  rm(df, organism_lookup)
  # gc()

  # Remove ties if requested
  if (remove_ties == TRUE) {
    logger::log_info("Removing ties")
    df_joined <- df_joined |>
      tidytable::distinct(c(feature_id, rank_final), .keep_all = TRUE)
  }

  # Summarize if requested
  if (summarize == TRUE) {
    # gc()

    # Get column names that match the pattern once
    collapse_cols <- colnames(df_joined)[grepl(
      pattern = "^candidate|^rank|^score",
      x = colnames(df_joined),
      perl = TRUE
    )]

    # Optimization: Use more efficient collapse function
    collapse_fn <- function(x) {
      gsub(
        pattern = "\\bNA\\b",
        replacement = "",
        x = paste(x, collapse = "|"),
        perl = TRUE
      )
    }

    df_summarized <- df_joined |>
      tidytable::group_by(feature_id) |>
      tidytable::reframe(
        tidytable::across(
          .cols = tidyselect::all_of(collapse_cols),
          .fns = collapse_fn
        )
      ) |>
      tidytable::ungroup()

    # Get remaining columns (non-collapsed)
    remaining_cols <- setdiff(
      colnames(df_joined),
      c("feature_id", collapse_cols)
    )

    df_final <- df_summarized |>
      tidytable::left_join(
        y = df_joined |>
          tidytable::select(c(
            "feature_id",
            tidyselect::all_of(remaining_cols)
          )) |>
          tidytable::distinct()
      )

    rm(df_summarized, df_joined)
  } else {
    df_final <- df_joined
    rm(df_joined)
  }

  # gc()

  # Final processing: convert to character, trim, handle NAs
  df_processed <- df_final |>
    tidytable::mutate(
      tidytable::across(
        .cols = tidyselect::everything(),
        .fns = as.character
      )
    ) |>
    tidytable::mutate(
      tidytable::across(
        .cols = tidyselect::where(fn = is.character),
        .fns = ~ tidytable::na_if(x = trimws(.x), y = "")
      )
    ) |>
    tidytable::select(tidyselect::any_of(x = final_select_cols))

  rm(df_final)

  # Handle features without candidate structures
  # Optimization: Do this in one pass with bind_rows
  has_structure <- !is.na(
    df_processed$candidate_structure_inchikey_connectivity_layer
  )

  results <- tidytable::bind_rows(
    # Features with structures
    df_processed |>
      tidytable::filter(has_structure),

    # Features without structures - add consensus
    df_processed |>
      tidytable::filter(!has_structure) |>
      tidytable::distinct(tidyselect::all_of(model$features_columns)) |>
      tidytable::left_join(
        y = annot_table_wei_chemo |>
          tidytable::mutate(
            tidytable::across(
              .cols = tidyselect::everything(),
              .fns = as.character
            )
          )
      ) |>
      tidytable::select(
        tidyselect::any_of(
          x = c(
            model$features_columns,
            model$features_calculated_columns,
            model$components_columns,
            # TODO
            "annotation_note"
          )
        )
      ) |>
      tidytable::distinct()
  ) |>
    tidytable::arrange(as.numeric(feature_id)) |>
    tidytable::select(tidyselect::where(fn = ~ any(!is.na(.))))

  rm(df_processed)

  return(results)
}
