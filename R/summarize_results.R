#' @title Summarize annotation results
#'
#' @description Summarizes annotation results by adding feature metadata,
#'     filtering/collapsing columns, and optionally removing tied scores or
#'     summarizing to one row per feature. Creates final results table.
#'
#' @include columns_utils.R
#' @include validations_utils.R
#'
#' @param df Data frame containing weighted annotation results
#' @param features_table Data frame with feature metadata (RT, m/z, etc.)
#' @param components_table Data frame with network component assignments
#' @param structure_organism_pairs_table Data frame with structure-organism pairs
#' @param annot_table_wei_chemo Data frame with chemically weighted annotations
#' @param remove_ties Logical whether to remove tied scores (keep only highest)
#' @param summarize Logical whether to collapse to 1 row per feature
#'
#' @return Data frame containing summarized annotation results
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' summary <- summarize_results(
#'   df = annotations,
#'   features_table = features,
#'   components_table = components,
#'   structure_organism_pairs_table = sop,
#'   annot_table_wei_chemo = weighted_chemo,
#'   remove_ties = TRUE,
#'   summarize = TRUE
#' )
#' }
summarize_results <- function(
  df,
  features_table,
  components_table,
  structure_organism_pairs_table,
  annot_table_wei_chemo,
  remove_ties,
  summarize,
  annotation_notes_lookup = NULL
) {
  # Input Validation ----
  validate_dataframe(df, param_name = "df")
  validate_dataframe(features_table, param_name = "features_table")
  validate_dataframe(components_table, param_name = "components_table")
  validate_dataframe(
    structure_organism_pairs_table,
    param_name = "structure_organism_pairs_table"
  )
  validate_dataframe(
    annot_table_wei_chemo,
    param_name = "annot_table_wei_chemo"
  )
  validate_logical(remove_ties, param_name = "remove_ties")
  validate_logical(summarize, param_name = "summarize")

  # Early exit for empty results
  if (nrow(df) == 0L) {
    msg <- "Empty results table provided"
    warning(msg, call. = FALSE)
    log_warn(msg)
    return(df)
  }

  log_info("Summarizing annotation results")
  log_debug("Remove ties: %s, Summarize: %s", remove_ties, summarize)

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
    "score_final" = "score_weighted_chemo"
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

  # Add annotation_note from lookup table at the very end
  if (!is.null(annotation_notes_lookup) && nrow(annotation_notes_lookup) > 0) {
    df_joined <- df_joined |>
      tidytable::left_join(
        y = annotation_notes_lookup,
        by = c("feature_id", "candidate_adduct", "rank_final")
      )
  }

  rm(df, organism_lookup)
  # gc()

  # Remove ties if requested
  if (remove_ties == TRUE) {
    log_info("Removing ties")
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
          .cols = tidyselect::all_of(x = collapse_cols),
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
            tidyselect::all_of(x = remaining_cols)
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
      tidytable::distinct(tidyselect::all_of(x = model$features_columns)) |>
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
    tidytable::arrange(
      # Try to sort numerically if possible, otherwise alphabetically
      suppressWarnings(as.numeric(feature_id))
    ) |>
    tidytable::select(tidyselect::where(fn = ~ any(!is.na(.))))

  rm(df_processed)

  return(results)
}
