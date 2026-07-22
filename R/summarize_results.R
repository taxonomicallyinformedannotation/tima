#' @title Summarize annotation results
#'
#' @description Summarizes annotation results by adding feature metadata,
#'     filtering/collapsing columns, and optionally removing tied scores or
#'     summarizing to one row per feature. Creates final results table.
#'
#' @include columns_utils.R
#' @include validations_utils.R
#'
#' @param df [data.frame] Data frame containing weighted annotation results
#' @param features_table [data.frame] Data frame with feature metadata (RT, m/z,
#'     etc.)
#' @param components_table [data.frame] Data frame with network component
#'     assignments
#' @param structure_organism_pairs_table [data.frame] Data frame with
#'     structure-organism pairs
#' @param annot_table_wei_chemo [data.frame] Data frame with chemically weighted
#'     annotations. Used to build feature-level consensus metadata for features
#'     without candidate structures.
#' @param remove_ties [logical] Logical whether to remove tied scores (keep only
#'     highest)
#' @param summarize [logical] Logical whether to collapse to 1 row per feature
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
  annot_table_wei_chemo = NULL,
  remove_ties,
  summarize,
  annotation_notes_lookup = NULL,
  # Optional pre-built output of .build_feature_consensus_table(). Callers
  # that invoke summarize_results() more than once against the SAME
  # annot_table_wei_chemo (e.g. clean_chemo()'s filtered- and full-tier
  # calls) can build this once and pass it in both times, instead of this
  # function rebuilding an identical table from scratch on every call.
  # When supplied, annot_table_wei_chemo is not needed for this purpose.
  feature_consensus_table = NULL
) {
  # Input Validation ----
  validate_dataframe(df, param_name = "df")
  validate_dataframe(features_table, param_name = "features_table")
  validate_dataframe(components_table, param_name = "components_table")
  validate_dataframe(
    structure_organism_pairs_table,
    param_name = "structure_organism_pairs_table"
  )
  if (!is.null(annot_table_wei_chemo)) {
    validate_dataframe(
      annot_table_wei_chemo,
      param_name = "annot_table_wei_chemo"
    )
  }
  validate_logical(remove_ties, param_name = "remove_ties")
  validate_logical(summarize, param_name = "summarize")

  # Early exit for empty results
  if (nrow(df) == 0L) {
    msg <- "Empty results table provided"
    warning(msg, call. = FALSE)
    log_warn(msg)

    return(df[0, , drop = FALSE])
  }

  log_info("Summarizing annotation results")
  log_debug("Remove ties: %s, Summarize: %s", remove_ties, summarize)

  model <- columns_model()
  candidate_id_cols <- grep(
    pattern = "^candidate_structure_id_",
    x = names(df),
    value = TRUE
  )

  if (!"reference_doi" %in% names(structure_organism_pairs_table)) {
    structure_organism_pairs_table <- structure_organism_pairs_table |>
      tidytable::mutate(reference_doi = NA_character_)
  }

  organism_lookup <- if (
    nrow(structure_organism_pairs_table) > 0L &&
      nrow(df) > 0L &&
      "candidate_structure_inchikey_connectivity_layer" %in% names(df) &&
      any(
        c(
          "candidate_structure_inchikey_connectivity_layer",
          "candidate_structure_organism_occurrence_closest"
        ) %in%
          names(df)
      )
  ) {
    .build_organism_lookup(
      structure_organism_pairs_table = structure_organism_pairs_table,
      df = df
    )
  } else {
    structure_organism_pairs_table[0L, ] |>
      tidytable::select(
        candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
        reference_doi,
        tidyselect::contains(match = "organism_taxonomy_"),
        -tidyselect::any_of("organism_taxonomy_ottid")
      ) |>
      tidytable::mutate(
        feature_id = character(),
        candidate_structure_organism_occurrence_closest = character(),
        candidate_structure_organism_occurrence_reference = character()
      )
  }

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
    candidate_id_cols,
    model$rank_columns,
    "score_initial" = "candidate_score_pseudo_initial",
    "score_biological",
    "score_interim" = "score_weighted_bio",
    "score_weighted_bio_coverage",
    "score_chemical",
    "score_weighted_chemo",
    "score_weighted_chemo_coverage",
    "score_final" = "score_weighted_chemo"
  )

  final_select_cols <- unique(c(
    model$features_columns,
    model$features_calculated_columns,
    model$components_columns,
    model$candidates_calculated_columns,
    model$candidates_sirius_for_columns,
    model$candidates_sirius_str_columns,
    model$candidates_spectra_columns,
    model$candidates_structures_columns,
    candidate_id_cols,
    model$rank_columns,
    model$score_columns,
    "score_weighted_chemo",
    "score_final",
    # Include annotation notes (if available from sampling/filtering operations)
    "annotation_note"
  ))

  features_min <- features_table |>
    tidytable::select(
      tidyselect::any_of(c("feature_id", "rt", "mz"))
    ) |>
    tidytable::distinct(feature_id, .keep_all = TRUE)

  components_min <- components_table |>
    tidytable::select(
      tidyselect::any_of(c("feature_id", model$components_columns))
    ) |>
    tidytable::distinct(feature_id, .keep_all = TRUE)

  # Start from annotation rows and add compact per-feature metadata.
  df_joined <- df |>
    tidytable::select(tidyselect::any_of(x = select_cols))

  if (nrow(df_joined) > 0L) {
    df_joined <- df_joined |>
      tidytable::left_join(
        y = features_min,
        by = "feature_id",
        suffix = c("", "_feature")
      ) |>
      tidytable::left_join(
        y = components_min,
        by = "feature_id",
        suffix = c("", "_component")
      ) |>
      tidytable::left_join(
        y = organism_lookup,
        by = c(
          "feature_id",
          "candidate_structure_inchikey_connectivity_layer",
          "candidate_structure_organism_occurrence_closest"
        )
      ) |>
      tidytable::select(tidyselect::any_of(x = final_select_cols))
  } else {
    df_joined <- df_joined |>
      tidytable::select(tidyselect::any_of(x = final_select_cols))
  }

  # Add annotation_note from lookup table at the very end
  # Note: annotation_notes_lookup has been pre-collapsed to ensure
  # one note per (feature_id, candidate_adduct, rank_final) group
  if (!is.null(annotation_notes_lookup) && nrow(annotation_notes_lookup) > 0) {
    note_join_cols <- intersect(
      c("feature_id", "candidate_adduct", "rank_final"),
      names(annotation_notes_lookup)
    )
    note_join_cols <- intersect(note_join_cols, names(df_joined))
    if (length(note_join_cols) > 0L) {
      df_joined <- df_joined |>
        tidytable::left_join(
          y = annotation_notes_lookup,
          by = note_join_cols
        )
    }
  }

  rm(df, organism_lookup)

  # Remove ties if requested
  if (remove_ties) {
    log_info("Removing ties")
    df_joined <- df_joined |>
      tidytable::distinct(feature_id, rank_final, .keep_all = TRUE)
  }

  # Summarize if requested
  if (summarize) {
    if (nrow(df_joined) == 0L) {
      df_final <- df_joined
    } else {
      rank_num <- suppressWarnings(as.integer(df_joined$rank_final))

      score_final_num <- if ("score_final" %in% names(df_joined)) {
        -suppressWarnings(as.numeric(df_joined$score_final))
      } else if ("score_weighted_chemo" %in% names(df_joined)) {
        -suppressWarnings(as.numeric(df_joined$score_weighted_chemo))
      } else {
        rep_len(0, nrow(df_joined))
      }

      coverage_num <- if (
        "score_weighted_chemo_coverage" %in% names(df_joined)
      ) {
        -suppressWarnings(as.numeric(df_joined$score_weighted_chemo_coverage))
      } else {
        rep_len(0, nrow(df_joined))
      }

      initial_num <- if ("score_initial" %in% names(df_joined)) {
        -suppressWarnings(as.numeric(df_joined$score_initial))
      } else if ("candidate_score_pseudo_initial" %in% names(df_joined)) {
        -suppressWarnings(as.numeric(df_joined$candidate_score_pseudo_initial))
      } else {
        rep_len(0, nrow(df_joined))
      }

      ord <- order(
        df_joined$feature_id,
        rank_num,
        score_final_num,
        coverage_num,
        initial_num,
        na.last = TRUE
      )
      keep <- !duplicated(df_joined$feature_id[ord])
      df_final <- df_joined[ord[keep], , drop = FALSE]

      rm(ord, keep, rank_num, score_final_num, coverage_num, initial_num)
    }

    rm(df_joined)
  } else {
    df_final <- df_joined
    rm(df_joined)
  }

  char_cols <- names(df_final)[vapply(df_final, is.character, logical(1L))]
  factor_cols <- names(df_final)[vapply(df_final, is.factor, logical(1L))]

  df_final <- df_final |>
    tidytable::mutate(
      tidytable::across(
        .cols = tidyselect::all_of(char_cols),
        .fns = ~ tidytable::na_if(x = trimws(.x), y = "")
      ),
      tidytable::across(
        .cols = tidyselect::all_of(factor_cols),
        .fns = ~ tidytable::na_if(x = trimws(as.character(.x)), y = "")
      )
    )

  df_processed <- df_final |>
    tidytable::select(tidyselect::any_of(x = final_select_cols))

  rm(df_final)

  # Handle features without candidate structures
  # Optimization: Do this in one pass with bind_rows
  has_structure <- !is.na(
    df_processed$candidate_structure_inchikey_connectivity_layer
  )

  results_with_structure <- df_processed[has_structure, , drop = FALSE]
  results_without_structure <- if (!all(has_structure)) {
    feature_consensus_table <- feature_consensus_table %||%
      .build_feature_consensus_table(
        annot_table_wei_chemo = annot_table_wei_chemo,
        model = model
      )
    df_processed[!has_structure, , drop = FALSE] |>
      tidytable::distinct(tidyselect::any_of(x = model$features_columns)) |>
      tidytable::left_join(y = feature_consensus_table) |>
      tidytable::select(
        tidyselect::any_of(
          x = c(
            model$features_columns,
            model$features_calculated_columns,
            model$components_columns
            ## Do not keep it if no structure
            # "annotation_note"
          )
        )
      ) |>
      tidytable::distinct()
  } else {
    tidytable::tidytable()
  }

  results <- tidytable::bind_rows(
    results_with_structure,
    results_without_structure
  ) |>
    tidytable::arrange(
      # Sort numerically when feature_id is numeric, otherwise alphabetically
      suppressWarnings(as.numeric(feature_id))
    ) |>
    tidytable::select(
      tidyselect::where(fn = ~ !all(is.na(.)))
    )

  rm(df_processed)

  # Log percentage of annotated features against the full feature universe.
  # The summarized output is one row per feature and can omit unannotated
  # features from the intermediate table, so the denominator must come from the
  # input feature table whenever that is available.
  annotation_coverage <- .count_annotated_features(
    results = results,
    features_table = features_table,
    feature_id_col = "feature_id",
    annotation_col = "candidate_structure_inchikey_connectivity_layer"
  )
  log_info(
    "Annotated features: %d/%d (%.1f%%)",
    annotation_coverage$annotated_features,
    annotation_coverage$total_features,
    annotation_coverage$pct_annotated
  )

  results
}

.count_annotated_features <- function(
  results,
  features_table = NULL,
  feature_id_col = "feature_id",
  annotation_col = "candidate_structure_inchikey_connectivity_layer"
) {
  total_feature_ids <- if (
    !is.null(features_table) && nrow(features_table) > 0L
  ) {
    as.character(features_table[[feature_id_col]])
  } else if (!is.null(results) && nrow(results) > 0L) {
    as.character(results[[feature_id_col]])
  } else {
    character(0)
  }
  total_features <- length(unique(total_feature_ids))

  annotated_features <- 0L
  if (!is.null(results) && nrow(results) > 0L) {
    has_annotation_col <- annotation_col %in% names(results)
    if (has_annotation_col) {
      annotated_ids <- results[[feature_id_col]][
        !is.na(results[[annotation_col]]) &
          nzchar(trimws(as.character(results[[annotation_col]])))
      ]
      if (length(annotated_ids) > 0L) {
        annotated_ids <- unique(as.character(annotated_ids))
        if (length(total_feature_ids) > 0L) {
          annotated_features <- sum(total_feature_ids %in% annotated_ids)
        } else {
          annotated_features <- length(annotated_ids)
        }
      }
    }
  }

  pct_annotated <- if (total_features > 0L) {
    100 * annotated_features / total_features
  } else {
    0
  }

  list(
    total_features = total_features,
    annotated_features = annotated_features,
    pct_annotated = pct_annotated
  )
}

.build_organism_lookup <- function(structure_organism_pairs_table, df) {
  # Check if the required columns exist in df
  has_organism_col <- "candidate_structure_organism_occurrence_closest" %in%
    names(df)

  empty_lookup <- function() {
    structure_organism_pairs_table[0L, ] |>
      tidytable::select(
        candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
        reference_doi,
        tidyselect::contains(match = "organism_taxonomy_"),
        -tidyselect::any_of("organism_taxonomy_ottid")
      ) |>
      tidytable::mutate(
        feature_id = character(),
        candidate_structure_organism_occurrence_closest = character(),
        candidate_structure_organism_occurrence_reference = character()
      )
  }

  if (!has_organism_col) {
    return(empty_lookup())
  }

  candidate_structure_ids <- unique(
    stats::na.omit(as.character(
      df$candidate_structure_inchikey_connectivity_layer
    ))
  )
  if (length(candidate_structure_ids) == 0L) {
    return(empty_lookup())
  }

  # Narrow FIRST, before any join or reshape.
  sop_narrow <- structure_organism_pairs_table |>
    tidytable::filter(
      structure_inchikey_connectivity_layer %in% candidate_structure_ids
    ) |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
      reference_doi,
      tidyselect::contains(match = "organism_taxonomy_"),
      -tidyselect::any_of("organism_taxonomy_ottid")
    ) |>
    tidytable::distinct()

  if (nrow(sop_narrow) == 0L) {
    return(empty_lookup())
  }

  taxonomy_cols <- grep("^organism_taxonomy_", names(sop_narrow), value = TRUE)

  # Melt ONCE on the already-narrowed table, instead of a 10-way OR filter
  # over a full inchikey-only cartesian join. values_drop_na keeps this from
  # ballooning on the many taxonomy ranks that are typically NA (species,
  # varietas, etc.).
  sop_long <- tidytable::pivot_longer(
    sop_narrow,
    cols = tidyselect::all_of(taxonomy_cols),
    names_to = ".taxonomy_rank",
    values_to = "candidate_structure_organism_occurrence_closest",
    values_drop_na = TRUE
  ) |>
    tidytable::select(-".taxonomy_rank") |>
    tidytable::distinct()

  # Bound the join output by df's own key combos rather than by however many
  # organism records a popular structure happens to carry.
  df_keys <- df |>
    tidytable::distinct(
      feature_id,
      candidate_structure_organism_occurrence_closest,
      candidate_structure_inchikey_connectivity_layer
    )

  sop_long |>
    tidytable::inner_join(
      y = df_keys,
      by = c(
        "candidate_structure_inchikey_connectivity_layer",
        "candidate_structure_organism_occurrence_closest"
      )
    ) |>
    tidytable::distinct(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      candidate_structure_organism_occurrence_closest,
      candidate_structure_organism_occurrence_reference = reference_doi,
      .keep_all = TRUE
    ) |>
    tidytable::group_by(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      candidate_structure_organism_occurrence_closest
    ) |>
    clean_collapse(
      cols = "candidate_structure_organism_occurrence_reference"
    ) |>
    tidytable::ungroup()
}

.build_feature_consensus_table <- function(annot_table_wei_chemo, model) {
  base_cols <- c(
    model$features_columns,
    "rt",
    "mz",
    model$features_calculated_columns,
    model$components_columns,
    "annotation_note"
  )

  consensus_src <- annot_table_wei_chemo |>
    tidytable::select(
      tidyselect::any_of(c(base_cols, "score_weighted_chemo", "rank_final"))
    )

  if (!"annotation_note" %in% names(consensus_src)) {
    consensus_src$annotation_note <- NA_character_
  }
  if (!"score_weighted_chemo" %in% names(consensus_src)) {
    consensus_src$score_weighted_chemo <- NA_real_
  }
  if (!"rank_final" %in% names(consensus_src)) {
    consensus_src$rank_final <- NA_real_
  }

  consensus_src$.note_present <- !is.na(consensus_src$annotation_note) &
    nzchar(trimws(as.character(consensus_src$annotation_note)))
  consensus_src$.score_num <- suppressWarnings(
    as.numeric(consensus_src$score_weighted_chemo)
  )
  consensus_src$.rank_num <- suppressWarnings(
    as.numeric(consensus_src$rank_final)
  )
  consensus_src$.score_present <- !is.na(consensus_src$.score_num)

  consensus_src <- as.data.frame(consensus_src, stringsAsFactors = FALSE)

  ord <- with(
    consensus_src,
    order(
      feature_id,
      -as.integer(.note_present),
      -as.integer(.score_present),
      -.score_num,
      .rank_num,
      na.last = TRUE
    )
  )

  consensus_src <- consensus_src[ord, , drop = FALSE]
  consensus_src <- consensus_src[
    !duplicated(consensus_src$feature_id),
    ,
    drop = FALSE
  ]

  consensus_src[, intersect(base_cols, names(consensus_src)), drop = FALSE]
}
