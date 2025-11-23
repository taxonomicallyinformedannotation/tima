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

  # Pre-process organism pairs separately to avoid huge pivot on main table
  if (nrow(structure_organism_pairs_table) > 0 &&
    "structure_inchikey_connectivity_layer" %in% colnames(structure_organism_pairs_table)) {
    org_pairs_processed <- structure_organism_pairs_table |>
      tidytable::select(
        candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
        reference_doi,
        organism_name,
        tidyselect::contains(match = "organism_taxonomy_"),
        -tidyselect::any_of("organism_taxonomy_ottid")
      ) |>
      tidytable::distinct() |>
      tidytable::pivot_longer(
        tidyselect::contains(match = "organism_taxonomy_")
      ) |>
      tidytable::filter(!is.na(value)) |>
      tidytable::filter(value != "notClassified") |>
      tidytable::distinct(
        candidate_structure_inchikey_connectivity_layer,
        candidate_structure_organism_occurrence_closest = value,
        candidate_structure_organism_occurrence_reference = reference_doi
      ) |>
      tidytable::group_by(
        candidate_structure_inchikey_connectivity_layer,
        candidate_structure_organism_occurrence_closest
      ) |>
      clean_collapse(cols = c("candidate_structure_organism_occurrence_reference")) |>
      tidytable::ungroup()
  } else {
    org_pairs_processed <- NULL
  }

  # Join only necessary columns, avoid selecting everything
  df3 <- features_table |>
    tidytable::left_join(y = df, by = "feature_id") |>
    tidytable::left_join(y = components_table, by = "feature_id")

  # MEMORY OPTIMIZATION 3: Rename columns without full select/distinct cycle
  if ("rt" %in% colnames(df3)) {
    df3 <- df3 |> tidytable::rename(feature_rt = rt)
  }
  if ("mz" %in% colnames(df3)) {
    df3 <- df3 |> tidytable::rename(feature_mz = mz)
  }
  if ("candidate_score_pseudo_initial" %in% colnames(df3)) {
    df3 <- df3 |> tidytable::rename(score_initial = candidate_score_pseudo_initial)
  }
  if ("score_weighted_bio" %in% colnames(df3)) {
    df3 <- df3 |> tidytable::rename(score_interim = score_weighted_bio)
  }
  if ("score_weighted_chemo" %in% colnames(df3)) {
    df3 <- df3 |> tidytable::rename(score_final = score_weighted_chemo)
  }

  # Add organism pairs if available
  if (!is.null(org_pairs_processed)) {
    df3 <- df3 |>
      tidytable::left_join(
        y = org_pairs_processed,
        by = "candidate_structure_inchikey_connectivity_layer"
      )
  }

  rm(org_pairs_processed)
  # gc()

  # Select only needed columns
  df3 <- df3 |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
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
      )
    ) |>
    tidytable::distinct() |>
    tidytable::arrange(rank_final) |>
    tidytable::ungroup()

  rm(df)
  # gc()

  if (remove_ties == TRUE) {
    logger::log_info("Removing ties")
    df3 <- df3 |>
      tidytable::distinct(tidytable::across(c(feature_id, rank_final)), .keep_all = TRUE)
  }

  if (summarize == TRUE) {
    # logger::log_trace("Collecting garbage")
    gc()
    # logger::log_trace("Summarizing results")
    # Identify columns to collapse once
    candidate_cols <- grep(
      pattern = "^candidate|^rank|^score",
      x = colnames(df3),
      perl = TRUE,
      value = TRUE
    )

    # MEMORY OPTIMIZATION 5: Use more efficient summarization without reframe
    # Process in chunks if needed, and use data.table style for speed
    df4 <- df3 |>
      tidytable::group_by(feature_id) |>
      tidytable::summarise(
        tidytable::across(
          .cols = tidyselect::all_of(candidate_cols),
          # TODO
          .fns = function(x) {
            # Remove NAs before collapse to avoid memory bloat
            x_clean <- x[!is.na(x)]
            if (length(x_clean) == 0) {
              return(NA_character_)
            }
            result <- paste(x_clean, collapse = "|")
            # Remove standalone NA strings more efficiently
            gsub(pattern="\\bNA\\b", replacement="", x=result, perl = TRUE)
          }
        ),
        .groups = "drop"
      )

    # Get non-candidate columns (feature metadata)
    non_candidate_cols <- setdiff(colnames(df3), candidate_cols)
    df3_meta <- df3 |>
      tidytable::select(tidyselect::all_of(c("feature_id", non_candidate_cols))) |>
      tidytable::distinct()

    df5 <- df4 |>
      tidytable::left_join(y = df3_meta, by = "feature_id")

    rm(df4, df3_meta)
    # gc()
  } else {
    df5 <- df3
  }

  rm(df3)
  # gc()

  # Convert to character only at the end, and only selected columns
  # Identify which columns actually need character conversion
  char_candidate_cols <- grep("^candidate|^rank|^score", colnames(df5), value = TRUE, perl = TRUE)

  # Convert candidate columns to character using mutate to avoid data.table join confusion
  df5 <- df5 |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::all_of(char_candidate_cols),
      .fns = as.character
    ))

  # Trim and clean only character columns
  df5 <- df5 |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.character),
      .fns = ~ tidytable::na_if(trimws(.x), "")
    ))

  # Final column selection
  df6 <- df5 |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
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
      )
    )

  rm(df5)
  # gc()

  # Process consensus separately for dropped candidates
  df6_kept <- df6 |>
    tidytable::filter(!is.na(candidate_structure_inchikey_connectivity_layer))

  df6_dropped <- df6 |>
    tidytable::filter(is.na(candidate_structure_inchikey_connectivity_layer)) |>
    tidytable::distinct(tidyselect::all_of(model$features_columns))

  if (nrow(df6_dropped) > 0) {
    # Only convert necessary columns in annot table
    annot_small <- annot_table_wei_chemo |>
      tidytable::select(tidyselect::any_of(c(
        model$features_columns,
        model$features_calculated_columns,
        model$components_columns,
        # TODO
        "annotation_note"
      ))) |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::where(is.character),
        .fns = as.character
      ))

    df6_consensus <- tidytable::left_join(
      x = df6_dropped,
      y = annot_small,
      by = model$features_columns
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

    rm(annot_small)
  } else {
    df6_consensus <- df6_dropped
  }

  rm(df6_dropped)
  # gc()

  # Combine results
  results <- tidytable::bind_rows(df6_kept, df6_consensus) |>
    tidytable::arrange(as.numeric(feature_id)) |>
    tidytable::select(tidyselect::where(fn = ~ any(!is.na(.))))

  rm(df6_kept, df6_consensus, df6)
  # gc()

  return(results)
}