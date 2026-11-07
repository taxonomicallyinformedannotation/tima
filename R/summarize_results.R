#' @title Summarize annotation results
#'
#' @description Summarizes annotation results by adding feature metadata,
#'     filtering/collapsing columns, and optionally removing tied scores or
#'     summarizing to one row per feature. Creates final results table.
#'
#' @include columns_utils.R
#' @include validations_utils.R
#' @include logs_utils.R
#' @include adduct_universe.R
#' @include validations_params.R
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
    "score_chemical",
    "score_weighted_chemo",
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

  feat_min_cols <- intersect(c("feature_id", "rt", "mz"), names(features_table))
  features_min <- .fast_distinct_with_key_guard(
    tidytable::as_tidytable(as.list(features_table)[feat_min_cols]),
    "feature_id"
  )

  comp_min_cols <- intersect(
    c("feature_id", model$components_columns),
    names(components_table)
  )
  components_min <- .fast_distinct_with_key_guard(
    tidytable::as_tidytable(as.list(components_table)[comp_min_cols]),
    "feature_id"
  )

  # Start from annotation rows and add compact per-feature metadata.
  src_cols <- unname(select_cols)
  tgt_cols <- names(select_cols)
  if (is.null(tgt_cols)) {
    tgt_cols <- src_cols
  } else {
    empty_tgt <- is.na(tgt_cols) | !nzchar(tgt_cols)
    tgt_cols[empty_tgt] <- src_cols[empty_tgt]
  }
  keep_sel <- src_cols %in% names(df)
  src_present <- src_cols[keep_sel]
  tgt_present <- tgt_cols[keep_sel]
  first_src <- !duplicated(src_present)
  u_src <- src_present[first_src]
  u_tgt <- tgt_present[first_src]
  df_list <- as.list(df)[u_src]
  names(df_list) <- u_tgt
  df_joined <- tidytable::as_tidytable(df_list)
  rm(df_list)

  if (nrow(df_joined) > 0L) {
    fids_j <- df_joined$feature_id

    feat_add_cols <- setdiff(names(features_min), names(df_joined))
    if (length(feat_add_cols) > 0L && nrow(features_min) > 0L) {
      f_min_ids <- features_min$feature_id
      m_f <- if (anyNA(f_min_ids) || anyNA(fids_j)) {
        match(fids_j, f_min_ids)
      } else {
        fastmatch::fmatch(fids_j, f_min_ids)
      }
      for (col in feat_add_cols) {
        data.table::set(df_joined, j = col, value = features_min[[col]][m_f])
      }
    }

    comp_add_cols <- setdiff(names(components_min), names(df_joined))
    if (length(comp_add_cols) > 0L && nrow(components_min) > 0L) {
      c_min_ids <- components_min$feature_id
      m_c <- if (anyNA(c_min_ids) || anyNA(fids_j)) {
        match(fids_j, c_min_ids)
      } else {
        fastmatch::fmatch(fids_j, c_min_ids)
      }
      for (col in comp_add_cols) {
        data.table::set(df_joined, j = col, value = components_min[[col]][m_c])
      }
    }

    org_join_by <- c(
      "feature_id",
      "candidate_structure_inchikey_connectivity_layer",
      "candidate_structure_organism_occurrence_closest"
    )
    org_add_cols <- setdiff(names(organism_lookup), names(df_joined))
    if (
      length(org_add_cols) > 0L &&
        all(org_join_by %in% names(df_joined)) &&
        all(org_join_by %in% names(organism_lookup))
    ) {
      if (nrow(organism_lookup) == 0L) {
        for (col in org_add_cols) {
          data.table::set(
            df_joined,
            j = col,
            value = rep.int(
              organism_lookup[[col]][NA_integer_],
              nrow(df_joined)
            )
          )
        }
      } else {
        m_org <- .fast_match_rows(df_joined, organism_lookup, by = org_join_by)
        for (col in org_add_cols) {
          data.table::set(
            df_joined,
            j = col,
            value = organism_lookup[[col]][m_org]
          )
        }
      }
    }

    keep_j_cols <- intersect(final_select_cols, names(df_joined))
    drop_j_cols <- setdiff(names(df_joined), keep_j_cols)
    if (length(drop_j_cols) > 0L) {
      data.table::set(df_joined, j = drop_j_cols, value = NULL)
    }
    if (!identical(names(df_joined), keep_j_cols) && length(keep_j_cols) > 0L) {
      data.table::setcolorder(df_joined, keep_j_cols)
    }
  } else {
    df_joined <- df_joined |>
      tidytable::select(tidyselect::any_of(x = final_select_cols))
  }

  rm(features_min, components_min)

  # Add annotation_note from lookup table at the very end
  # Note: annotation_notes_lookup has been pre-collapsed to ensure
  # one note per (feature_id, candidate_adduct, rank_final) group
  if (!is.null(annotation_notes_lookup) && nrow(annotation_notes_lookup) > 0) {
    note_join_cols <- intersect(
      c(
        "feature_id",
        "candidate_adduct",
        "candidate_structure_inchikey_connectivity_layer",
        "rank_final"
      ),
      names(annotation_notes_lookup)
    )
    note_join_cols <- intersect(note_join_cols, names(df_joined))
    if (length(note_join_cols) > 0L) {
      note_add_cols <- setdiff(names(annotation_notes_lookup), names(df_joined))
      notes_key_df <- as.data.frame(
        as.list(annotation_notes_lookup)[note_join_cols]
      )
      if (anyDuplicated(notes_key_df) == 0L) {
        m_note <- .fast_match_rows(
          df_joined,
          annotation_notes_lookup,
          by = note_join_cols
        )
        for (col in note_add_cols) {
          data.table::set(
            df_joined,
            j = col,
            value = annotation_notes_lookup[[col]][m_note]
          )
        }
        if (
          "annotation_note" %in%
            names(df_joined) &&
            "annotation_note" %in% names(annotation_notes_lookup) &&
            !"annotation_note" %in% note_add_cols
        ) {
          cur_note <- as.character(df_joined$annotation_note)
          lkp_note <- as.character(annotation_notes_lookup$annotation_note[
            m_note
          ])
          fill_na <- (is.na(cur_note) | !nzchar(trimws(cur_note))) &
            !is.na(lkp_note)
          if (any(fill_na)) {
            cur_note[fill_na] <- lkp_note[fill_na]
            data.table::set(df_joined, j = "annotation_note", value = cur_note)
          }
        }
      } else {
        lkp_ren <- annotation_notes_lookup
        if (
          "annotation_note" %in%
            names(df_joined) &&
            "annotation_note" %in% names(lkp_ren)
        ) {
          names(lkp_ren)[
            names(lkp_ren) == "annotation_note"
          ] <- ".annotation_note_lkp"
          df_joined <- df_joined |>
            tidytable::left_join(y = lkp_ren, by = note_join_cols)
          df_joined[["annotation_note"]] <- tidytable::coalesce(
            df_joined[["annotation_note"]],
            df_joined[[".annotation_note_lkp"]]
          )
          df_joined[[".annotation_note_lkp"]] <- NULL
        } else {
          df_joined <- df_joined |>
            tidytable::left_join(
              y = annotation_notes_lookup,
              by = note_join_cols
            )
        }
      }
    }
  }

  rm(df, organism_lookup)

  # Remove ties if requested
  if (remove_ties) {
    log_info("Removing ties")
    if (
      nrow(df_joined) > 1L &&
        all(c("feature_id", "rank_final") %in% names(df_joined))
    ) {
      f_rt <- df_joined$feature_id
      r_rt <- df_joined$rank_final
      u_f_rt <- unique(f_rt)
      u_r_rt <- unique(r_rt)
      f_id_rt <- if (anyNA(u_f_rt)) {
        match(f_rt, u_f_rt)
      } else {
        fastmatch::fmatch(f_rt, u_f_rt)
      }
      r_id_rt <- if (anyNA(u_r_rt)) {
        match(r_rt, u_r_rt)
      } else {
        fastmatch::fmatch(r_rt, u_r_rt)
      }
      nr_rt <- max(1L, length(u_r_rt))
      key_rt <- if (as.double(length(u_f_rt)) * nr_rt < .Machine$integer.max) {
        (f_id_rt - 1L) * nr_rt + r_id_rt
      } else {
        as.double(f_id_rt - 1L) * nr_rt + r_id_rt
      }
      dup_rt <- duplicated(key_rt)
      if (any(dup_rt)) {
        df_joined <- df_joined[!dup_rt, , drop = FALSE]
      }
    } else {
      df_joined <- df_joined |>
        tidytable::distinct(feature_id, rank_final, .keep_all = TRUE)
    }
  }

  # Summarize if requested
  if (summarize) {
    if (nrow(df_joined) == 0L) {
      df_final <- df_joined
    } else {
      rank_num <- suppressWarnings(as.integer(df_joined$rank_final))

      score_final_num <- if ("score_final" %in% names(df_joined)) {
        -.as_numeric_dedup(df_joined$score_final)
      } else if ("score_weighted_chemo" %in% names(df_joined)) {
        -.as_numeric_dedup(df_joined$score_weighted_chemo)
      } else {
        rep_len(0, nrow(df_joined))
      }

      initial_num <- if ("score_initial" %in% names(df_joined)) {
        -.as_numeric_dedup(df_joined$score_initial)
      } else if ("candidate_score_pseudo_initial" %in% names(df_joined)) {
        -.as_numeric_dedup(df_joined$candidate_score_pseudo_initial)
      } else {
        rep_len(0, nrow(df_joined))
      }

      fids_sum <- df_joined$feature_id
      u_fids_sum <- sort(unique(fids_sum), na.last = TRUE)
      fid_id_sum <- if (anyNA(u_fids_sum)) {
        match(fids_sum, u_fids_sum)
      } else {
        fastmatch::fmatch(fids_sum, u_fids_sum)
      }

      ord <- order(
        fid_id_sum,
        rank_num,
        score_final_num,
        initial_num,
        na.last = TRUE,
        method = "radix"
      )
      keep <- !duplicated(fid_id_sum[ord])
      df_final <- df_joined[ord[keep], , drop = FALSE]

      rm(
        ord,
        keep,
        rank_num,
        score_final_num,
        initial_num,
        fids_sum,
        u_fids_sum,
        fid_id_sum
      )
    }

    rm(df_joined)
  } else {
    df_final <- df_joined
    rm(df_joined)
  }

  col_types <- vapply(
    df_final,
    function(col) {
      if (is.character(col)) {
        1L
      } else if (is.factor(col)) {
        2L
      } else {
        0L
      }
    },
    integer(1L)
  )
  char_cols <- names(col_types)[col_types == 1L]
  factor_cols <- names(col_types)[col_types == 2L]

  # OPTIMIZATION: Vectorized column trimming in-place using unique-value
  # whitespace detection instead of base R trimws() across millions of rows.
  trim_col_fast <- function(val) {
    u_orig <- unique(val)
    non_na_u <- !is.na(u_orig)
    if (!any(non_na_u)) {
      return(val)
    }
    u_sub <- u_orig[non_na_u]
    has_ws <- startsWith(u_sub, " ") |
      startsWith(u_sub, "\t") |
      startsWith(u_sub, "\r") |
      startsWith(u_sub, "\n") |
      endsWith(u_sub, " ") |
      endsWith(u_sub, "\t") |
      endsWith(u_sub, "\r") |
      endsWith(u_sub, "\n")
    has_empty <- !nzchar(u_sub)
    if (!any(has_ws) && !any(has_empty)) {
      return(val)
    }
    if (any(has_ws)) {
      u_sub[has_ws] <- trimws(u_sub[has_ws])
    }
    u_sub[!nzchar(u_sub)] <- NA_character_
    u_new <- u_orig
    u_new[non_na_u] <- u_sub
    u_new[fastmatch::fmatch(val, u_orig)]
  }

  for (col in char_cols) {
    data.table::set(df_final, j = col, value = trim_col_fast(df_final[[col]]))
  }
  for (col in factor_cols) {
    data.table::set(
      df_final,
      j = col,
      value = trim_col_fast(as.character(df_final[[col]]))
    )
  }

  keep_p_cols <- intersect(final_select_cols, names(df_final))
  drop_p_cols <- setdiff(names(df_final), keep_p_cols)
  if (length(drop_p_cols) > 0L) {
    data.table::set(df_final, j = drop_p_cols, value = NULL)
  }
  if (!identical(names(df_final), keep_p_cols) && length(keep_p_cols) > 0L) {
    data.table::setcolorder(df_final, keep_p_cols)
  }
  df_processed <- df_final

  rm(df_final)

  # Handle features without candidate structures
  # Optimization: Do this in one pass with bind_rows
  has_structure <- !is.na(
    df_processed$candidate_structure_inchikey_connectivity_layer
  )

  if (all(has_structure)) {
    results <- df_processed
  } else {
    results_with_structure <- df_processed[has_structure, , drop = FALSE]
    # Without the weighted table, `df` (its ranked candidates, same columns)
    # is the consensus source
    feature_consensus_table <- feature_consensus_table %||%
      .build_feature_consensus_table(
        annot_table_wei_chemo = annot_table_wei_chemo %||% df_processed,
        model = model
      )
    no_str_idx <- which(!has_structure)
    feat_cols_no_str <- intersect(model$features_columns, names(df_processed))
    no_str_sub <- tidytable::as_tidytable(
      lapply(as.list(df_processed)[feat_cols_no_str], `[`, no_str_idx)
    )
    results_without_structure <- no_str_sub |>
      tidytable::distinct() |>
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

    results <- tidytable::bind_rows(
      results_with_structure,
      results_without_structure
    )
    rm(results_with_structure, results_without_structure)
  }
  rm(df_processed)

  if (nrow(results) > 1L && "feature_id" %in% names(results)) {
    fid_num <- .as_numeric_dedup(results$feature_id)
    ord_res <- order(fid_num, na.last = TRUE, method = "radix")
    if (is.unsorted(ord_res)) {
      results <- results[ord_res, , drop = FALSE]
    }
  }

  all_na_cols <- names(results)[vapply(
    results,
    function(col) all(is.na(col)),
    logical(1L)
  )]
  if (length(all_na_cols) > 0L) {
    results <- tidytable::as_tidytable(as.list(results)[
      setdiff(names(results), all_na_cols)
    ])
  }

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
      ann_col_val <- results[[annotation_col]]
      u_ann <- unique(ann_col_val)
      u_ann_ok <- !is.na(u_ann) & nzchar(trimws(as.character(u_ann)))
      has_ann <- if (all(u_ann_ok)) {
        rep.int(TRUE, length(ann_col_val))
      } else if (!any(u_ann_ok)) {
        rep.int(FALSE, length(ann_col_val))
      } else {
        u_ann_ok[fastmatch::fmatch(ann_col_val, u_ann)]
      }
      annotated_ids <- results[[feature_id_col]][has_ann]
      if (length(annotated_ids) > 0L) {
        annotated_ids <- unique(as.character(annotated_ids))
        if (length(total_feature_ids) > 0L) {
          annotated_features <- sum(
            fastmatch::fmatch(total_feature_ids, annotated_ids, nomatch = 0L) >
              0L
          )
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

  inchikey_col <- intersect(
    c(
      "structure_inchikey_connectivity_layer",
      "candidate_structure_inchikey_connectivity_layer"
    ),
    names(structure_organism_pairs_table)
  )[1L]
  if (is.na(inchikey_col) || length(inchikey_col) == 0L) {
    return(empty_lookup())
  }

  taxonomy_cols <- setdiff(
    grep(
      "^organism_taxonomy_",
      names(structure_organism_pairs_table),
      value = TRUE
    ),
    "organism_taxonomy_ottid"
  )
  if (length(taxonomy_cols) == 0L) {
    return(empty_lookup())
  }

  # Extract unique (inchikey, occurrence) pairs present in df using integer IDs
  ik_df <- as.character(df$candidate_structure_inchikey_connectivity_layer)
  occ_df <- as.character(df$candidate_structure_organism_occurrence_closest)
  valid_df <- which(
    !is.na(ik_df) &
      !is.na(occ_df) &
      nzchar(ik_df) &
      nzchar(occ_df)
  )
  if (length(valid_df) == 0L) {
    return(empty_lookup())
  }

  ik_sub <- ik_df[valid_df]
  occ_sub <- occ_df[valid_df]
  fid_sub <- as.character(df$feature_id[valid_df])

  u_ik <- unique(ik_sub)
  u_occ <- unique(occ_sub)
  n_occ <- length(u_occ)

  ik_sop_all <- fastmatch::fmatch(
    structure_organism_pairs_table[[inchikey_col]],
    u_ik
  )
  sop_keep <- which(!is.na(ik_sop_all))
  if (length(sop_keep) == 0L) {
    return(empty_lookup())
  }

  ik_id <- fastmatch::fmatch(ik_sub, u_ik)
  occ_id <- fastmatch::fmatch(occ_sub, u_occ)
  pair_id_df <- (as.numeric(ik_id - 1L) * n_occ) + occ_id
  u_pair_ids <- unique(pair_id_df)

  # Scan taxonomy columns against u_pair_ids without pivot_longer or table copy
  ik_sop <- ik_sop_all[sop_keep]
  doi_sop <- if ("reference_doi" %in% names(structure_organism_pairs_table)) {
    structure_organism_pairs_table$reference_doi[sop_keep]
  } else {
    rep.int(NA_character_, length(sop_keep))
  }

  matched_pair_list <- vector("list", length(taxonomy_cols))
  matched_doi_list <- vector("list", length(taxonomy_cols))

  for (idx in seq_along(taxonomy_cols)) {
    tc <- taxonomy_cols[[idx]]
    v <- structure_organism_pairs_table[[tc]][sop_keep]
    occ_m <- fastmatch::fmatch(v, u_occ)
    keep <- which(!is.na(occ_m))
    if (length(keep) > 0L) {
      pid <- (as.numeric(ik_sop[keep] - 1L) * n_occ) + occ_m[keep]
      in_df <- !is.na(fastmatch::fmatch(pid, u_pair_ids))
      if (any(in_df)) {
        k2 <- keep[in_df]
        matched_pair_list[[idx]] <- pid[in_df]
        matched_doi_list[[idx]] <- doi_sop[k2]
      }
    }
  }

  all_pids <- unlist(matched_pair_list, use.names = FALSE)
  if (length(all_pids) == 0L) {
    return(empty_lookup())
  }
  all_dois <- unlist(matched_doi_list, use.names = FALSE)

  # Collapse reference_doi ONCE per unique (inchikey, occurrence) pair,
  # BEFORE expanding across feature_id.
  u_matched_pids <- unique(all_pids)
  pid_grp <- fastmatch::fmatch(all_pids, u_matched_pids)

  u_dois <- unique(all_dois)
  doi_id <- fastmatch::fmatch(all_dois, u_dois)
  n_u_dois <- max(1L, length(u_dois))
  use_int_pd <- as.double(length(u_matched_pids)) * n_u_dois <
    .Machine$integer.max
  pd_code <- if (use_int_pd) {
    (pid_grp - 1L) * n_u_dois + doi_id
  } else {
    (as.numeric(pid_grp - 1L) * n_u_dois) + doi_id
  }
  first_pd <- if (use_int_pd) {
    !duplicated(pd_code)
  } else {
    fastmatch::fmatch(pd_code, pd_code) == seq_along(pd_code)
  }

  pid_grp_u <- pid_grp[first_pd]
  dois_u <- all_dois[first_pd]

  counts_p <- tabulate(pid_grp_u, nbins = length(u_matched_pids))
  collapsed_doi <- character(length(u_matched_pids))

  single_p <- which(counts_p == 1L)
  if (length(single_p) > 0L) {
    pos1 <- fastmatch::fmatch(single_p, pid_grp_u)
    collapsed_doi[single_p] <- ifelse(
      is.na(dois_u[pos1]),
      "",
      as.character(dois_u[pos1])
    )
  }

  multi_p <- which(counts_p > 1L)
  if (length(multi_p) > 0L) {
    in_m <- pid_grp_u %in% multi_p
    ord_m <- which(in_m)[order(pid_grp_u[in_m])]
    g_m <- pid_grp_u[ord_m]
    d_m <- dois_u[ord_m]
    non_na_m <- !is.na(d_m) & nzchar(d_m)
    if (any(non_na_m)) {
      g_m2 <- g_m[non_na_m]
      d_m2 <- d_m[non_na_m]
      sp <- split(d_m2, g_m2)
      collapsed_doi[as.integer(names(sp))] <- vapply(
        sp,
        paste,
        character(1L),
        collapse = " | ",
        USE.NAMES = FALSE
      )
    }
  }

  # Match distinct (feature_id, inchikey, occurrence) triples to u_matched_pids
  in_matched <- fastmatch::fmatch(pair_id_df, u_matched_pids)
  has_m <- which(!is.na(in_matched))
  if (length(has_m) == 0L) {
    return(empty_lookup())
  }

  f_m <- fid_sub[has_m]
  g_m <- in_matched[has_m]
  u_fids <- unique(f_m)
  f_id <- fastmatch::fmatch(f_m, u_fids)
  n_g <- max(1L, length(u_matched_pids))
  use_int_fg <- as.double(length(u_fids)) * n_g < .Machine$integer.max
  fg_code <- if (use_int_fg) {
    (f_id - 1L) * n_g + g_m
  } else {
    (as.numeric(f_id - 1L) * n_g) + g_m
  }
  first_fg <- if (use_int_fg) {
    !duplicated(fg_code)
  } else {
    fastmatch::fmatch(fg_code, fg_code) == seq_along(fg_code)
  }
  keep_fg <- has_m[first_fg]
  g_final <- in_matched[keep_fg]

  tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = ik_sub[keep_fg],
    feature_id = fid_sub[keep_fg],
    candidate_structure_organism_occurrence_closest = occ_sub[keep_fg],
    candidate_structure_organism_occurrence_reference = collapsed_doi[g_final]
  )
}

.build_feature_consensus_table <- function(annot_table_wei_chemo, model) {
  base_cols <- unique(c(
    model$features_columns,
    "feature_rt",
    "feature_mz",
    model$features_calculated_columns,
    model$components_columns,
    "annotation_note"
  ))

  nms_src <- names(annot_table_wei_chemo)
  keep_cols <- intersect(base_cols, nms_src)
  n_src <- nrow(annot_table_wei_chemo)
  if (n_src == 0L) {
    out <- as.data.frame(
      as.list(annot_table_wei_chemo)[keep_cols],
      stringsAsFactors = FALSE
    )
    if (!"annotation_note" %in% names(out)) {
      out$annotation_note <- character(0L)
    }
    return(out)
  }

  note_present <- rep.int(FALSE, n_src)
  if ("annotation_note" %in% nms_src) {
    an <- annot_table_wei_chemo$annotation_note
    non_na_an <- which(!is.na(an))
    if (length(non_na_an) > 0L) {
      note_present[non_na_an] <- nzchar(trimws(as.character(an[non_na_an])))
    }
  }

  score_num <- if ("score_weighted_chemo" %in% nms_src) {
    sc_col <- annot_table_wei_chemo$score_weighted_chemo
    if (is.double(sc_col)) {
      sc_col
    } else {
      suppressWarnings(.as_numeric_dedup(sc_col))
    }
  } else {
    rep.int(NA_real_, n_src)
  }

  rank_num <- if ("rank_final" %in% nms_src) {
    rk_col <- annot_table_wei_chemo$rank_final
    if (is.numeric(rk_col)) {
      rk_col
    } else {
      suppressWarnings(.as_numeric_dedup(rk_col))
    }
  } else {
    rep.int(NA_real_, n_src)
  }

  fids <- annot_table_wei_chemo$feature_id
  u_fids <- sort(unique(fids), na.last = TRUE)
  fid_id <- if (anyNA(u_fids)) {
    match(fids, u_fids)
  } else {
    fastmatch::fmatch(fids, u_fids)
  }

  ord_keys <- list(fid_id)
  if (any(note_present)) {
    ord_keys[[length(ord_keys) + 1L]] <- -as.integer(note_present)
  }
  if (!all(is.na(score_num))) {
    score_present <- !is.na(score_num)
    if (!all(score_present)) {
      ord_keys[[length(ord_keys) + 1L]] <- -as.integer(score_present)
    }
    ord_keys[[length(ord_keys) + 1L]] <- -score_num
  }
  if (!all(is.na(rank_num))) {
    ord_keys[[length(ord_keys) + 1L]] <- rank_num
  }

  if (length(ord_keys) == 1L) {
    best_idx <- integer(length(u_fids))
    first_u <- which(!duplicated(fid_id))
    best_idx[fid_id[first_u]] <- first_u
  } else {
    ord <- do.call(order, c(ord_keys, list(na.last = TRUE, method = "radix")))
    best_idx <- ord[!duplicated(fid_id[ord])]
  }

  out_list <- lapply(as.list(annot_table_wei_chemo)[keep_cols], `[`, best_idx)
  if (!"annotation_note" %in% names(out_list)) {
    out_list[["annotation_note"]] <- rep.int(NA_character_, length(best_idx))
  }
  attr(out_list, "row.names") <- .set_row_names(length(best_idx))
  class(out_list) <- "data.frame"
  out_list
}
