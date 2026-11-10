#' @title Clean Chemical Annotations
#'
#' @description Cleans and filters chemically weighted annotation results
#'     through a multi-tier pipeline. Applies MS1 score thresholds, percentile
#'     filtering, ranking, and optional high-evidence filtering. Ranking is
#'     score-based, and optional cluster-consensus promotion can retain
#'     plausible siblings when they are the best coherent choices. Returns
#'     three-tier output: full (comprehensive), filtered (top candidates), and
#'     mini (one row per feature).
#'
#' @include add_xrefs_to_annotations.R
#' @include calculate_mass_of_m.R
#' @include constants.R
#' @include filter_high_evidence_only.R
#' @include parse_adduct.R
#' @include summarize_results.R
#' @include logs_utils.R
#' @include errors_utils.R
#' @include columns_utils.R
#' @include clean_chemo_preprocessing.R
#' @include clean_chemo_candidates.R
#'
#' @param annot_table_wei_chemo Data frame with chemically weighted annotations.
#' Required columns: feature_id,
#'     candidate_structure_inchikey_connectivity_layer,
#' score_weighted_chemo, score_biological, score_chemical,
#'     candidate_score_pseudo_initial
#' @param components_table Data frame with molecular network component
#'     assignments.
#'     Required columns: feature_id, component_id
#' @param features_table Data frame with feature metadata (RT, m/z, etc.).
#'     Required columns: feature_id
#' @param structure_organism_pairs_table Data frame linking structures to
#'     organisms.
#'     Required columns: structure_inchikey_connectivity_layer
#' @param candidates_final Integer, number of top candidates to retain per
#'     feature (>= 1)
#' @param best_percentile Numeric (0-1), percentile threshold for score
#'     filtering.
#' Candidates with scores >= percentile * max_score are kept. Default: 0.9 (90th
#'     percentile)
#' @param minimal_ms1_bio Numeric (0-1), minimum biological score for MS1-only
#'     annotations
#' @param minimal_ms1_chemo Numeric (0-1), minimum chemical score for MS1-only
#'     annotations
#' @param minimal_ms1_condition Character, logical operator for MS1 filtering:
#'     "OR" or "AND".
#'     "OR" = keep if bio >= threshold OR chem >= threshold.
#'     "AND" = keep if bio >= threshold AND chem >= threshold
#' @param compounds_names Logical, include compound names in output (may
#'     increase size)
#' @param high_evidence Logical, also apply the high-evidence filter
#'     ([filter_high_evidence_only()]) to the full tier. The filtered and mini
#'     tiers always use it, whatever this value.
#' @param remove_ties Logical, remove tied scores (keep only highest-ranked)
#' @param summarize Logical, collapse results to one row per feature
#' @param max_per_score Integer, max candidates to keep per feature per score.
#'   If more exist, they are randomly sampled and a note is added. Default 7.
#' @param score_chemical_cla_kingdom Numeric (0-1), score for ClassyFire kingdom
#'     level
#' @param score_chemical_cla_superclass Numeric (0-1), score for ClassyFire
#'     superclass level
#' @param score_chemical_cla_class Numeric (0-1), score for ClassyFire class
#'     level
#' @param score_chemical_cla_parent Numeric (0-1), score for ClassyFire direct
#'     parent level
#' @param score_chemical_npc_pathway Numeric (0-1), score for NPClassifier
#'     pathway level
#' @param score_chemical_npc_superclass Numeric (0-1), score for NPClassifier
#'     superclass level
#' @param score_chemical_npc_class Numeric (0-1), score for NPClassifier class
#'     level
#' @param xrefs_table Optional data frame with columns inchikey/prefix/id from
#'   get_compounds_xrefs(), used to add candidate_structure_id_* columns before
#'   summarization.
#' @param enforce_cluster_consensus Logical, apply cluster-consensus promotion
#'   before candidate sampling and percentile filtering. Default TRUE.
#'
#' @return Named list with three data frames:
#'   \describe{
#'     \item{full}{All annotations (optionally high-evidence filtered)}
#'     \item{filtered}{Top candidates meeting percentile + rank thresholds}
#'     \item{mini}{One row per feature with best compound/taxonomy}
#'   }
#'
#' @seealso \code{\link{weight_chemo}},
#'     \code{\link{filter_high_evidence_only}},
#'     \code{\link{summarize_results}}
#'
#' @examples
#' \dontrun{
#' results <- clean_chemo(
#'   annot_table_wei_chemo = annotations,
#'   features_table = features,
#'   components_table = components,
#'   structure_organism_pairs_table = sop_table,
#'   candidates_final = 10,
#'   best_percentile = 0.9,
#'   minimal_ms1_bio = 0.5,
#'   minimal_ms1_chemo = 0.5,
#'   minimal_ms1_condition = "OR",
#'   compounds_names = TRUE,
#'   high_evidence = FALSE,
#'   remove_ties = FALSE,
#'   summarize = FALSE
#' )
#' }
clean_chemo <- function(
  annot_table_wei_chemo,
  components_table,
  features_table,
  structure_organism_pairs_table,
  candidates_final,
  best_percentile,
  minimal_ms1_bio,
  minimal_ms1_chemo,
  minimal_ms1_condition,
  compounds_names,
  high_evidence,
  remove_ties,
  summarize,
  # Explicit taxonomy weights (0-1)
  score_chemical_cla_kingdom = 0.2,
  score_chemical_cla_superclass = 0.4,
  score_chemical_cla_class = 0.6,
  score_chemical_cla_parent = 0.8,
  score_chemical_npc_pathway = 0.25,
  score_chemical_npc_superclass = 0.5,
  score_chemical_npc_class = 0.75,
  max_per_score = 7L,
  xrefs_table = NULL,
  enforce_cluster_consensus = TRUE
) {
  ctx <- log_operation(
    "clean_chemo",
    n_annotations = nrow(annot_table_wei_chemo),
    candidates_final = candidates_final,
    high_evidence = high_evidence
  )

  validate_clean_chemo_inputs(
    annot_table_wei_chemo = annot_table_wei_chemo,
    candidates_final = candidates_final,
    best_percentile = best_percentile,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    minimal_ms1_condition = minimal_ms1_condition,
    compounds_names = compounds_names,
    high_evidence = high_evidence,
    remove_ties = remove_ties,
    summarize = summarize,
    max_per_score = max_per_score,
    score_chemical_cla_kingdom = score_chemical_cla_kingdom,
    score_chemical_cla_superclass = score_chemical_cla_superclass,
    score_chemical_cla_class = score_chemical_cla_class,
    score_chemical_cla_parent = score_chemical_cla_parent,
    score_chemical_npc_pathway = score_chemical_npc_pathway,
    score_chemical_npc_superclass = score_chemical_npc_superclass,
    score_chemical_npc_class = score_chemical_npc_class
  )

  # Early exit for empty input
  if (nrow(annot_table_wei_chemo) == 0L) {
    log_complete(ctx, n_final = 0, note = "Empty annotation table")
    return(annot_table_wei_chemo)
  }

  # Validate features and components schema minimally
  .validate_features_dataframe(features_table)
  .validate_features_dataframe(components_table)
  if (!is.data.frame(structure_organism_pairs_table)) {
    tima_abort(
      problem = "structure_organism_pairs_table must be a data frame",
      class = c("tima_validation_error", "tima_error")
    )
  }

  annot_table_wei_chemo <- select_clean_chemo_working_columns(
    annot_table_wei_chemo
  )
  annot_table_wei_chemo <- coerce_score_columns(annot_table_wei_chemo)

  # Ensure feature-level `rt` and `mz` are present on every candidate row:
  # `sample_candidates_per_group` uses them for the cross-feature M + RT
  # anchor. If upstream scoring dropped them, attach from features_table.
  .missing_feature_cols <- setdiff(
    c("rt", "mz"),
    names(annot_table_wei_chemo)
  )
  .missing_from_ft <- intersect(.missing_feature_cols, names(features_table))
  if (
    length(.missing_from_ft) > 0L && "feature_id" %in% names(features_table)
  ) {
    .ft_fids <- features_table$feature_id
    .m_ft <- fastmatch::fmatch(annot_table_wei_chemo$feature_id, .ft_fids)
    for (.col in .missing_from_ft) {
      data.table::set(
        annot_table_wei_chemo,
        j = .col,
        value = features_table[[.col]][.m_ft]
      )
    }
  }

  log_metadata(
    ctx,
    phase = "filtering",
    best_percentile = best_percentile,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    max_per_score = max_per_score
  )

  candidate_tables <- prepare_ranked_candidates(
    annot_table_wei_chemo = annot_table_wei_chemo,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    minimal_ms1_condition = minimal_ms1_condition,
    best_percentile = best_percentile,
    max_per_score = max_per_score,
    components_table = components_table,
    enforce_cluster_consensus = enforce_cluster_consensus
  )

  df_ranked <- candidate_tables$df_ranked
  df_percentile <- candidate_tables$df_percentile
  results_candidates <- candidate_tables$results_candidates
  annotation_notes_lookup <- candidate_tables$annotation_notes_lookup

  if (candidate_tables$n_sampled_features > 0L) {
    log_info(
      "Sampling candidates for %d features with more than %d candidates per score",
      candidate_tables$n_sampled_features,
      max_per_score
    )
  }
  rm(candidate_tables)

  model <- columns_model()
  feature_consensus_table <- .build_feature_consensus_table(
    annot_table_wei_chemo = annot_table_wei_chemo,
    model = model
  )

  # The filtered tier (and the mini tier built from it) is always restricted
  # to high-evidence candidates; `high_evidence` only governs the full tier
  df_filtered <- df_percentile |>
    filter_high_evidence_only(context = "filtered") |>
    .slice_top_n_by_feature(n_keep = candidates_final)

  df_filtered <- retain_promoted_rank1_in_filtered(
    df_filtered = df_filtered,
    annotation_notes_lookup = annotation_notes_lookup
  )
  if (enforce_cluster_consensus) {
    df_filtered <- drop_cross_rt_duplicate_final_candidates(df_filtered)
  }

  if (!is.null(xrefs_table) && nrow(xrefs_table) > 0L) {
    df_filtered <- .add_xrefs_to_df(df_filtered, xrefs_table)
  }

  results_filtered <- df_filtered |>
    summarize_results(
      features_table = features_table,
      components_table = components_table,
      structure_organism_pairs_table = structure_organism_pairs_table,
      feature_consensus_table = feature_consensus_table,
      remove_ties = remove_ties,
      summarize = summarize,
      annotation_notes_lookup = annotation_notes_lookup
    ) |>
    .attach_candidate_counts(results_candidates = results_candidates) |>
    .restore_annotation_notes(annotation_notes_lookup = annotation_notes_lookup)

  # Tier 1: MINI - Extract from df_percentile BEFORE high-evidence filter
  # This ensures we get predicted taxonomy for features without inchikey

  df_classes_mini <- build_mini_taxonomy_table(
    df_percentile = df_percentile,
    score_chemical_cla_kingdom = score_chemical_cla_kingdom,
    score_chemical_cla_superclass = score_chemical_cla_superclass,
    score_chemical_cla_class = score_chemical_cla_class,
    score_chemical_cla_parent = score_chemical_cla_parent,
    score_chemical_npc_pathway = score_chemical_npc_pathway,
    score_chemical_npc_superclass = score_chemical_npc_superclass,
    score_chemical_npc_class = score_chemical_npc_class
  )

  # Combine mini results and adjust candidates_best based on inchikey presence
  results_mini <- build_mini_results_table(
    features_table = features_table,
    df_classes_mini = df_classes_mini,
    results_filtered = results_filtered,
    xrefs_table = xrefs_table
  )

  rm(df_classes_mini, df_filtered, df_percentile)

  # Tier 3: FULL - Optionally apply high-evidence filter
  df_full <- if (high_evidence) {
    df_ranked |>
      filter_high_evidence_only(context = "full")
  } else {
    df_ranked
  }

  if (!is.null(xrefs_table) && nrow(xrefs_table) > 0L) {
    df_full <- .add_xrefs_to_df(df_full, xrefs_table)
  }

  results_full <- df_full |>
    summarize_results(
      features_table = features_table,
      components_table = components_table,
      structure_organism_pairs_table = structure_organism_pairs_table,
      feature_consensus_table = feature_consensus_table,
      remove_ties = remove_ties,
      summarize = summarize,
      annotation_notes_lookup = annotation_notes_lookup
    ) |>
    .attach_candidate_counts(results_candidates = results_candidates) |>
    .restore_annotation_notes(annotation_notes_lookup = annotation_notes_lookup)
  rm(annot_table_wei_chemo, annotation_notes_lookup, results_candidates)

  rm(df_full, df_ranked, feature_consensus_table)

  results_list <- list(
    full = results_full,
    filtered = results_filtered,
    mini = results_mini
  )

  results_list <- remove_compound_names(results_list, compounds_names)

  log_complete(
    ctx,
    n_final_full = nrow(results_full),
    n_final_filtered = nrow(results_filtered),
    n_final_mini = nrow(results_mini),
    n_features = length(unique(results_filtered$feature_id))
  )

  # Clean up intermediate objects
  rm(
    features_table,
    components_table,
    structure_organism_pairs_table
  )

  results_list
}

.slice_top_n_by_feature <- function(df, n_keep) {
  n <- nrow(df)
  if (n == 0L) {
    return(df)
  }
  ord1 <- order(df$rank_final, na.last = TRUE, method = "radix")
  fids_ord1 <- df$feature_id[ord1]
  u_fids <- unique(fids_ord1)
  fid_id <- fastmatch::fmatch(fids_ord1, u_fids)
  ord2 <- order(fid_id, method = "radix")
  g_sorted <- fid_id[ord2]
  new_g <- if (n > 1L) c(TRUE, g_sorted[-1L] != g_sorted[-n]) else TRUE
  pos_in_g <- seq_len(n) - cummax(ifelse(new_g, seq_len(n), 1L)) + 1L
  keep_idx <- ord1[ord2[pos_in_g <= n_keep]]
  df[keep_idx, , drop = FALSE]
}

.attach_candidate_counts <- function(results, results_candidates) {
  res_list <- as.list(results)
  if (nrow(results) == 0L) {
    res_list[["candidates_evaluated"]] <- integer(0L)
    res_list[["candidates_best"]] <- integer(0L)
    return(tidytable::as_tidytable(res_list))
  }
  rc_fids <- results_candidates$feature_id
  idx <- fastmatch::fmatch(results$feature_id, rc_fids)
  c_eval <- results_candidates$candidates_evaluated[idx]
  c_best <- results_candidates$candidates_best[idx]
  if ("candidate_structure_inchikey_connectivity_layer" %in% names(results)) {
    is_na_ik <- is.na(results$candidate_structure_inchikey_connectivity_layer)
    if (any(is_na_ik)) {
      c_best[is_na_ik] <- NA_integer_
    }
  }
  res_list[["candidates_evaluated"]] <- c_eval
  res_list[["candidates_best"]] <- c_best
  tidytable::as_tidytable(res_list)
}

.restore_annotation_notes <- function(results, annotation_notes_lookup) {
  if (
    is.null(annotation_notes_lookup) ||
      nrow(annotation_notes_lookup) == 0L ||
      nrow(results) == 0L ||
      !"annotation_note" %in% names(annotation_notes_lookup)
  ) {
    return(results)
  }

  note_join_cols <- intersect(
    c(
      "feature_id",
      "candidate_adduct",
      "candidate_structure_inchikey_connectivity_layer",
      "rank_final"
    ),
    names(annotation_notes_lookup)
  )
  note_join_cols <- intersect(note_join_cols, names(results))
  if (length(note_join_cols) == 0L) {
    return(results)
  }

  notes_key_df <- as.data.frame(as.list(annotation_notes_lookup)[
    note_join_cols
  ])
  if (anyDuplicated(notes_key_df) == 0L) {
    n_res <- nrow(results)
    cand_idx <- if ("feature_id" %in% note_join_cols) {
      u_nf <- unique(annotation_notes_lookup$feature_id)
      m_nf <- fastmatch::fmatch(results$feature_id, u_nf)
      which(!is.na(m_nf))
    } else {
      seq_len(n_res)
    }

    note_col <- if ("annotation_note" %in% names(results)) {
      results$annotation_note
    } else {
      rep(NA_character_, n_res)
    }

    if (length(cand_idx) > 0L) {
      r_cols <- lapply(note_join_cols, function(col) results[[col]][cand_idx])
      n_cols <- lapply(
        note_join_cols,
        function(col) annotation_notes_lookup[[col]]
      )
      r_k <- do.call(paste, c(r_cols, list(sep = "\r")))
      n_k <- do.call(paste, c(n_cols, list(sep = "\r")))
      m_idx <- fastmatch::fmatch(r_k, n_k)
      matched_notes <- annotation_notes_lookup$annotation_note[m_idx]
      cur_sub <- note_col[cand_idx]
      fill_mask <- is.na(cur_sub) & !is.na(matched_notes)
      if (any(fill_mask)) {
        note_col[cand_idx[fill_mask]] <- matched_notes[fill_mask]
      }
    }
    res_list <- as.list(results)
    res_list[["annotation_note"]] <- note_col
    return(tidytable::as_tidytable(res_list))
  }

  notes <- annotation_notes_lookup |>
    tidytable::select(
      tidyselect::any_of(c(note_join_cols, "annotation_note"))
    ) |>
    tidytable::rename(annotation_note_lookup = annotation_note)

  joined <- results |>
    tidytable::left_join(notes, by = note_join_cols)

  if ("annotation_note" %in% names(joined)) {
    joined <- joined |>
      tidytable::mutate(
        annotation_note = tidytable::coalesce(
          annotation_note,
          annotation_note_lookup
        )
      )
  } else {
    joined <- joined |>
      tidytable::mutate(annotation_note = annotation_note_lookup)
  }

  joined |>
    tidytable::select(-tidyselect::any_of("annotation_note_lookup"))
}

#' Keep only rank-1 rows in filtered tier when cluster consensus promoted rank 1
#' @keywords internal
retain_promoted_rank1_in_filtered <- function(
  df_filtered,
  annotation_notes_lookup
) {
  if (
    nrow(df_filtered) == 0L ||
      is.null(annotation_notes_lookup) ||
      nrow(annotation_notes_lookup) == 0L ||
      !all(c("feature_id", "rank_final") %in% names(df_filtered)) ||
      !"annotation_note" %in% names(annotation_notes_lookup)
  ) {
    return(df_filtered)
  }

  notes_vec <- annotation_notes_lookup$annotation_note
  is_prom <- !is.na(notes_vec) & startsWith(notes_vec, "Promoted to rank 1:")
  if (!any(is_prom)) {
    return(df_filtered)
  }

  promoted_ids <- unique(annotation_notes_lookup$feature_id[is_prom])
  promoted_ids <- promoted_ids[!is.na(promoted_ids)]
  if (length(promoted_ids) == 0L) {
    return(df_filtered)
  }

  before_n <- nrow(df_filtered)
  in_prom <- !is.na(fastmatch::fmatch(df_filtered$feature_id, promoted_ids))
  keep <- !in_prom | df_filtered$rank_final == 1L
  out <- df_filtered[keep, , drop = FALSE]

  dropped_n <- before_n - nrow(out)
  if (dropped_n > 0L) {
    log_info(
      "Filtered-tier promotion gate removed %d non-rank1 row(s) for promoted features",
      dropped_n
    )
  }

  out
}
