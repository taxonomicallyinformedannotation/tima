#' @title Clean bio
#'
#' @description This function cleans and filters biologically weighted
#'     annotation results by calculating chemical consistency scores across
#'     network neighbors. Only features with at least 2 neighbors are evaluated.
#'     Internal helper for weight_annotations().
#'
#' @include validations_utils.R
#' @include logs_utils.R
#' @include validations_params.R
#'
#' @param annot_table_wei_bio Data frame containing biologically weighted
#'     annotations
#' @param edges_table Data frame containing network edges between features
#' @param minimal_consistency Numeric minimum consistency score (0-1) required
#'     to retain a classification at each taxonomic level
#'
#' @return Data frame containing filtered biologically weighted annotations
#'     with consistency scores
#'
#' @seealso weight_bio
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal use only - called by weight_annotations()
#' cleaned <- clean_bio(
#'   annot_table_wei_bio = bio_weighted_annotations,
#'   edges_table = network_edges,
#'   minimal_consistency = 0.5
#' )
#' }
clean_bio <- function(
  annot_table_wei_bio,
  edges_table,
  minimal_consistency
) {
  ctx <- log_operation(
    "clean_bio",
    n_annotations = nrow(annot_table_wei_bio),
    minimal_consistency = minimal_consistency
  )

  # Input Validation (using centralized validators) ----

  validate_dataframe(
    annot_table_wei_bio,
    param_name = "annot_table_wei_bio",
    allow_empty = TRUE
  )

  validate_dataframe(
    edges_table,
    param_name = "edges_table",
    allow_empty = TRUE
  )

  validate_numeric_range(
    minimal_consistency,
    min_value = 0,
    max_value = 1,
    param_name = "minimal_consistency"
  )

  # Early exit for empty inputs
  if (nrow(annot_table_wei_bio) == 0L) {
    log_warn("Empty annotation table provided")
    return(annot_table_wei_bio)
  }

  if (nrow(edges_table) == 0L) {
    log_warn("Empty edges table provided, cannot calculate consistency")
    return(.add_default_prediction_columns(annot_table_wei_bio))
  }

  # Extract Distinct Structure-Taxonomy Pairs ----

  # Extract unique structure-taxonomy combinations
  annotations_distinct <- .extract_distinct_taxonomy_pairs(annot_table_wei_bio)

  # Filter Edges for Consistency Calculation ----

  edges_filtered <- .filter_edges_for_consistency(edges_table)

  log_debug(
    "Found %d valid edges for consistency calculation",
    nrow(edges_filtered)
  )

  # Early exit if no valid edges - add required columns with defaults
  if (nrow(edges_filtered) == 0L) {
    log_warn(
      "No features with >=2 neighbors found, skipping consistency calculation"
    )
    log_debug(
      "Adding default feature_pred_tax columns (all features marked as 'empty')"
    )
    return(.add_default_prediction_columns(annot_table_wei_bio))
  }

  # Calculate Consistency Scores ----

  # Pass edges and annotations separately to avoid materializing a huge
  # (edges × all_annotations) join that causes OOM on memory-limited runners.
  consistency_results <- .calculate_consistency_all_levels(
    edges_filtered = edges_filtered,
    annotations_for_join = annotations_distinct,
    minimal_consistency = minimal_consistency
  )

  rm(edges_filtered)

  has_pred_col <- "feature_pred_tax_cla_02sup_val" %in%
    colnames(annotations_distinct) &&
    !all(is.na(annotations_distinct$feature_pred_tax_cla_02sup_val))

  if (has_pred_col) {
    has_pred_row <- !is.na(annotations_distinct$feature_pred_tax_cla_02sup_val)
    df1 <- annotations_distinct[has_pred_row, ]
    pred_fids <- unique(df1$feature_id)
    keep_no_pred <- is.na(fastmatch::fmatch(
      annotations_distinct$feature_id,
      pred_fids
    ))
    df2 <- annotations_distinct[keep_no_pred, ] |>
      tidytable::select(-tidyselect::contains(match = "feature_pred_tax"))
  } else {
    df1 <- NULL
    df2 <- annotations_distinct |>
      tidytable::select(-tidyselect::contains(match = "feature_pred_tax"))
  }
  rm(annotations_distinct)

  # Memory-efficient approach: merge all consistency results at once
  # instead of sequential purrr::reduce operations
  annot_table_wei_bio_preclean <- .merge_consistency_tables(
    base_df = df2,
    consistency_list = consistency_results
  )

  rm(df2, consistency_results)

  annot_table_wei_bio_preclean <- .add_default_prediction_columns(
    annot_table_wei_bio_preclean
  )

  if (is.null(df1) || nrow(df1) == 0L) {
    return(annot_table_wei_bio_preclean)
  }

  # Replace features that should be cleaned with their precomputed versions
  annot_table_wei_bio_clean <- tidytable::bind_rows(
    annot_table_wei_bio_preclean,
    df1
  )

  rm(
    annot_table_wei_bio_preclean,
    df1
  )

  log_complete(ctx, n_cleaned = nrow(annot_table_wei_bio_clean))

  annot_table_wei_bio_clean
}

# Internal Helper Functions ----

#' Merge consistency tables efficiently for large datasets
#'
#' @description
#' Combines all consistency result tables into base dataframe using batch
#' operations to minimize memory overhead from sequential joins.
#'
#' @param base_df Data frame to which consistency data will be added
#' @param consistency_list List of consistency result data frames
#'
#' @return Merged dataframe with all consistency columns
#'
#' @keywords internal
.merge_consistency_tables <- function(base_df, consistency_list) {
  if (length(consistency_list) == 0L) {
    return(base_df)
  }

  result <- data.table::copy(tidytable::as_tidytable(base_df))
  fids <- result$feature_id
  for (right_df in consistency_list) {
    val_cols <- setdiff(names(right_df), "feature_source")
    if (nrow(right_df) == 0L) {
      for (col in val_cols) {
        data.table::set(
          result,
          j = col,
          value = rep.int(right_df[[col]][NA_integer_], length(fids))
        )
      }
    } else {
      m <- fastmatch::fmatch(fids, right_df$feature_source)
      for (col in val_cols) {
        data.table::set(result, j = col, value = right_df[[col]][m])
      }
    }
  }

  result |> tidytable::select(feature_id, tidyselect::everything())
}

#' Add or fill default prediction columns in data frame
#'
#' @description
#' Ensures all taxonomy prediction columns exist with appropriate defaults.
#' Adds missing columns and fills NA values using clean vectorized approach.
#'
#' @param df Data frame to process
#'
#' @return Data frame with all prediction columns properly initialized
#'
#' @keywords internal
.add_default_prediction_columns <- function(df) {
  defaults <- list(
    feature_pred_tax_cla_01kin_val = NA_character_,
    consistency_structure_cla_kin = 1,
    feature_pred_tax_cla_01kin_score = NA_real_,
    feature_pred_tax_npc_01pat_val = NA_character_,
    consistency_structure_npc_pat = 1,
    feature_pred_tax_npc_01pat_score = NA_real_,
    feature_pred_tax_cla_02sup_val = NA_character_,
    consistency_structure_cla_sup = 1,
    feature_pred_tax_cla_02sup_score = NA_real_,
    feature_pred_tax_npc_02sup_val = NA_character_,
    consistency_structure_npc_sup = 1,
    feature_pred_tax_npc_02sup_score = NA_real_,
    feature_pred_tax_cla_03cla_val = NA_character_,
    consistency_structure_cla_cla = 1,
    feature_pred_tax_cla_03cla_score = NA_real_,
    feature_pred_tax_npc_03cla_val = NA_character_,
    consistency_structure_npc_cla = 1,
    feature_pred_tax_npc_03cla_score = NA_real_,
    feature_pred_tax_cla_04dirpar_val = NA_character_,
    consistency_structure_cla_par = 1,
    feature_pred_tax_cla_04dirpar_score = NA_real_
  )

  out <- data.table::copy(tidytable::as_tidytable(df))
  missing_cols <- setdiff(names(defaults), names(out))
  existing_cols <- intersect(names(defaults), names(out))

  if (length(missing_cols) > 0L) {
    n_rows <- nrow(out)
    for (col in missing_cols) {
      data.table::set(out, j = col, value = rep.int(defaults[[col]], n_rows))
    }
  }

  for (col in existing_cols) {
    def_val <- defaults[[col]]
    if (!is.na(def_val)) {
      na_idx <- which(is.na(out[[col]]))
      if (length(na_idx) > 0L) {
        data.table::set(out, i = na_idx, j = col, value = def_val)
      }
    }
  }

  out
}


#' Extract distinct structure-taxonomy pairs from annotations
#'
#' @description
#' Extracts unique structure-taxonomy combinations from annotation table,
#' removing duplicates while preserving all relevant columns.
#'
#' @param annot_table Data frame containing annotations
#'
#' @return Data frame with distinct structure-taxonomy pairs
#'
#' @keywords internal
.extract_distinct_taxonomy_pairs <- function(annot_table) {
  annot_table |>
    tidytable::distinct(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      candidate_structure_tax_cla_01kin,
      candidate_structure_tax_npc_01pat,
      candidate_structure_tax_cla_02sup,
      candidate_structure_tax_npc_02sup,
      candidate_structure_tax_cla_03cla,
      candidate_structure_tax_npc_03cla,
      candidate_structure_tax_cla_04dirpar,
      score_weighted_bio,
      .keep_all = TRUE
    )
}

#' Filter edges for consistency calculation
#'
#' @description
#' Filters edges to keep only features with at least 2 neighbors and
#' removes self-loops. Optionally filters by spectral entropy and labels.
#'
#' @param edges_table Data frame containing network edges
#' @param min_neighbors Integer minimum number of neighbors required (default:
#'     2)
#' @param min_entropy Numeric minimum spectral entropy (default: 0)
#'
#' @return Filtered edges data frame
#'
#' @keywords internal
#'
#' @note
#' TODO: Potential future enhancement:
#' Consider implementing more sophisticated filtering criteria:
#' - Configurable entropy thresholds for spectral quality
#' - Similarity score minimums
#' - Evaluate edge filtering during edge creation step (performance vs. correctness trade-off)
#'
#' Current implementation uses simple fixed thresholds and works correctly,
#' but could benefit from domain-specific tuning parameters.
.filter_edges_for_consistency <- function(
  edges_table,
  min_neighbors = 2L,
  min_entropy = 0
) {
  src <- edges_table$feature_source
  tgt <- edges_table$feature_target
  not_self <- !is.na(src) & !is.na(tgt) & src != tgt

  ent_raw <- edges_table$feature_spectrum_entropy
  if (is.numeric(ent_raw)) {
    ent_ok <- !is.na(ent_raw) & ent_raw > min_entropy
  } else {
    u_ent <- unique(ent_raw)
    u_num <- suppressWarnings(as.numeric(u_ent))
    u_ok <- !is.na(u_num) & u_num > min_entropy
    ent_ok <- u_ok[fastmatch::fmatch(ent_raw, u_ent)]
  }

  if ("label" %in% colnames(edges_table)) {
    keep <- not_self & (ent_ok | !is.na(edges_table$label))
  } else {
    keep <- not_self & ent_ok
  }

  if (!any(keep)) {
    return(tidytable::tidytable(
      feature_source = src[0L],
      feature_target = tgt[0L]
    ))
  }

  src_k <- src[keep]
  tgt_k <- tgt[keep]
  u_nodes <- unique(c(unique(src_k), unique(tgt_k)))
  n_nodes <- length(u_nodes)
  s_id <- fastmatch::fmatch(src_k, u_nodes)
  t_id <- fastmatch::fmatch(tgt_k, u_nodes)

  pair_key <- (as.numeric(s_id) - 1) * n_nodes + t_id
  not_dup <- !duplicated(pair_key)

  s_dedup <- s_id[not_dup]
  counts <- tabulate(s_dedup, nbins = n_nodes)
  keep_deg <- counts[s_dedup] >= min_neighbors

  idx_final <- which(not_dup)[keep_deg]
  tidytable::tidytable(
    feature_source = src_k[idx_final],
    feature_target = tgt_k[idx_final]
  )
}

#' Calculate consistency scores for all taxonomic levels
#'
#' @description
#' Wrapper function that calculates consistency scores across all taxonomic
#' levels (ClassyFire and NPC hierarchies). Pre-computes the denominator
#' (total distinct target features per source) once, then processes all
#' levels in a single pass over the edges for maximum efficiency.
#'
#' @param edges_filtered Data frame with unique (feature_source, feature_target)
#'     pairs already filtered for min-neighbor requirement
#' @param annotations_for_join Data frame with (feature_id, taxonomy_cols…,
#'     score_weighted_bio)
#' @param minimal_consistency Numeric minimum consistency threshold (0-1)
#'
#' @return List of data frames with consistency scores for each level
#'
#' @keywords internal
.calculate_consistency_all_levels <- function(
  edges_filtered,
  annotations_for_join,
  minimal_consistency
) {
  # Define taxonomic levels to process
  levels <- list(
    list(
      col = "candidate_structure_tax_cla_01kin",
      consistency = "consistency_structure_cla_kin",
      score = "feature_pred_tax_cla_01kin_score",
      val = "feature_pred_tax_cla_01kin_val"
    ),
    list(
      col = "candidate_structure_tax_npc_01pat",
      consistency = "consistency_structure_npc_pat",
      score = "feature_pred_tax_npc_01pat_score",
      val = "feature_pred_tax_npc_01pat_val"
    ),
    list(
      col = "candidate_structure_tax_cla_02sup",
      consistency = "consistency_structure_cla_sup",
      score = "feature_pred_tax_cla_02sup_score",
      val = "feature_pred_tax_cla_02sup_val"
    ),
    list(
      col = "candidate_structure_tax_npc_02sup",
      consistency = "consistency_structure_npc_sup",
      score = "feature_pred_tax_npc_02sup_score",
      val = "feature_pred_tax_npc_02sup_val"
    ),
    list(
      col = "candidate_structure_tax_cla_03cla",
      consistency = "consistency_structure_cla_cla",
      score = "feature_pred_tax_cla_03cla_score",
      val = "feature_pred_tax_cla_03cla_val"
    ),
    list(
      col = "candidate_structure_tax_npc_03cla",
      consistency = "consistency_structure_npc_cla",
      score = "feature_pred_tax_npc_03cla_score",
      val = "feature_pred_tax_npc_03cla_val"
    ),
    list(
      col = "candidate_structure_tax_cla_04dirpar",
      consistency = "consistency_structure_cla_par",
      score = "feature_pred_tax_cla_04dirpar_score",
      val = "feature_pred_tax_cla_04dirpar_val"
    )
  )

  ann_fids_raw <- annotations_for_join$feature_id
  u_fids <- sort(unique(c(
    unique(ann_fids_raw),
    unique(edges_filtered$feature_source),
    unique(edges_filtered$feature_target)
  )))
  n_fids <- length(u_fids)

  ann_fid <- fastmatch::fmatch(ann_fids_raw, u_fids)
  edge_src <- fastmatch::fmatch(edges_filtered$feature_source, u_fids)
  edge_tgt <- fastmatch::fmatch(edges_filtered$feature_target, u_fids)

  # Only count target features that have at least one annotation, matching the
  # semantics of the original inner_join with distinct annotated feature_ids.
  is_annotated_fid <- tabulate(ann_fid, nbins = n_fids) > 0L
  keep_edge <- !is.na(edge_src) & !is.na(edge_tgt) & is_annotated_fid[edge_tgt]
  edge_src_ann <- edge_src[keep_edge]
  edge_tgt_ann <- edge_tgt[keep_edge]

  # Since edges_filtered has distinct (feature_source, feature_target) pairs,
  # n_targets per source is simply tabulate(edge_src_ann)
  n_targets_by_fid <- tabulate(edge_src_ann, nbins = n_fids)

  edges_int_dt <- data.table::data.table(
    src_id = edge_src_ann,
    tgt_id = edge_tgt_ann
  )

  # Pre-sort annotations by decreasing score_weighted_bio so first occurrence
  # per (fid, tax_id) has max(score_weighted_bio, na.rm = TRUE) in O(N)
  scores_raw <- annotations_for_join$score_weighted_bio
  ord_score <- order(scores_raw, decreasing = TRUE, na.last = TRUE)
  ann_fid_sorted <- ann_fid[ord_score]
  scores_sorted <- scores_raw[ord_score]
  has_na_scores <- anyNA(scores_sorted)

  lapply(levels, function(lvl) {
    empty_result <- tidytable::tidytable(
      feature_source = u_fids[0L],
      !!as.name(lvl$val) := character(0L),
      !!as.name(lvl$consistency) := numeric(0L),
      !!as.name(lvl$score) := numeric(0L)
    )

    if (length(edge_src_ann) == 0L) {
      return(empty_result)
    }

    col_vals <- annotations_for_join[[lvl$col]][ord_score]
    valid_tax <- !is.na(col_vals)
    if (!any(valid_tax)) {
      return(empty_result)
    }

    vals_v <- col_vals[valid_tax]
    fid_v <- ann_fid_sorted[valid_tax]
    sc_v <- scores_sorted[valid_tax]

    u_tax <- sort(unique(vals_v))
    n_tax <- length(u_tax)
    tax_v <- fastmatch::fmatch(vals_v, u_tax)

    pair_key <- (as.numeric(fid_v) - 1) * n_tax + tax_v
    first_pair <- !duplicated(pair_key)

    u_fid_pair <- fid_v[first_pair]
    u_tax_pair <- tax_v[first_pair]
    u_sc_pair <- sc_v[first_pair]
    u_pair_keys <- pair_key[first_pair]

    if (has_na_scores && anyNA(u_sc_pair)) {
      u_sc_pair[is.na(u_sc_pair)] <- -Inf
    }

    target_dt <- data.table::data.table(
      tgt_id = u_fid_pair,
      tax_id = u_tax_pair
    )

    joined_dt <- edges_int_dt[
      target_dt,
      on = "tgt_id",
      nomatch = NULL,
      allow.cartesian = TRUE
    ]
    if (nrow(joined_dt) == 0L) {
      return(empty_result)
    }

    # keyby sorts by (src_id, tax_id), matching tidytable::summarize(.by = ...)
    count_dt <- joined_dt[, list(count = .N), keyby = c("src_id", "tax_id")]
    if (nrow(count_dt) == 0L) {
      return(empty_result)
    }

    c_src <- count_dt$src_id
    c_tax <- count_dt$tax_id
    c_cnt <- count_dt$count

    cons_vec <- c_cnt / n_targets_by_fid[c_src]
    c_pair_key <- (as.numeric(c_src) - 1) * n_tax + c_tax
    src_sc <- u_sc_pair[fastmatch::fmatch(c_pair_key, u_pair_keys)]
    score_vec <- cons_vec * src_sc

    ord_res <- order(-score_vec)
    keep_top <- ord_res[!duplicated(c_src[ord_res])]

    top_src <- c_src[keep_top]
    top_tax <- u_tax[c_tax[keep_top]]
    top_cons <- cons_vec[keep_top]
    top_sc <- score_vec[keep_top]

    top_val <- ifelse(
      !is.na(top_sc) & top_sc >= minimal_consistency,
      top_tax,
      ifelse(is.na(top_sc), NA_character_, "notConsistent")
    )

    out <- tidytable::tidytable(
      feature_source = u_fids[top_src],
      v_col = top_val,
      c_col = top_cons,
      s_col = top_sc
    )
    data.table::setnames(
      out,
      c("v_col", "c_col", "s_col"),
      c(lvl$val, lvl$consistency, lvl$score)
    )
    out
  })
}
