#' @title Clean bio
#'
#' @description This function cleans and filters biologically weighted
#'     annotation results by calculating chemical consistency scores across
#'     network neighbors. Only features with at least 2 neighbors are evaluated.
#'     Internal helper for weight_annotations().
#'
#' @include validations_utils.R
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

  # Prepare annotations for consistency scoring ----

  # Select only required columns for consistency calculation
  annotations_for_join <- annotations_distinct |>
    tidytable::select(
      feature_id,
      candidate_structure_tax_cla_01kin,
      candidate_structure_tax_npc_01pat,
      candidate_structure_tax_cla_02sup,
      candidate_structure_tax_npc_02sup,
      candidate_structure_tax_cla_03cla,
      candidate_structure_tax_npc_03cla,
      candidate_structure_tax_cla_04dirpar,
      score_weighted_bio
    ) |>
    tidytable::distinct()

  # Calculate Consistency Scores ----

  # Pass edges and annotations separately to avoid materialising a huge
  # (edges × all_annotations) join that causes OOM on memory-limited runners.
  consistency_results <- .calculate_consistency_all_levels(
    edges_filtered = edges_filtered,
    annotations_for_join = annotations_for_join,
    minimal_consistency = minimal_consistency
  )

  rm(annotations_for_join, edges_filtered)

  if ("feature_pred_tax_cla_02sup_val" %in% colnames(annotations_distinct)) {
    df1 <- annotations_distinct |>
      tidytable::filter(!is.na(feature_pred_tax_cla_02sup_val))

    df1b <- df1 |>
      tidytable::select(-tidyselect::contains(match = "feature_pred_tax"))

    # Get feature_ids that have predictions to exclude from the main set
    # Using anti_join is more efficient than %in% for large vectors
    df2 <- annotations_distinct |>
      tidytable::select(
        -tidyselect::contains(match = "feature_pred_tax")
      ) |>
      tidytable::anti_join(
        df1b |> tidytable::distinct(feature_id),
        by = "feature_id"
      ) |>
      tidytable::bind_rows(df1b)
  } else {
    df1 <- tidytable::tidytable()
    df1b <- tidytable::tidytable()
    df2 <- annotations_distinct
  }
  rm(annotations_distinct)

  supp_tables <- consistency_results

  # Memory-efficient approach: merge all consistency results at once
  # instead of sequential purrr::reduce operations
  annot_table_wei_bio_preclean <- .merge_consistency_tables(
    base_df = df2,
    consistency_list = supp_tables
  )

  rm(df2, supp_tables)

  annot_table_wei_bio_preclean <- .add_default_prediction_columns(
    annot_table_wei_bio_preclean
  )

  if (nrow(df1b) == 0L) {
    return(annot_table_wei_bio_preclean)
  }

  # Replace features that should be cleaned with their precomputed versions
  annot_table_wei_bio_clean <- annot_table_wei_bio_preclean |>
    tidytable::anti_join(
      df1b |> tidytable::distinct(feature_id),
      by = "feature_id"
    ) |>
    tidytable::bind_rows(df1)

  rm(
    annot_table_wei_bio_preclean,
    df1,
    df1b
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

  # Use Reduce with base::identity to minimize intermediate allocations
  result <- Reduce(
    function(left_df, right_df) {
      tidytable::left_join(
        left_df,
        right_df,
        by = stats::setNames(object = "feature_source", nm = "feature_id")
      )
    },
    consistency_list,
    init = base_df
  )

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

  # Identify missing and existing columns
  missing_cols <- setdiff(names(defaults), names(df))
  existing_cols <- intersect(names(defaults), names(df))

  # Add missing columns
  if (length(missing_cols) > 0L) {
    n_rows <- nrow(df)
    for (col in missing_cols) {
      df[[col]] <- rep(defaults[[col]], n_rows)
    }
  }

  # Fill NAs in existing columns
  for (col in existing_cols) {
    na_idx <- is.na(df[[col]])
    if (any(na_idx)) {
      df[[col]][na_idx] <- defaults[[col]]
    }
  }

  tidytable::as_tidytable(df)
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
#' TODO: Implement more sophisticated filtering criteria:
#' - Configurable entropy thresholds for spectral quality
#' - Similarity score minimums
#' - Consider edge filtering during edge creation step
.filter_edges_for_consistency <- function(
  edges_table,
  min_neighbors = 2L,
  min_entropy = 0
) {
  # Remove self-loops
  edges_filtered <- edges_table |>
    tidytable::filter(feature_source != feature_target)

  # Filter by entropy or label presence
  if ("label" %in% colnames(edges_filtered)) {
    edges_filtered <- edges_filtered |>
      tidytable::filter(
        feature_spectrum_entropy > min_entropy | !is.na(label)
      )
  } else {
    edges_filtered <- edges_filtered |>
      tidytable::filter(feature_spectrum_entropy > min_entropy)
  }

  # Keep only features with at least min_neighbors neighbors
  edges_filtered |>
    tidytable::distinct(feature_source, feature_target) |>
    tidytable::group_by(feature_source) |>
    tidytable::add_count() |>
    tidytable::ungroup() |>
    tidytable::filter(n >= min_neighbors) |>
    tidytable::select(-n)
}

#' Calculate consistency per taxonomic level
#'
#' @description
#' Calculates chemical consistency scores for a single taxonomic level by
#' comparing classifications across network neighbors. Avoids materialising
#' the full (edges × all_annotations) join; instead it builds a compact
#' per-target summary for the single taxonomy column requested and then joins
#' with the edge list, keeping peak memory O(edges × distinct_taxonomies).
#'
#' @param edges_filtered Data frame with unique (feature_source, feature_target)
#'     pairs already filtered for min-neighbor requirement
#' @param annotations_for_join Data frame with (feature_id, taxonomy_cols…,
#'     score_weighted_bio) — the pool of candidate annotations
#' @param total_targets_per_source Data frame with (feature_source, n_targets)
#'     pre-computed denominator: total distinct target features per source
#' @param candidates Character name of the candidate taxonomy column
#' @param consistency_name Character name for output consistency column
#' @param feature_score_name Character name for output score column
#' @param feature_val_name Character name for output value column
#' @param minimal_consistency Numeric minimum consistency threshold (0-1)
#'
#' @return Data frame with consistency scores for this level
#'
#' @keywords internal
.calculate_consistency_per_level <- function(
  edges_filtered,
  annotations_for_join,
  total_targets_per_source,
  candidates,
  consistency_name,
  feature_score_name,
  feature_val_name,
  minimal_consistency
) {
  empty_result <- tidytable::tidytable(
    feature_source = character(0L),
    !!as.name(feature_val_name) := character(0L),
    !!as.name(consistency_name) := numeric(0L),
    !!as.name(feature_score_name) := numeric(0L)
  )

  # Step 1: compact summary — one row per (feature, taxonomy_val) with the
  # best annotation score for that pair.  This table is reused twice: once
  # to count how many neighbours support each taxonomy value (joined on
  # feature_target) and once to score the *source* feature itself (joined on
  # feature_source).  Using the source's own score instead of the neighbours'
  # max avoids the "same score for every feature" collapse that occurs when
  # neighbours share a common max score (e.g. popular Biota matches).
  taxonomy_scores <- annotations_for_join |>
    tidytable::select(feature_id, !!as.name(candidates), score_weighted_bio) |>
    tidytable::summarize(
      score_weighted_bio = max(score_weighted_bio, na.rm = TRUE),
      .by = c("feature_id", candidates)
    )

  # Step 2: join edges with compact target taxonomies
  # rows: (feature_source, feature_target, taxonomy_val, best_score)
  df_level <- edges_filtered |>
    tidytable::inner_join(
      taxonomy_scores,
      by = c("feature_target" = "feature_id")
    )

  if (nrow(df_level) == 0L) {
    return(empty_result)
  }

  # Step 3: numerator — distinct target features per (source, taxonomy_val).
  # Because edges_filtered has unique (source, target) pairs and target_taxonomy
  # is unique per (feature_id, taxonomy_val), each df_level row is already
  # distinct, so n() == n_distinct(feature_target) here.
  count_per_group <- df_level |>
    tidytable::summarize(
      count = tidytable::n(),
      .by = c("feature_source", candidates)
    )

  if (nrow(count_per_group) == 0L) {
    return(empty_result)
  }

  # Step 4: source feature's own best score for the predicted taxonomy value.
  # Joining taxonomy_scores on feature_source (not feature_target) gives each
  # source its own annotation-derived score for that taxonomy class, making
  # the final feature_pred_tax_*_score discriminative across sources.  When a
  # source has no annotation for a given taxonomy value the join yields NA,
  # which sorts last and is never selected as the top-ranked class.

  # Step 5: compute consistency and pick the top-ranked taxonomy value per source
  count_per_group |>
    tidytable::left_join(total_targets_per_source, by = "feature_source") |>
    tidytable::mutate(!!as.name(consistency_name) := count / n_targets) |>
    tidytable::left_join(
      taxonomy_scores,
      by = c("feature_source" = "feature_id", candidates)
    ) |>
    tidytable::mutate(
      !!as.name(feature_score_name) := !!as.name(consistency_name) *
        score_weighted_bio
    ) |>
    tidytable::arrange(-!!as.name(feature_score_name)) |>
    tidytable::distinct(feature_source, .keep_all = TRUE) |>
    tidytable::select(
      feature_source,
      !!as.name(feature_val_name) := !!as.name(candidates),
      !!as.name(consistency_name),
      !!as.name(feature_score_name)
    ) |>
    tidytable::mutate(
      !!as.name(feature_val_name) := tidytable::if_else(
        condition = !!as.name(feature_score_name) >= minimal_consistency,
        true = !!as.name(feature_val_name),
        false = "notConsistent"
      )
    )
}

#' Calculate consistency scores for all taxonomic levels
#'
#' @description
#' Wrapper function that calculates consistency scores across all taxonomic
#' levels (ClassyFire and NPC hierarchies). Pre-computes the denominator
#' (total distinct target features per source) once, then dispatches each
#' level to \code{.calculate_consistency_per_level}.
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
  # Pre-compute denominator once — avoids repeating this inside every level.
  # Only count target features that have at least one annotation, matching the
  # semantics of the original right_join + filter(!is.na(feature_source)).
  annotated_target_ids <- annotations_for_join |>
    tidytable::distinct(feature_id)

  total_targets_per_source <- edges_filtered |>
    tidytable::inner_join(
      annotated_target_ids,
      by = c("feature_target" = "feature_id")
    ) |>
    tidytable::summarize(
      n_targets = tidytable::n_distinct(feature_target),
      .by = "feature_source"
    )

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

  # Helper to calculate consistency for a single taxonomic level
  .process_level_consistency <- function(level) {
    .calculate_consistency_per_level(
      edges_filtered = edges_filtered,
      annotations_for_join = annotations_for_join,
      total_targets_per_source = total_targets_per_source,
      candidates = level$col,
      consistency_name = level$consistency,
      feature_score_name = level$score,
      feature_val_name = level$val,
      minimal_consistency = minimal_consistency
    )
  }

  # Calculate consistency for each level
  lapply(
    X = levels,
    FUN = .process_level_consistency
  )
}
